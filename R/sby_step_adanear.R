#' Balancear dados em uma recipe com NearMiss-1
#'
#' @details
#' A interface KNN foi unificada em tres parametros publicos: `sby_knn_engine`,
#' `sby_knn_algorithm` e `sby_distance_metric`. O argumento
#' `sby_knn_algorithm` concentra tanto as estrategias exatas do FNN quanto os
#' algoritmos antes associados ao BiocNeighbors. A etapa de padronizacao por
#' Z-score continua sendo executada antes da busca; quando `sby_distance_metric`
#' e `"ip"` ou `"cosine"`, as matrizes de referencia e consulta recebem tambem
#' normalizacao L2 interna obrigatoria antes da submissao ao engine KNN.
#'
#' Compatibilidade entre engines, algoritmos e metricas:
#'
#' | Engine | Algoritmos em `sby_knn_algorithm` | Tipo de busca | Metricas suportadas |
#' |---|---|---|---|
#' | FNN | `auto`, `kd_tree`, `cover_tree`, `brute` | Busca exata | `euclidean` |
#' | BiocNeighbors | `Kmknn`, `Vptree`, `Exhaustive` | Busca exata | `euclidean`, `cosine` |
#' | BiocNeighbors | `Annoy`, `Hnsw` | Busca aproximada | `euclidean`, `cosine` |
#' | RcppHNSW | gerido internamente | Busca aproximada | `euclidean`, `cosine`, `ip` |
#'
#' Metricas disponiveis:
#'
#' * `euclidean`: distancia em linha reta no espaco continuo e padrao universal
#'   para KNN, definida por \eqn{d(x, y) = \sqrt{\sum (x_i - y_i)^2}}.
#' * `ip`: produto interno convertido em distancia por
#'   \eqn{d(x, y) = 1 - \sum x_i y_i}. E a alternativa computacionalmente mais
#'   rapida em engines vetorizados, mas aciona obrigatoriamente normalizacao L2
#'   automatica das matrizes apos o Z-score e antes da busca.
#' * `cosine`: similaridade angular espelhada como distancia por
#'   \eqn{d(x, y) = 1 - \frac{\sum x_i y_i}{\sqrt{\sum x_i^2} \sqrt{\sum y_i^2}}}.
#'   Tambem aciona normalizacao L2 interna em engines aproximados para favorecer
#'   desempenho vetorial e coerencia numerica.
#'
#' O engine FNN e explicitamente bloqueado para `ip` e `cosine`, pois sua
#' implementacao nativa no pacote suporta somente distancia euclidiana. O produto
#' interno e aceito exclusivamente por `sby_knn_engine = "RcppHNSW"`.
#'
#' @references
#' He, H., Bai, Y., Garcia, E. A., & Li, S. (2008). ADASYN: Adaptive synthetic
#' sampling approach for imbalanced learning. IEEE International Joint Conference
#' on Neural Networks.
#'
#' Malkov, Y. A., & Yashunin, D. A. (2018). Efficient and robust approximate
#' nearest neighbor search using hierarchical navigable small world graphs. IEEE
#' Transactions on Pattern Analysis and Machine Intelligence.
#' @description
#' `sby_step_adanear()` adiciona uma etapa de balanceamento para recipes/tidymodels
#' A etapa executa `sby_nearmiss()` e, por padrao,
#' e pulada em novos dados (`sby_skip = TRUE`), pois altera o numero de linhas
#'
#' @param sby_recipe Objeto recipe que recebera a etapa de balanceamento
#' @param ... Seletores recipes ou tidyselect que identificam uma unica coluna de desfecho binario
#' @param sby_role Papel da etapa mantido para compatibilidade com recipes
#' @param sby_trained Indicador logico de que a etapa ja foi treinada
#' @param sby_columns Coluna de desfecho selecionada durante `prep()`
#' @param sby_under_ratio Fracao da classe majoritaria retida pelo NearMiss-1
#' @param sby_k_under Numero de vizinhos usado pelo NearMiss-1
#' @param sby_seed Semente usada pela rotina de undersampling
#' @param sby_audit Indicador logico para retornar auditoria no `bake()`
#' @param sby_restore_types Indicador logico para restaurar tipos numericos inferidos ao final
#' @param sby_knn_algorithm Algoritmo KNN unificado usado pelo engine selecionado
#' @param sby_knn_engine Engine usado para calcular vizinhos proximos
#' @param sby_distance_metric Metrica de distancia para a busca KNN
#' @param sby_knn_workers Numero de workers usado por engines paralelizaveis
#' @param sby_hnsw_m Parametro de conectividade do indice HNSW no engine RcppHNSW
#' @param sby_hnsw_ef Tamanho da lista dinamica HNSW no engine RcppHNSW
#' @param sby_skip Indicador logico de que a etapa deve ser pulada em novos dados
#' @param sby_id Identificador unico da etapa recipes
#'
#' @return Objeto recipe com a etapa de balanceamento adicionada
#' @export
sby_step_adanear <- function(
  sby_recipe,
  ...,
  sby_role = NA,
  sby_trained = FALSE,
  sby_columns = NULL,
  sby_under_ratio = 0.5,
  sby_k_under = 5L,
  sby_seed = 42L,
  sby_audit = FALSE,
  sby_restore_types = TRUE,
  sby_knn_algorithm = c("auto", "kd_tree", "cover_tree", "brute", "Kmknn", "Vptree", "Exhaustive", "Annoy", "Hnsw"),
  sby_knn_engine = c("auto", "FNN", "BiocNeighbors", "RcppHNSW"),
  sby_distance_metric = c("euclidean", "ip", "cosine"),
  sby_knn_workers = 1L,
  sby_hnsw_m = 16L,
  sby_hnsw_ef = 200L,
  sby_skip = TRUE,
  sby_id = recipes::rand_id("balance")
){
  
  # Verifica se ha solicitacao de interrupcao antes de configurar a etapa
  sby_adanear_check_user_interrupt()

  # Valida dependencias declaradas para a etapa recipes
  recipes::recipes_pkg_check(
    required_pkgs.step_sby_step_adanear()
  )

  # Captura seletores de desfecho informados pelo chamador
  sby_terms <- rlang::enquos(...)

  # Valida indicadores logicos de controle da etapa
  sby_audit <- sby_validate_logical_scalar(
    sby_value = sby_audit,
    sby_name  = "sby_audit"
  )
  sby_restore_types <- sby_validate_logical_scalar(
    sby_value = sby_restore_types,
    sby_name  = "sby_restore_types"
  )
  sby_skip <- sby_validate_logical_scalar(
    sby_value = sby_skip,
    sby_name  = "sby_skip"
  )

  # Resolve opcoes declaradas de algoritmo e engine KNN
  sby_knn_algorithm <- match.arg(
    arg = sby_knn_algorithm
  )
  sby_knn_engine <- match.arg(
    arg = sby_knn_engine
  )
  sby_distance_metric <- match.arg(
    arg = sby_distance_metric
  )

  # Valida recursos paralelos e parametros HNSW
  sby_knn_workers <- sby_validate_knn_workers(
    sby_knn_workers = sby_knn_workers
  )
  sby_hnsw_params <- sby_validate_hnsw_params(
    sby_hnsw_m  = sby_hnsw_m,
    sby_hnsw_ef = sby_hnsw_ef
  )

  # Adiciona etapa configurada ao objeto recipe
  return(recipes::add_step(
    object = sby_recipe,
    step = sby_step_adanear_new(
      sby_terms                   = sby_terms,
      sby_role                    = sby_role,
      sby_trained                 = sby_trained,
      sby_columns                 = sby_columns,
      sby_under_ratio             = sby_under_ratio,
      sby_k_under                 = sby_k_under,
      sby_seed                    = sby_seed,
      sby_audit                   = sby_audit,
      sby_restore_types           = sby_restore_types,
      sby_knn_algorithm           = sby_knn_algorithm,
      sby_knn_engine             = sby_knn_engine,
      sby_distance_metric         = sby_distance_metric,
      sby_knn_workers             = sby_knn_workers,
      sby_hnsw_m                  = sby_hnsw_params$sby_hnsw_m,
      sby_hnsw_ef                 = sby_hnsw_params$sby_hnsw_ef,
      sby_skip                    = sby_skip,
      sby_id                      = sby_id
    )
  ))
}
####
## Fim
#
