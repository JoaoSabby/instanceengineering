#' Executar pipeline combinado com sby_adanear
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
#' @title Executar pipeline combinado com sby_adanear
#' @name sby_adanear
#' @param sby_predictor_data Data frame ou matriz com variaveis preditoras numericas
#' @param sby_target_vector Vetor de classe binaria associado as linhas de entrada
#' @param sby_over_ratio Fator relativo de expansao da classe minoritaria
#' @param sby_under_ratio Fracao da classe majoritaria a ser retida apos subamostragem
#' @param sby_k_over Numero de vizinhos usados pelo criterio ADASYN
#' @param sby_k_under Numero de vizinhos usados pelo criterio NearMiss
#' @param sby_seed Semente usada para controlar componentes estocasticos do processamento
#' @param sby_audit Indicador logico para retornar metadados completos de auditoria
#' @param sby_restore_types Indicador logico para restaurar tipos numericos originais ao final
#' @param sby_knn_algorithm Algoritmo KNN unificado usado pelo engine selecionado
#' @param sby_knn_engine Engine usado para calcular vizinhos proximos
#' @param sby_distance_metric Metrica de distancia para a busca KNN
#' @param sby_knn_workers Numero de workers usado por engines paralelizaveis
#' @param sby_hnsw_m Parametro de conectividade do indice HNSW no engine RcppHNSW
#' @param sby_hnsw_ef Tamanho da lista dinamica de construcao e busca HNSW no engine RcppHNSW
#'
#' @return Tibble balanceado quando `sby_audit = FALSE`; lista de auditoria quando `sby_audit = TRUE`
#' @export
sby_adanear <- function(
  sby_predictor_data,
  sby_target_vector,
  sby_over_ratio = 0.2,
  sby_under_ratio = 0.5,
  sby_k_over = 5L,
  sby_k_under = 5L,
  sby_seed = 42L,
  sby_audit = FALSE,
  sby_restore_types = TRUE,
  sby_knn_algorithm = c("auto", "kd_tree", "cover_tree", "brute", "Kmknn", "Vptree", "Exhaustive", "Annoy", "Hnsw"),
  sby_knn_engine = c("auto", "FNN", "BiocNeighbors", "RcppHNSW"),
  sby_distance_metric = c("euclidean", "ip", "cosine"),
  sby_knn_workers = 1L,
  sby_hnsw_m = 16L,
  sby_hnsw_ef = 200L
){
  # Verifica se ha solicitacao de interrupcao pelo usuario
  sby_adanear_check_user_interrupt()

  # Valida parametros logicos escalares de controle operacional
  sby_audit <- sby_validate_logical_scalar(
    sby_value = sby_audit,
    sby_name  = "sby_audit"
  )
  sby_restore_types <- sby_validate_logical_scalar(
    sby_value = sby_restore_types,
    sby_name  = "sby_restore_types"
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

  # Extrai parametros HNSW normalizados para uso posterior
  sby_hnsw_m  <- sby_hnsw_params$sby_hnsw_m
  sby_hnsw_ef <- sby_hnsw_params$sby_hnsw_ef

  # Resolve engine e algoritmo KNN automaticos para ambas as etapas
  sby_knn_engine <- sby_resolve_knn_engine(
    sby_knn_engine = sby_knn_engine,
    sby_knn_workers = sby_knn_workers
  )
  sby_knn_algorithm <- sby_resolve_knn_algorithm(
    sby_knn_algorithm          = sby_knn_algorithm,
    sby_predictor_column_count = NCOL(sby_predictor_data),
    sby_knn_engine             = sby_knn_engine
  )

  # Executa sobreamostragem ADASYN mantendo matriz escalada para encadeamento
  sby_over_result <- sby_adasyn(
    sby_predictor_data          = sby_predictor_data,
    sby_target_vector           = sby_target_vector,
    sby_over_ratio              = sby_over_ratio,
    sby_k_over                  = sby_k_over,
    sby_seed                    = sby_seed,
    sby_audit                   = TRUE,
    sby_return_scaled           = TRUE,
    sby_restore_types           = FALSE,
    sby_knn_algorithm           = sby_knn_algorithm,
    sby_knn_engine             = sby_knn_engine,
    sby_distance_metric         = sby_distance_metric,
    sby_knn_workers             = sby_knn_workers,
    sby_hnsw_m                  = sby_hnsw_m,
    sby_hnsw_ef                 = sby_hnsw_ef
  )

  # Verifica se ha solicitacao de interrupcao apos sobreamostragem
  sby_adanear_check_user_interrupt()

  # Executa subamostragem NearMiss reutilizando escala e tipos inferidos
  sby_under_result <- sby_nearmiss(
    sby_predictor_data          = sby_over_result$sby_balanced_scaled$x,
    sby_target_vector           = sby_over_result$sby_balanced_scaled$y,
    sby_under_ratio             = sby_under_ratio,
    sby_k_under                 = sby_k_under,
    sby_seed                    = sby_seed,
    sby_audit                   = TRUE,
    sby_precomputed_scaling     = sby_over_result$sby_scaling_info,
    sby_input_already_scaled    = TRUE,
    sby_restore_types           = sby_restore_types,
    sby_type_info               = sby_over_result$sby_type_info,
    sby_knn_algorithm           = sby_knn_algorithm,
    sby_knn_engine             = sby_knn_engine,
    sby_distance_metric         = sby_distance_metric,
    sby_knn_workers             = sby_knn_workers,
    sby_hnsw_m                  = sby_hnsw_m,
    sby_hnsw_ef                 = sby_hnsw_ef
  )

  # Verifica se ha solicitacao de interrupcao apos subamostragem
  sby_adanear_check_user_interrupt()

  # Define metadado de conectividade HNSW usado no diagnostico
  sby_diagnostic_hnsw_m <- ifelse(
    test = identical(
      x = sby_knn_engine,
      y = "RcppHNSW"
    ),
    yes = sby_hnsw_m,
    no  = NA_integer_
  )

  # Define metadado de busca HNSW usado no diagnostico
  sby_diagnostic_hnsw_ef <- ifelse(
    test = identical(
      x = sby_knn_engine,
      y = "RcppHNSW"
    ),
    yes = sby_hnsw_ef,
    no  = NA_integer_
  )

  # Consolida diagnosticos das etapas de sobreamostragem e subamostragem
  sby_diagnostics <- list(
    sby_original_rows                         = NROW(sby_predictor_data),
    sby_after_oversampling_rows               = nrow(sby_over_result$sby_balanced_scaled$x),
    sby_final_rows                            = nrow(sby_under_result$sby_balanced_data),
    sby_knn_engine                           = sby_knn_engine,
    sby_distance_metric                       = sby_distance_metric,
    sby_knn_workers                           = sby_knn_workers,
    sby_hnsw_m                                = sby_diagnostic_hnsw_m,
    sby_hnsw_ef                               = sby_diagnostic_hnsw_ef,
    sby_original_class_distribution           = table(as.factor(sby_target_vector)),
    sby_after_oversampling_class_distribution = table(sby_over_result$sby_balanced_scaled$y),
    sby_final_class_distribution              = table(sby_under_result$sby_balanced_data$TARGET)
  )

  # Consolida resultado completo para retorno auditavel
  sby_result <- list(
    sby_oversampling_result  = sby_over_result,
    sby_undersampling_result = sby_under_result,
    sby_balanced_data        = sby_under_result$sby_balanced_data,
    sby_diagnostics          = sby_diagnostics
  )

  # Retorna estrutura completa quando auditoria foi solicitada
  if(isTRUE(sby_audit)){

    # Entrega resultados intermediarios, dados finais e diagnosticos ao chamador
    return(sby_result)
  }

  # Retorna apenas os dados balanceados no fluxo operacional padrao
  return(sby_under_result$sby_balanced_data)
}
####
## Fim
#
