#' Balancear dados em uma recipe com NearMiss-1
#'
#' @details
#' A funcao compoe a interface publica do pacote e valida os contratos de entrada antes de executar a etapa principal
#' O processamento preserva a semantica dos dados, registra pontos de diagnostico quando aplicavel e mantem retornos explicitos para facilitar auditoria em ambientes de producao
#' As chamadas auxiliares usam argumentos nomeados para reduzir ambiguidades durante manutencao e revisao de codigo
#'
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
#' @param sby_knn_algorithm Algoritmo usado pelo backend FNN para busca de vizinhos proximos
#' @param sby_knn_backend Backend usado para calcular vizinhos proximos
#' @param sby_knn_workers Numero de workers usado por backends paralelizaveis
#' @param sby_bioc_neighbor_algorithm Algoritmo usado pelo backend BiocNeighbors
#' @param sby_hnsw_m Parametro de conectividade do indice HNSW no backend RcppHNSW
#' @param sby_hnsw_ef Tamanho da lista dinamica HNSW no backend RcppHNSW
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
  sby_knn_algorithm = c("auto", "cover_tree", "kd_tree", "brute"),
  sby_knn_backend = c("auto", "FNN", "BiocNeighbors", "RcppHNSW"),
  sby_knn_workers = 1L,
  sby_bioc_neighbor_algorithm = c("auto", "Kmknn", "Vptree", "Exhaustive", "Annoy", "Hnsw"),
  sby_hnsw_m = 16L,
  sby_hnsw_ef = 200L,
  sby_skip = TRUE,
  sby_id = recipes::rand_id("balance")
){
  
  # Verifica se ha solicitacao de interrupcao antes de configurar a etapa
  sby_over_under_check_user_interrupt()

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

  # Resolve opcoes declaradas de algoritmo e backend KNN
  sby_knn_algorithm <- match.arg(
    arg = sby_knn_algorithm
  )
  sby_knn_backend <- match.arg(
    arg = sby_knn_backend
  )
  sby_bioc_neighbor_algorithm <- match.arg(
    arg = sby_bioc_neighbor_algorithm
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
      sby_knn_backend             = sby_knn_backend,
      sby_knn_workers             = sby_knn_workers,
      sby_bioc_neighbor_algorithm = sby_bioc_neighbor_algorithm,
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
