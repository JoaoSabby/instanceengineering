#' Executar pipeline combinado com sby_adanear
#'
#' @details
#' A funcao compoe a interface publica do pacote e valida os contratos de entrada antes de executar a etapa principal
#' O processamento preserva a semantica dos dados, registra pontos de diagnostico quando aplicavel e mantem retornos explicitos para facilitar auditoria em ambientes de producao
#' As chamadas auxiliares usam argumentos nomeados para reduzir ambiguidades durante manutencao e revisao de codigo
#'
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
#' @param sby_knn_algorithm Algoritmo usado pelo backend FNN para busca de vizinhos proximos
#' @param sby_knn_backend Backend usado para calcular vizinhos proximos
#' @param sby_knn_workers Numero de workers usado por backends paralelizaveis
#' @param sby_bioc_neighbor_algorithm Algoritmo usado pelo backend BiocNeighbors
#' @param sby_hnsw_m Parametro de conectividade do indice HNSW no backend RcppHNSW
#' @param sby_hnsw_ef Tamanho da lista dinamica de construcao e busca HNSW no backend RcppHNSW
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
  sby_knn_algorithm = c("auto", "cover_tree", "kd_tree", "brute"),
  sby_knn_backend = c("auto", "FNN", "BiocNeighbors", "RcppHNSW"),
  sby_knn_workers = 1L,
  sby_bioc_neighbor_algorithm = c("auto", "Kmknn", "Vptree", "Exhaustive", "Annoy", "Hnsw"),
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

  # Extrai parametros HNSW normalizados para uso posterior
  sby_hnsw_m  <- sby_hnsw_params$sby_hnsw_m
  sby_hnsw_ef <- sby_hnsw_params$sby_hnsw_ef

  # Resolve algoritmo e backend KNN automaticos para ambas as etapas
  sby_knn_algorithm <- sby_resolve_knn_algorithm(
    sby_knn_algorithm           = sby_knn_algorithm,
    sby_predictor_column_count = NCOL(sby_predictor_data)
  )
  sby_knn_backend <- sby_resolve_knn_backend(
    sby_knn_backend = sby_knn_backend,
    sby_knn_workers = sby_knn_workers
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
    sby_knn_backend             = sby_knn_backend,
    sby_knn_workers             = sby_knn_workers,
    sby_bioc_neighbor_algorithm = sby_bioc_neighbor_algorithm,
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
    sby_knn_backend             = sby_knn_backend,
    sby_knn_workers             = sby_knn_workers,
    sby_bioc_neighbor_algorithm = sby_bioc_neighbor_algorithm,
    sby_hnsw_m                  = sby_hnsw_m,
    sby_hnsw_ef                 = sby_hnsw_ef
  )

  # Verifica se ha solicitacao de interrupcao apos subamostragem
  sby_adanear_check_user_interrupt()

  # Define metadado do algoritmo BiocNeighbors usado no diagnostico
  sby_diagnostic_bioc_neighbor_algorithm <- ifelse(
    test = identical(
      x = sby_knn_backend,
      y = "BiocNeighbors"
    ),
    yes = sby_bioc_neighbor_algorithm,
    no  = NA_character_
  )

  # Define metadado de conectividade HNSW usado no diagnostico
  sby_diagnostic_hnsw_m <- ifelse(
    test = identical(
      x = sby_knn_backend,
      y = "RcppHNSW"
    ),
    yes = sby_hnsw_m,
    no  = NA_integer_
  )

  # Define metadado de busca HNSW usado no diagnostico
  sby_diagnostic_hnsw_ef <- ifelse(
    test = identical(
      x = sby_knn_backend,
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
    sby_knn_backend                           = sby_knn_backend,
    sby_knn_workers                           = sby_knn_workers,
    sby_bioc_neighbor_algorithm               = sby_diagnostic_bioc_neighbor_algorithm,
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
