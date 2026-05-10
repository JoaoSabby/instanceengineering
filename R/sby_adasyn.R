#' Aplicar sobreamostragem ADASYN em dados binarios
#'
#' @details
#' A funcao compoe a interface publica do pacote e valida os contratos de entrada antes de executar a etapa principal
#' O processamento preserva a semantica dos dados, registra pontos de diagnostico quando aplicavel e mantem retornos explicitos para facilitar auditoria em ambientes de producao
#' As chamadas auxiliares usam argumentos nomeados para reduzir ambiguidades durante manutencao e revisao de codigo
#'
#' @title Aplicar sobreamostragem ADASYN em dados binarios
#' @name sby_adasyn
#' @param sby_predictor_data Data frame ou matriz com variaveis preditoras numericas
#' @param sby_target_vector Vetor de classe binaria associado as linhas de entrada
#' @param sby_over_ratio Fator relativo de expansao da classe minoritaria
#' @param sby_k_over Numero de vizinhos usados pelo criterio ADASYN
#' @param sby_seed Semente usada para controlar componentes estocasticos do processamento
#' @param sby_audit Indicador logico para retornar metadados completos de auditoria
#' @param sby_return_scaled Indicador logico para incluir matriz escalada na auditoria
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
sby_adasyn <- function(
  sby_predictor_data,
  sby_target_vector,
  sby_over_ratio = 0.2,
  sby_k_over = 5L,
  sby_seed = 42L,
  sby_audit = FALSE,
  sby_return_scaled = FALSE,
  sby_restore_types = TRUE,
  sby_knn_algorithm = c("auto", "cover_tree", "kd_tree", "brute"),
  sby_knn_backend = c("auto", "FNN", "BiocNeighbors", "RcppHNSW"),
  sby_knn_workers = 1L,
  sby_bioc_neighbor_algorithm = c("auto", "Kmknn", "Vptree", "Exhaustive", "Annoy", "Hnsw"),
  sby_hnsw_m = 16L,
  sby_hnsw_ef = 200L
){
  # Verifica se ha solicitacao de interrupcao pelo usuario
  sby_over_under_check_user_interrupt()

  # Valida parametros logicos escalares de controle operacional
  sby_audit <- sby_validate_logical_scalar(
    sby_value = sby_audit,
    sby_name  = "sby_audit"
  )
  sby_return_scaled <- sby_validate_logical_scalar(
    sby_value = sby_return_scaled,
    sby_name  = "sby_return_scaled"
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

  # Valida consistencia basica entre preditores, alvo e semente
  sby_validate_sampling_inputs(
    sby_predictor_data = sby_predictor_data,
    sby_target_vector  = sby_target_vector,
    sby_seed           = sby_seed
  )

  # Verifica se o numero de vizinhos de sobreamostragem e valido
  if(!(is.numeric(sby_k_over) && length(sby_k_over) == 1L && !is.na(sby_k_over) && sby_k_over >= 1L)){

    # Aborta quando o parametro ADASYN nao representa inteiro positivo
    sby_over_under_abort(
      sby_message = "'sby_k_over' deve ser inteiro positivo"
    )
  }

  # Converte preditores e preserva nomes de colunas para o processamento matricial
  sby_x_matrix <- sby_over_under_as_numeric_matrix(
    sby_predictor_data = sby_predictor_data
  )
  colnames(sby_x_matrix) <- sby_over_under_get_column_names(
    sby_predictor_data = sby_predictor_data
  )

  # Resolve algoritmo e backend KNN automaticos conforme dados e workers
  sby_knn_algorithm <- sby_resolve_knn_algorithm(
    sby_knn_algorithm           = sby_knn_algorithm,
    sby_predictor_column_count = NCOL(sby_x_matrix)
  )
  sby_knn_backend <- sby_resolve_knn_backend(
    sby_knn_backend = sby_knn_backend,
    sby_knn_workers = sby_knn_workers
  )

  # Prepara alvo, metadados de tipos e matriz padronizada para ADASYN
  sby_target_factor <- as.factor(
    x = sby_target_vector
  )
  sby_type_info <- sby_infer_numeric_column_types(
    sby_data_frame = sby_predictor_data
  )
  sby_scaling_info <- sby_compute_z_score_params(
    sby_x_matrix = sby_x_matrix
  )
  sby_x_scaled <- sby_apply_z_score_scaling_matrix(
    sby_x_matrix      = sby_x_matrix,
    sby_scaling_info = sby_scaling_info
  )

  # Verifica se ha solicitacao de interrupcao antes da geracao sintetica
  sby_over_under_check_user_interrupt()

  # Calcula quantidade de amostras sinteticas a gerar
  sby_synthetic_count <- sby_compute_minority_expansion_count(
    sby_target_factor = sby_target_factor,
    sby_over_ratio    = sby_over_ratio
  )

  # Define semente para manter reprodutibilidade da geracao ADASYN
  set.seed(
    seed = sby_seed
  )

  # Gera amostras sinteticas em escala padronizada
  sby_adasyn_result <- sby_generate_adasyn_samples(
    sby_x_scaled                 = sby_x_scaled,
    sby_target_factor            = sby_target_factor,
    sby_synthetic_count          = sby_synthetic_count,
    sby_k_over                   = sby_k_over,
    sby_knn_algorithm            = sby_knn_algorithm,
    sby_knn_backend              = sby_knn_backend,
    sby_knn_workers              = sby_knn_workers,
    sby_bioc_neighbor_algorithm  = sby_bioc_neighbor_algorithm,
    sby_hnsw_m                   = sby_hnsw_m,
    sby_hnsw_ef                  = sby_hnsw_ef
  )

  # Verifica se ha solicitacao de interrupcao apos a geracao sintetica
  sby_over_under_check_user_interrupt()

  # Restaura nomes de colunas e escala original dos preditores expandidos
  colnames(sby_adasyn_result$x) <- colnames(sby_x_matrix)
  sby_x_restored <- sby_revert_z_score_scaling_matrix(
    sby_x_matrix      = sby_adasyn_result$x,
    sby_scaling_info = sby_scaling_info
  )

  # Verifica se ha solicitacao de interrupcao apos reversao de escala
  sby_over_under_check_user_interrupt()

  # Define preditores finais com ou sem restauracao dos tipos originais
  sby_final_predictors <- if(sby_restore_types){

    # Restaura classes numericas originais e retorna estrutura tabular
    sby_restore_numeric_column_types(
      sby_x_matrix       = sby_x_restored,
      sby_type_info     = sby_type_info,
      sby_as_data_frame = TRUE
    )
  }else{

    # Converte matriz restaurada em data frame mantendo nomes originais
    sby_out <- as.data.frame(
      x = sby_x_restored,
      stringsAsFactors = FALSE
    )
    names(sby_out) <- sby_type_info$sby_column_name
    sby_out
  }

  # Combina preditores finais e alvo expandido em tibble balanceado
  sby_balanced_data <- sby_build_balanced_tibble(
    sby_predictor_data = sby_final_predictors,
    sby_target_vector  = sby_adasyn_result$y
  )

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

  # Consolida diagnosticos de entrada, saida e configuracao KNN
  sby_diagnostics <- list(
    sby_input_rows                = NROW(sby_x_matrix),
    sby_output_rows               = nrow(sby_balanced_data),
    sby_generated_rows            = nrow(sby_balanced_data) - nrow(sby_x_matrix),
    sby_knn_backend               = sby_knn_backend,
    sby_knn_workers               = sby_knn_workers,
    sby_bioc_neighbor_algorithm   = sby_diagnostic_bioc_neighbor_algorithm,
    sby_hnsw_m                    = sby_diagnostic_hnsw_m,
    sby_hnsw_ef                   = sby_diagnostic_hnsw_ef,
    sby_input_class_distribution  = table(sby_target_factor),
    sby_output_class_distribution = table(as.factor(sby_adasyn_result$y))
  )

  # Consolida resultado completo para retorno auditavel
  sby_result <- list(
    sby_balanced_data = sby_balanced_data,
    sby_type_info     = sby_type_info,
    sby_scaling_info  = sby_scaling_info,
    sby_diagnostics   = sby_diagnostics
  )

  # Inclui matriz escalada quando solicitada para encadeamento interno
  if(isTRUE(sby_return_scaled)){

    # Anexa representacao escalada ao resultado de auditoria
    sby_result$sby_balanced_scaled <- list(
      x = sby_adasyn_result$x,
      y = as.factor(sby_adasyn_result$y)
    )
  }

  # Retorna estrutura completa quando auditoria foi solicitada
  if(isTRUE(sby_audit)){

    # Entrega dados balanceados, metadados e diagnosticos ao chamador
    return(sby_result)
  }

  # Retorna apenas os dados balanceados no fluxo operacional padrao
  return(sby_balanced_data)
}
####
## Fim
#
