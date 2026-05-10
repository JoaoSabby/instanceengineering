

#' Aplicar oversampling com sby_adasyn
#'
#' @title Aplicar oversampling com sby_adasyn
#' @name sby_adasyn
#' @param sby_predictor_data data.frame ou matrix numerica.
#' @param sby_target_vector vetor binario.
#' @param sby_over_ratio fator de expansao relativa da minoria.
#' @param sby_k_over numero de vizinhos do ADASYN.
#' @param sby_seed semente.
#' @param sby_audit logical; quando TRUE retorna lista completa de auditoria.
#' @param sby_return_scaled logical; quando TRUE inclui matriz escalada na auditoria.
#' @param sby_restore_types logical; restaura tipos ao final.
#' @param sby_knn_algorithm algoritmo para FNN::get.knnx quando sby_knn_backend = "FNN".
#' @param sby_knn_backend backend KNN: "FNN", "BiocNeighbors", "RcppHNSW" ou "auto".
#' @param sby_knn_workers numero de workers para BiocNeighbors/BiocParallel ou RcppHNSW.
#' @param sby_bioc_neighbor_algorithm algoritmo BiocNeighbors.
#' @param sby_hnsw_m conectividade do indice RcppHNSW.
#' @param sby_hnsw_ef tamanho da lista dinamica de construcao/busca RcppHNSW.
#' @return Tibble balanceado quando sby_audit = FALSE; lista de auditoria quando TRUE.
#' @export
# Executa instrucao do fluxo preservado
sby_adasyn <- function(
  # Executa instrucao do fluxo preservado
  sby_predictor_data,
  # Executa instrucao do fluxo preservado
  sby_target_vector,
  # Executa instrucao do fluxo preservado
  sby_over_ratio = 0.2,
  # Executa instrucao do fluxo preservado
  sby_k_over = 5L,
  # Executa instrucao do fluxo preservado
  sby_seed = 42L,
  # Executa instrucao do fluxo preservado
  sby_audit = FALSE,
  # Executa instrucao do fluxo preservado
  sby_return_scaled = FALSE,
  # Executa instrucao do fluxo preservado
  sby_restore_types = TRUE,
  # Executa instrucao do fluxo preservado
  sby_knn_algorithm = c("auto", "cover_tree", "kd_tree", "brute"),
  # Executa instrucao do fluxo preservado
  sby_knn_backend = c("auto", "FNN", "BiocNeighbors", "RcppHNSW"),
  # Executa instrucao do fluxo preservado
  sby_knn_workers = 1L,
  # Executa instrucao do fluxo preservado
  sby_bioc_neighbor_algorithm = c("auto", "Kmknn", "Vptree", "Exhaustive", "Annoy", "Hnsw"),
  # Executa instrucao do fluxo preservado
  sby_hnsw_m = 16L,
  # Executa instrucao do fluxo preservado
  sby_hnsw_ef = 200L
# Executa instrucao do fluxo preservado
) {
  # Executa instrucao do fluxo preservado
  sby_over_under_check_user_interrupt()
  # Executa instrucao do fluxo preservado
  sby_audit <- sby_validate_logical_scalar(sby_audit, "sby_audit")
  # Executa instrucao do fluxo preservado
  sby_return_scaled <- sby_validate_logical_scalar(sby_return_scaled, "sby_return_scaled")
  # Executa instrucao do fluxo preservado
  sby_restore_types <- sby_validate_logical_scalar(sby_restore_types, "sby_restore_types")
  # Executa instrucao do fluxo preservado
  sby_knn_algorithm <- match.arg(sby_knn_algorithm)
  # Executa instrucao do fluxo preservado
  sby_knn_backend <- match.arg(sby_knn_backend)
  # Executa instrucao do fluxo preservado
  sby_bioc_neighbor_algorithm <- match.arg(sby_bioc_neighbor_algorithm)
  # Executa instrucao do fluxo preservado
  sby_knn_workers <- sby_validate_knn_workers(sby_knn_workers)
  # Executa instrucao do fluxo preservado
  sby_hnsw_params <- sby_validate_hnsw_params(sby_hnsw_m, sby_hnsw_ef)
  # Executa instrucao do fluxo preservado
  sby_hnsw_m <- sby_hnsw_params$sby_hnsw_m
  # Executa instrucao do fluxo preservado
  sby_hnsw_ef <- sby_hnsw_params$sby_hnsw_ef
  # Executa instrucao do fluxo preservado
  sby_validate_sampling_inputs(sby_predictor_data, sby_target_vector, sby_seed)

  # Executa instrucao do fluxo preservado
  if (!is.numeric(sby_k_over) || length(sby_k_over) != 1L || is.na(sby_k_over) || sby_k_over < 1L) {
    # Executa instrucao do fluxo preservado
    sby_over_under_abort("'sby_k_over' deve ser inteiro positivo")
  # Executa instrucao do fluxo preservado
  }

  # Executa instrucao do fluxo preservado
  sby_x_matrix <- sby_over_under_as_numeric_matrix(sby_predictor_data)
  # Executa instrucao do fluxo preservado
  colnames(sby_x_matrix) <- sby_over_under_get_column_names(sby_predictor_data)
  # Executa instrucao do fluxo preservado
  sby_knn_algorithm <- sby_resolve_knn_algorithm(sby_knn_algorithm, NCOL(sby_x_matrix))
  # Executa instrucao do fluxo preservado
  sby_knn_backend <- sby_resolve_knn_backend(sby_knn_backend, sby_knn_workers)
  # Executa instrucao do fluxo preservado
  sby_target_factor <- as.factor(sby_target_vector)
  # Executa instrucao do fluxo preservado
  sby_type_info <- sby_infer_numeric_column_types(sby_predictor_data)
  # Executa instrucao do fluxo preservado
  sby_scaling_info <- sby_compute_z_score_params(sby_x_matrix)
  # Executa instrucao do fluxo preservado
  sby_x_scaled <- sby_apply_z_score_scaling_matrix(sby_x_matrix, sby_scaling_info)
  # Executa instrucao do fluxo preservado
  sby_over_under_check_user_interrupt()

  # Executa instrucao do fluxo preservado
  sby_synthetic_count <- sby_compute_minority_expansion_count(sby_target_factor, sby_over_ratio)

  # Executa instrucao do fluxo preservado
  set.seed(sby_seed)
  # Executa instrucao do fluxo preservado
  sby_adasyn_result <- sby_generate_adasyn_samples(
    # Executa instrucao do fluxo preservado
    sby_x_scaled = sby_x_scaled,
    # Executa instrucao do fluxo preservado
    sby_target_factor = sby_target_factor,
    # Executa instrucao do fluxo preservado
    sby_synthetic_count = sby_synthetic_count,
    # Executa instrucao do fluxo preservado
    sby_k_over = sby_k_over,
    # Executa instrucao do fluxo preservado
    sby_knn_algorithm = sby_knn_algorithm,
    # Executa instrucao do fluxo preservado
    sby_knn_backend = sby_knn_backend,
    # Executa instrucao do fluxo preservado
    sby_knn_workers = sby_knn_workers,
    # Executa instrucao do fluxo preservado
    sby_bioc_neighbor_algorithm = sby_bioc_neighbor_algorithm,
    # Executa instrucao do fluxo preservado
    sby_hnsw_m = sby_hnsw_m,
    # Executa instrucao do fluxo preservado
    sby_hnsw_ef = sby_hnsw_ef
  # Executa instrucao do fluxo preservado
  )
  # Executa instrucao do fluxo preservado
  sby_over_under_check_user_interrupt()

  # Executa instrucao do fluxo preservado
  colnames(sby_adasyn_result$x) <- colnames(sby_x_matrix)
  # Executa instrucao do fluxo preservado
  sby_x_restored <- sby_revert_z_score_scaling_matrix(sby_adasyn_result$x, sby_scaling_info)
  # Executa instrucao do fluxo preservado
  sby_over_under_check_user_interrupt()

  # Executa instrucao do fluxo preservado
  sby_final_predictors <- if (sby_restore_types) {
    # Executa instrucao do fluxo preservado
    sby_restore_numeric_column_types(sby_x_restored, sby_type_info, sby_as_data_frame = TRUE)
  # Executa instrucao do fluxo preservado
  } else {
    # Executa instrucao do fluxo preservado
    sby_out <- as.data.frame(sby_x_restored, stringsAsFactors = FALSE)
    # Executa instrucao do fluxo preservado
    names(sby_out) <- sby_type_info$sby_column_name
    # Executa instrucao do fluxo preservado
    sby_out
  # Executa instrucao do fluxo preservado
  }

  # Executa instrucao do fluxo preservado
  sby_balanced_data <- sby_build_balanced_tibble(sby_final_predictors, sby_adasyn_result$y)

  # Executa instrucao do fluxo preservado
  sby_diagnostics <- list(
    # Executa instrucao do fluxo preservado
    sby_input_rows = NROW(sby_x_matrix),
    # Executa instrucao do fluxo preservado
    sby_output_rows = nrow(sby_balanced_data),
    # Executa instrucao do fluxo preservado
    sby_generated_rows = nrow(sby_balanced_data) - nrow(sby_x_matrix),
    # Executa instrucao do fluxo preservado
    sby_knn_backend = sby_knn_backend,
    # Executa instrucao do fluxo preservado
    sby_knn_workers = sby_knn_workers,
    # Executa instrucao do fluxo preservado
    sby_bioc_neighbor_algorithm = if (identical(sby_knn_backend, "BiocNeighbors")) sby_bioc_neighbor_algorithm else NA_character_,
    # Executa instrucao do fluxo preservado
    sby_hnsw_m = if (identical(sby_knn_backend, "RcppHNSW")) sby_hnsw_m else NA_integer_,
    # Executa instrucao do fluxo preservado
    sby_hnsw_ef = if (identical(sby_knn_backend, "RcppHNSW")) sby_hnsw_ef else NA_integer_,
    # Executa instrucao do fluxo preservado
    sby_input_class_distribution = table(sby_target_factor),
    # Executa instrucao do fluxo preservado
    sby_output_class_distribution = table(as.factor(sby_adasyn_result$y))
  # Executa instrucao do fluxo preservado
  )

  # Executa instrucao do fluxo preservado
  sby_result <- list(
    # Executa instrucao do fluxo preservado
    sby_balanced_data = sby_balanced_data,
    # Executa instrucao do fluxo preservado
    sby_type_info = sby_type_info,
    # Executa instrucao do fluxo preservado
    sby_scaling_info = sby_scaling_info,
    # Executa instrucao do fluxo preservado
    sby_diagnostics = sby_diagnostics
  # Executa instrucao do fluxo preservado
  )

  # Executa instrucao do fluxo preservado
  if (isTRUE(sby_return_scaled)) {
    # Executa instrucao do fluxo preservado
    sby_result$sby_balanced_scaled <- list(
      # Executa instrucao do fluxo preservado
      x = sby_adasyn_result$x,
      # Executa instrucao do fluxo preservado
      y = as.factor(sby_adasyn_result$y)
    # Executa instrucao do fluxo preservado
    )
  # Executa instrucao do fluxo preservado
  }

  # Executa instrucao do fluxo preservado
  if (isTRUE(sby_audit)) {
    # Executa instrucao do fluxo preservado
    return(sby_result)
  # Executa instrucao do fluxo preservado
  }

  # Executa instrucao do fluxo preservado
  sby_balanced_data
# Executa instrucao do fluxo preservado
}

####
## Fim
#
