

#' Aplicar oversampling com ADASYN
#'
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
sby_apply_adasyn_oversampling <- function(
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
  sby_over_under_check_user_interrupt()
  sby_audit <- sby_validate_logical_scalar(sby_audit, "sby_audit")
  sby_return_scaled <- sby_validate_logical_scalar(sby_return_scaled, "sby_return_scaled")
  sby_restore_types <- sby_validate_logical_scalar(sby_restore_types, "sby_restore_types")
  sby_knn_algorithm <- match.arg(sby_knn_algorithm)
  sby_knn_backend <- match.arg(sby_knn_backend)
  sby_bioc_neighbor_algorithm <- match.arg(sby_bioc_neighbor_algorithm)
  sby_knn_workers <- sby_validate_knn_workers(sby_knn_workers)
  sby_hnsw_params <- sby_validate_hnsw_params(sby_hnsw_m, sby_hnsw_ef)
  sby_hnsw_m <- sby_hnsw_params$sby_hnsw_m
  sby_hnsw_ef <- sby_hnsw_params$sby_hnsw_ef
  sby_validate_sampling_inputs(sby_predictor_data, sby_target_vector, sby_seed)

  if(!is.numeric(sby_k_over) || length(sby_k_over) != 1L || is.na(sby_k_over) || sby_k_over < 1L){
    sby_over_under_abort("'sby_k_over' deve ser inteiro positivo")
  }

  sby_x_matrix <- sby_over_under_as_numeric_matrix(sby_predictor_data)
  colnames(sby_x_matrix) <- sby_over_under_get_column_names(sby_predictor_data)
  sby_knn_algorithm <- sby_resolve_knn_algorithm(sby_knn_algorithm, NCOL(sby_x_matrix))
  sby_knn_backend <- sby_resolve_knn_backend(sby_knn_backend, sby_knn_workers)
  sby_target_factor <- as.factor(sby_target_vector)
  sby_type_info <- sby_infer_numeric_column_types(sby_predictor_data)
  sby_scaling_info <- sby_compute_z_score_params(sby_x_matrix)
  sby_x_scaled <- sby_apply_z_score_scaling_matrix(sby_x_matrix, sby_scaling_info)
  sby_over_under_check_user_interrupt()

  sby_synthetic_count <- sby_compute_minority_expansion_count(sby_target_factor, sby_over_ratio)

  set.seed(sby_seed)
  sby_adasyn_result <- sby_generate_adasyn_samples(
    sby_x_scaled = sby_x_scaled,
    sby_target_factor = sby_target_factor,
    sby_synthetic_count = sby_synthetic_count,
    sby_k_over = sby_k_over,
    sby_knn_algorithm = sby_knn_algorithm,
    sby_knn_backend = sby_knn_backend,
    sby_knn_workers = sby_knn_workers,
    sby_bioc_neighbor_algorithm = sby_bioc_neighbor_algorithm,
    sby_hnsw_m = sby_hnsw_m,
    sby_hnsw_ef = sby_hnsw_ef
  )
  sby_over_under_check_user_interrupt()

  colnames(sby_adasyn_result$x) <- colnames(sby_x_matrix)
  sby_x_restored <- sby_revert_z_score_scaling_matrix(sby_adasyn_result$x, sby_scaling_info)
  sby_over_under_check_user_interrupt()

  sby_final_predictors <- if(sby_restore_types) {
    sby_restore_numeric_column_types(sby_x_restored, sby_type_info, sby_as_data_frame = TRUE)
  } else {
    sby_out <- as.data.frame(sby_x_restored, stringsAsFactors = FALSE)
    names(sby_out) <- sby_type_info$sby_column_name
    sby_out
  }

  sby_balanced_data <- sby_build_balanced_tibble(sby_final_predictors, sby_adasyn_result$y)

  sby_diagnostics <- list(
    sby_input_rows = NROW(sby_x_matrix),
    sby_output_rows = nrow(sby_balanced_data),
    sby_generated_rows = nrow(sby_balanced_data) - nrow(sby_x_matrix),
    sby_knn_backend = sby_knn_backend,
    sby_knn_workers = sby_knn_workers,
    sby_bioc_neighbor_algorithm = if(identical(sby_knn_backend, "BiocNeighbors")) sby_bioc_neighbor_algorithm else NA_character_,
    sby_hnsw_m = if(identical(sby_knn_backend, "RcppHNSW")) sby_hnsw_m else NA_integer_,
    sby_hnsw_ef = if(identical(sby_knn_backend, "RcppHNSW")) sby_hnsw_ef else NA_integer_,
    sby_input_class_distribution = table(sby_target_factor),
    sby_output_class_distribution = table(as.factor(sby_adasyn_result$y))
  )

  sby_result <- list(
    sby_balanced_data = sby_balanced_data,
    sby_type_info = sby_type_info,
    sby_scaling_info = sby_scaling_info,
    sby_diagnostics = sby_diagnostics
  )

  if(isTRUE(sby_return_scaled)){
    sby_result$sby_balanced_scaled <- list(
      x = sby_adasyn_result$x,
      y = as.factor(sby_adasyn_result$y)
    )
  }

  if(isTRUE(sby_audit)){
    return(sby_result)
  }

  sby_balanced_data
}
