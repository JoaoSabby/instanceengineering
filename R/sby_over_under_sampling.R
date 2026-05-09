
#' Pipeline combinado ADASYN + NearMiss
#'
#' @inheritParams sby_apply_adasyn_oversampling
#' @param sby_under_ratio fator de undersampling.
#' @param sby_k_under vizinhos NearMiss.
#' @return Tibble balanceado quando sby_audit = FALSE; lista de auditoria quando TRUE.
#' @export
sby_over_under_sampling <- function(
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
  sby_over_under_check_user_interrupt()
  sby_audit <- sby_validate_logical_scalar(sby_audit, "sby_audit")
  sby_restore_types <- sby_validate_logical_scalar(sby_restore_types, "sby_restore_types")
  sby_knn_algorithm <- match.arg(sby_knn_algorithm)
  sby_knn_backend <- match.arg(sby_knn_backend)
  sby_bioc_neighbor_algorithm <- match.arg(sby_bioc_neighbor_algorithm)
  sby_knn_workers <- sby_validate_knn_workers(sby_knn_workers)
  sby_hnsw_params <- sby_validate_hnsw_params(sby_hnsw_m, sby_hnsw_ef)
  sby_hnsw_m <- sby_hnsw_params$sby_hnsw_m
  sby_hnsw_ef <- sby_hnsw_params$sby_hnsw_ef
  sby_knn_algorithm <- sby_resolve_knn_algorithm(sby_knn_algorithm, NCOL(sby_predictor_data))
  sby_knn_backend <- sby_resolve_knn_backend(sby_knn_backend, sby_knn_workers)

  sby_over_result <- sby_apply_adasyn_oversampling(
    sby_predictor_data = sby_predictor_data,
    sby_target_vector = sby_target_vector,
    sby_over_ratio = sby_over_ratio,
    sby_k_over = sby_k_over,
    sby_seed = sby_seed,
    sby_audit = TRUE,
    sby_return_scaled = TRUE,
    sby_restore_types = FALSE,
    sby_knn_algorithm = sby_knn_algorithm,
    sby_knn_backend = sby_knn_backend,
    sby_knn_workers = sby_knn_workers,
    sby_bioc_neighbor_algorithm = sby_bioc_neighbor_algorithm,
    sby_hnsw_m = sby_hnsw_m,
    sby_hnsw_ef = sby_hnsw_ef
  )
  sby_over_under_check_user_interrupt()

  sby_under_result <- sby_apply_nearmiss_undersampling(
    sby_predictor_data = sby_over_result$sby_balanced_scaled$x,
    sby_target_vector = sby_over_result$sby_balanced_scaled$y,
    sby_under_ratio = sby_under_ratio,
    sby_k_under = sby_k_under,
    sby_seed = sby_seed,
    sby_audit = TRUE,
    sby_precomputed_scaling = sby_over_result$sby_scaling_info,
    sby_input_already_scaled = TRUE,
    sby_restore_types = sby_restore_types,
    sby_type_info = sby_over_result$sby_type_info,
    sby_knn_algorithm = sby_knn_algorithm,
    sby_knn_backend = sby_knn_backend,
    sby_knn_workers = sby_knn_workers,
    sby_bioc_neighbor_algorithm = sby_bioc_neighbor_algorithm,
    sby_hnsw_m = sby_hnsw_m,
    sby_hnsw_ef = sby_hnsw_ef
  )
  sby_over_under_check_user_interrupt()

  sby_diagnostics <- list(
    sby_original_rows = NROW(sby_predictor_data),
    sby_after_oversampling_rows = nrow(sby_over_result$sby_balanced_scaled$x),
    sby_final_rows = nrow(sby_under_result$sby_balanced_data),
    sby_knn_backend = sby_knn_backend,
    sby_knn_workers = sby_knn_workers,
    sby_bioc_neighbor_algorithm = if(identical(sby_knn_backend, "BiocNeighbors")) sby_bioc_neighbor_algorithm else NA_character_,
    sby_hnsw_m = if(identical(sby_knn_backend, "RcppHNSW")) sby_hnsw_m else NA_integer_,
    sby_hnsw_ef = if(identical(sby_knn_backend, "RcppHNSW")) sby_hnsw_ef else NA_integer_,
    sby_original_class_distribution = table(as.factor(sby_target_vector)),
    sby_after_oversampling_class_distribution = table(sby_over_result$sby_balanced_scaled$y),
    sby_final_class_distribution = table(sby_under_result$sby_balanced_data$TARGET)
  )

  sby_result <- list(
    sby_oversampling_result = sby_over_result,
    sby_undersampling_result = sby_under_result,
    sby_balanced_data = sby_under_result$sby_balanced_data,
    sby_diagnostics = sby_diagnostics
  )

  if(isTRUE(sby_audit)){
    return(sby_result)
  }

  sby_under_result$sby_balanced_data
}
