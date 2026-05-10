
#' Executar pipeline combinado com sby_adanear
#'
#' @title Executar pipeline combinado com sby_adanear
#' @name sby_adanear
#' @inheritParams sby_adasyn
#' @param sby_under_ratio fator de undersampling.
#' @param sby_k_under vizinhos NearMiss.
#' @return Tibble balanceado quando sby_audit = FALSE; lista de auditoria quando TRUE.
#' @export
# Executa instrucao do fluxo preservado
sby_adanear <- function(
  # Executa instrucao do fluxo preservado
  sby_predictor_data,
  # Executa instrucao do fluxo preservado
  sby_target_vector,
  # Executa instrucao do fluxo preservado
  sby_over_ratio = 0.2,
  # Executa instrucao do fluxo preservado
  sby_under_ratio = 0.5,
  # Executa instrucao do fluxo preservado
  sby_k_over = 5L,
  # Executa instrucao do fluxo preservado
  sby_k_under = 5L,
  # Executa instrucao do fluxo preservado
  sby_seed = 42L,
  # Executa instrucao do fluxo preservado
  sby_audit = FALSE,
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
  sby_knn_algorithm <- sby_resolve_knn_algorithm(sby_knn_algorithm, NCOL(sby_predictor_data))
  # Executa instrucao do fluxo preservado
  sby_knn_backend <- sby_resolve_knn_backend(sby_knn_backend, sby_knn_workers)

  # Executa instrucao do fluxo preservado
  sby_over_result <- sby_adasyn(
    # Executa instrucao do fluxo preservado
    sby_predictor_data = sby_predictor_data,
    # Executa instrucao do fluxo preservado
    sby_target_vector = sby_target_vector,
    # Executa instrucao do fluxo preservado
    sby_over_ratio = sby_over_ratio,
    # Executa instrucao do fluxo preservado
    sby_k_over = sby_k_over,
    # Executa instrucao do fluxo preservado
    sby_seed = sby_seed,
    # Executa instrucao do fluxo preservado
    sby_audit = TRUE,
    # Executa instrucao do fluxo preservado
    sby_return_scaled = TRUE,
    # Executa instrucao do fluxo preservado
    sby_restore_types = FALSE,
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
  sby_under_result <- sby_nearmiss(
    # Executa instrucao do fluxo preservado
    sby_predictor_data = sby_over_result$sby_balanced_scaled$x,
    # Executa instrucao do fluxo preservado
    sby_target_vector = sby_over_result$sby_balanced_scaled$y,
    # Executa instrucao do fluxo preservado
    sby_under_ratio = sby_under_ratio,
    # Executa instrucao do fluxo preservado
    sby_k_under = sby_k_under,
    # Executa instrucao do fluxo preservado
    sby_seed = sby_seed,
    # Executa instrucao do fluxo preservado
    sby_audit = TRUE,
    # Executa instrucao do fluxo preservado
    sby_precomputed_scaling = sby_over_result$sby_scaling_info,
    # Executa instrucao do fluxo preservado
    sby_input_already_scaled = TRUE,
    # Executa instrucao do fluxo preservado
    sby_restore_types = sby_restore_types,
    # Executa instrucao do fluxo preservado
    sby_type_info = sby_over_result$sby_type_info,
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
  sby_diagnostics <- list(
    # Executa instrucao do fluxo preservado
    sby_original_rows = NROW(sby_predictor_data),
    # Executa instrucao do fluxo preservado
    sby_after_oversampling_rows = nrow(sby_over_result$sby_balanced_scaled$x),
    # Executa instrucao do fluxo preservado
    sby_final_rows = nrow(sby_under_result$sby_balanced_data),
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
    sby_original_class_distribution = table(as.factor(sby_target_vector)),
    # Executa instrucao do fluxo preservado
    sby_after_oversampling_class_distribution = table(sby_over_result$sby_balanced_scaled$y),
    # Executa instrucao do fluxo preservado
    sby_final_class_distribution = table(sby_under_result$sby_balanced_data$TARGET)
  # Executa instrucao do fluxo preservado
  )

  # Executa instrucao do fluxo preservado
  sby_result <- list(
    # Executa instrucao do fluxo preservado
    sby_oversampling_result = sby_over_result,
    # Executa instrucao do fluxo preservado
    sby_undersampling_result = sby_under_result,
    # Executa instrucao do fluxo preservado
    sby_balanced_data = sby_under_result$sby_balanced_data,
    # Executa instrucao do fluxo preservado
    sby_diagnostics = sby_diagnostics
  # Executa instrucao do fluxo preservado
  )

  # Executa instrucao do fluxo preservado
  if (isTRUE(sby_audit)) {
    # Executa instrucao do fluxo preservado
    return(sby_result)
  # Executa instrucao do fluxo preservado
  }

  # Executa instrucao do fluxo preservado
  sby_under_result$sby_balanced_data
# Executa instrucao do fluxo preservado
}

####
## Fim
#
