
#' @export
bake.step_sby_step_balance <- function(object, new_data, ...) {
  sby_object <- object
  sby_new_data <- new_data
  sby_over_under_check_user_interrupt()

  if (!isTRUE(sby_object$sby_trained)) {
    sby_over_under_abort("'sby_step_balance()' precisa ser treinado com prep() antes de bake()")
  }

  sby_target_column <- sby_object$sby_columns[[1L]]
  if (!sby_target_column %in% names(sby_new_data)) {
    sby_over_under_abort(paste0("Coluna de desfecho nao encontrada em 'new_data': ", sby_target_column))
  }

  sby_original_names <- names(sby_new_data)
  sby_predictor_names <- setdiff(sby_original_names, sby_target_column)
  if (length(sby_predictor_names) < 1L) {
    sby_over_under_abort("'sby_step_balance()' requer ao menos uma coluna preditora")
  }

  sby_sampling_result <- sby_nearmiss(
    sby_predictor_data = as.data.frame(sby_new_data[, sby_predictor_names, drop = FALSE]),
    sby_target_vector = sby_new_data[[sby_target_column]],
    sby_under_ratio = sby_object$sby_under_ratio,
    sby_k_under = sby_object$sby_k_under,
    sby_seed = sby_object$sby_seed,
    sby_audit = TRUE,
    sby_restore_types = sby_object$sby_restore_types,
    sby_knn_algorithm = sby_object$sby_knn_algorithm,
    sby_knn_backend = sby_object$sby_knn_backend,
    sby_knn_workers = sby_object$sby_knn_workers,
    sby_bioc_neighbor_algorithm = sby_object$sby_bioc_neighbor_algorithm,
    sby_hnsw_m = sby_object$sby_hnsw_m,
    sby_hnsw_ef = sby_object$sby_hnsw_ef
  )
  sby_over_under_check_user_interrupt()

  if (isTRUE(sby_object$sby_audit)) {
    return(sby_sampling_result)
  }

  sby_sampling_result$sby_balanced_data
}

####
## Fim
#
