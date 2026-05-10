
#' @export
<<<<<<< HEAD
bake.step_sby_step_balance <- function(object, new_data, ...) {
=======
# Executa instrucao do fluxo preservado
bake.step_sby_step_balance <- function(object, new_data, ...) {
  # Executa instrucao do fluxo preservado
>>>>>>> origin/main
  sby_object <- object
  # Executa instrucao do fluxo preservado
  sby_new_data <- new_data
  # Executa instrucao do fluxo preservado
  sby_over_under_check_user_interrupt()

<<<<<<< HEAD
  if (!isTRUE(sby_object$sby_trained)) {
=======
  # Executa instrucao do fluxo preservado
  if (!isTRUE(sby_object$sby_trained)) {
    # Executa instrucao do fluxo preservado
>>>>>>> origin/main
    sby_over_under_abort("'sby_step_balance()' precisa ser treinado com prep() antes de bake()")
  # Executa instrucao do fluxo preservado
  }

  # Executa instrucao do fluxo preservado
  sby_target_column <- sby_object$sby_columns[[1L]]
<<<<<<< HEAD
  if (!sby_target_column %in% names(sby_new_data)) {
=======
  # Executa instrucao do fluxo preservado
  if (!sby_target_column %in% names(sby_new_data)) {
    # Executa instrucao do fluxo preservado
>>>>>>> origin/main
    sby_over_under_abort(paste0("Coluna de desfecho nao encontrada em 'new_data': ", sby_target_column))
  # Executa instrucao do fluxo preservado
  }

  # Executa instrucao do fluxo preservado
  sby_original_names <- names(sby_new_data)
  # Executa instrucao do fluxo preservado
  sby_predictor_names <- setdiff(sby_original_names, sby_target_column)
<<<<<<< HEAD
  if (length(sby_predictor_names) < 1L) {
=======
  # Executa instrucao do fluxo preservado
  if (length(sby_predictor_names) < 1L) {
    # Executa instrucao do fluxo preservado
>>>>>>> origin/main
    sby_over_under_abort("'sby_step_balance()' requer ao menos uma coluna preditora")
  # Executa instrucao do fluxo preservado
  }

<<<<<<< HEAD
  sby_sampling_result <- sby_nearmiss(
=======
  # Executa instrucao do fluxo preservado
  sby_sampling_result <- sby_nearmiss(
    # Executa instrucao do fluxo preservado
>>>>>>> origin/main
    sby_predictor_data = as.data.frame(sby_new_data[, sby_predictor_names, drop = FALSE]),
    # Executa instrucao do fluxo preservado
    sby_target_vector = sby_new_data[[sby_target_column]],
    # Executa instrucao do fluxo preservado
    sby_under_ratio = sby_object$sby_under_ratio,
    # Executa instrucao do fluxo preservado
    sby_k_under = sby_object$sby_k_under,
    # Executa instrucao do fluxo preservado
    sby_seed = sby_object$sby_seed,
    # Executa instrucao do fluxo preservado
    sby_audit = TRUE,
    # Executa instrucao do fluxo preservado
    sby_restore_types = sby_object$sby_restore_types,
    # Executa instrucao do fluxo preservado
    sby_knn_algorithm = sby_object$sby_knn_algorithm,
    # Executa instrucao do fluxo preservado
    sby_knn_backend = sby_object$sby_knn_backend,
    # Executa instrucao do fluxo preservado
    sby_knn_workers = sby_object$sby_knn_workers,
    # Executa instrucao do fluxo preservado
    sby_bioc_neighbor_algorithm = sby_object$sby_bioc_neighbor_algorithm,
    # Executa instrucao do fluxo preservado
    sby_hnsw_m = sby_object$sby_hnsw_m,
    # Executa instrucao do fluxo preservado
    sby_hnsw_ef = sby_object$sby_hnsw_ef
  # Executa instrucao do fluxo preservado
  )
  # Executa instrucao do fluxo preservado
  sby_over_under_check_user_interrupt()

<<<<<<< HEAD
  if (isTRUE(sby_object$sby_audit)) {
=======
  # Executa instrucao do fluxo preservado
  if (isTRUE(sby_object$sby_audit)) {
    # Executa instrucao do fluxo preservado
>>>>>>> origin/main
    return(sby_sampling_result)
  # Executa instrucao do fluxo preservado
  }

  # Executa instrucao do fluxo preservado
  sby_sampling_result$sby_balanced_data
# Executa instrucao do fluxo preservado
}

####
## Fim
#
