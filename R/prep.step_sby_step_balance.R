
#' @export
# Executa instrucao do fluxo preservado
prep.step_sby_step_balance <- function(x, training, info = NULL, ...) {
  # Executa instrucao do fluxo preservado
  sby_x <- x
  # Executa instrucao do fluxo preservado
  sby_training <- training
  # Executa instrucao do fluxo preservado
  sby_info <- info
  # Executa instrucao do fluxo preservado
  sby_over_under_check_user_interrupt()

  # Executa instrucao do fluxo preservado
  sby_selected_columns <- recipes::recipes_eval_select(sby_x$sby_terms, sby_training, sby_info)
  # Executa instrucao do fluxo preservado
  if (length(sby_selected_columns) != 1L) {
    # Executa instrucao do fluxo preservado
    sby_over_under_abort("'sby_step_balance()' deve selecionar exatamente uma coluna de desfecho")
  # Executa instrucao do fluxo preservado
  }

  # Executa instrucao do fluxo preservado
  sby_step_balance_new(
    # Executa instrucao do fluxo preservado
    sby_terms = sby_x$sby_terms,
    # Executa instrucao do fluxo preservado
    sby_role = sby_x$sby_role,
    # Executa instrucao do fluxo preservado
    sby_trained = TRUE,
    # Executa instrucao do fluxo preservado
    sby_columns = names(sby_selected_columns),
    # Executa instrucao do fluxo preservado
    sby_under_ratio = sby_x$sby_under_ratio,
    # Executa instrucao do fluxo preservado
    sby_k_under = sby_x$sby_k_under,
    # Executa instrucao do fluxo preservado
    sby_seed = sby_x$sby_seed,
    # Executa instrucao do fluxo preservado
    sby_audit = sby_x$sby_audit,
    # Executa instrucao do fluxo preservado
    sby_restore_types = sby_x$sby_restore_types,
    # Executa instrucao do fluxo preservado
    sby_knn_algorithm = sby_x$sby_knn_algorithm,
    # Executa instrucao do fluxo preservado
    sby_knn_backend = sby_x$sby_knn_backend,
    # Executa instrucao do fluxo preservado
    sby_knn_workers = sby_x$sby_knn_workers,
    # Executa instrucao do fluxo preservado
    sby_bioc_neighbor_algorithm = sby_x$sby_bioc_neighbor_algorithm,
    # Executa instrucao do fluxo preservado
    sby_hnsw_m = sby_x$sby_hnsw_m,
    # Executa instrucao do fluxo preservado
    sby_hnsw_ef = sby_x$sby_hnsw_ef,
    # Executa instrucao do fluxo preservado
    sby_skip = sby_x$sby_skip,
    # Executa instrucao do fluxo preservado
    sby_id = sby_x$sby_id
  # Executa instrucao do fluxo preservado
  )
# Executa instrucao do fluxo preservado
}

####
## Fim
#
