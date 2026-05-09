
#' @export
prep.step_sby_step_balance <- function(x, training, info = NULL, ...){
  sby_x <- x
  sby_training <- training
  sby_info <- info
  sby_over_under_check_user_interrupt()

  sby_selected_columns <- recipes::recipes_eval_select(sby_x$sby_terms, sby_training, sby_info)
  if(length(sby_selected_columns) != 1L){
    sby_over_under_abort("'sby_step_balance()' deve selecionar exatamente uma coluna de desfecho")
  }

  sby_step_balance_new(
    sby_terms = sby_x$sby_terms,
    sby_role = sby_x$sby_role,
    sby_trained = TRUE,
    sby_columns = names(sby_selected_columns),
    sby_under_ratio = sby_x$sby_under_ratio,
    sby_k_under = sby_x$sby_k_under,
    sby_seed = sby_x$sby_seed,
    sby_audit = sby_x$sby_audit,
    sby_restore_types = sby_x$sby_restore_types,
    sby_knn_algorithm = sby_x$sby_knn_algorithm,
    sby_knn_backend = sby_x$sby_knn_backend,
    sby_knn_workers = sby_x$sby_knn_workers,
    sby_bioc_neighbor_algorithm = sby_x$sby_bioc_neighbor_algorithm,
    sby_hnsw_m = sby_x$sby_hnsw_m,
    sby_hnsw_ef = sby_x$sby_hnsw_ef,
    sby_skip = sby_x$sby_skip,
    sby_id = sby_x$sby_id
  )
}
