
#' Construtor interno de sby_step_balance
#' @noRd
sby_step_balance_new <- function(
  sby_terms,
  sby_role,
  sby_trained,
  sby_columns,
  sby_under_ratio,
  sby_k_under,
  sby_seed,
  sby_audit,
  sby_restore_types,
  sby_knn_algorithm,
  sby_knn_backend,
  sby_knn_workers,
  sby_bioc_neighbor_algorithm,
  sby_hnsw_m,
  sby_hnsw_ef,
  sby_skip,
  sby_id
) {
  recipes::step(
    subclass = "sby_step_balance",
    terms = sby_terms,
    role = sby_role,
    trained = sby_trained,
    skip = sby_skip,
    id = sby_id,
    sby_terms = sby_terms,
    sby_role = sby_role,
    sby_trained = sby_trained,
    sby_columns = sby_columns,
    sby_under_ratio = sby_under_ratio,
    sby_k_under = sby_k_under,
    sby_seed = sby_seed,
    sby_audit = sby_audit,
    sby_restore_types = sby_restore_types,
    sby_knn_algorithm = sby_knn_algorithm,
    sby_knn_backend = sby_knn_backend,
    sby_knn_workers = sby_knn_workers,
    sby_bioc_neighbor_algorithm = sby_bioc_neighbor_algorithm,
    sby_hnsw_m = sby_hnsw_m,
    sby_hnsw_ef = sby_hnsw_ef,
    sby_skip = sby_skip,
    sby_id = sby_id
  )
}

####
## Fim
#
