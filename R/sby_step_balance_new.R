
#' Construtor interno de sby_step_balance
#' @noRd
# Executa instrucao do fluxo preservado
sby_step_balance_new <- function(
  # Executa instrucao do fluxo preservado
  sby_terms,
  # Executa instrucao do fluxo preservado
  sby_role,
  # Executa instrucao do fluxo preservado
  sby_trained,
  # Executa instrucao do fluxo preservado
  sby_columns,
  # Executa instrucao do fluxo preservado
  sby_under_ratio,
  # Executa instrucao do fluxo preservado
  sby_k_under,
  # Executa instrucao do fluxo preservado
  sby_seed,
  # Executa instrucao do fluxo preservado
  sby_audit,
  # Executa instrucao do fluxo preservado
  sby_restore_types,
  # Executa instrucao do fluxo preservado
  sby_knn_algorithm,
  # Executa instrucao do fluxo preservado
  sby_knn_backend,
  # Executa instrucao do fluxo preservado
  sby_knn_workers,
  # Executa instrucao do fluxo preservado
  sby_bioc_neighbor_algorithm,
  # Executa instrucao do fluxo preservado
  sby_hnsw_m,
  # Executa instrucao do fluxo preservado
  sby_hnsw_ef,
  # Executa instrucao do fluxo preservado
  sby_skip,
  # Executa instrucao do fluxo preservado
  sby_id
<<<<<<< HEAD
) {
=======
# Executa instrucao do fluxo preservado
) {
  # Executa instrucao do fluxo preservado
>>>>>>> origin/main
  recipes::step(
    # Executa instrucao do fluxo preservado
    subclass = "sby_step_balance",
    # Executa instrucao do fluxo preservado
    terms = sby_terms,
    # Executa instrucao do fluxo preservado
    role = sby_role,
    # Executa instrucao do fluxo preservado
    trained = sby_trained,
    # Executa instrucao do fluxo preservado
    skip = sby_skip,
    # Executa instrucao do fluxo preservado
    id = sby_id,
    # Executa instrucao do fluxo preservado
    sby_terms = sby_terms,
    # Executa instrucao do fluxo preservado
    sby_role = sby_role,
    # Executa instrucao do fluxo preservado
    sby_trained = sby_trained,
    # Executa instrucao do fluxo preservado
    sby_columns = sby_columns,
    # Executa instrucao do fluxo preservado
    sby_under_ratio = sby_under_ratio,
    # Executa instrucao do fluxo preservado
    sby_k_under = sby_k_under,
    # Executa instrucao do fluxo preservado
    sby_seed = sby_seed,
    # Executa instrucao do fluxo preservado
    sby_audit = sby_audit,
    # Executa instrucao do fluxo preservado
    sby_restore_types = sby_restore_types,
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
    sby_hnsw_ef = sby_hnsw_ef,
    # Executa instrucao do fluxo preservado
    sby_skip = sby_skip,
    # Executa instrucao do fluxo preservado
    sby_id = sby_id
  # Executa instrucao do fluxo preservado
  )
# Executa instrucao do fluxo preservado
}

####
## Fim
#
