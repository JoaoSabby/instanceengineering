
#' Resolver algoritmo KNN automatico quando solicitado
#' @noRd
# Executa instrucao do fluxo preservado
sby_resolve_knn_algorithm <- function(sby_knn_algorithm, sby_predictor_column_count) {
  # Executa instrucao do fluxo preservado
  if (!identical(sby_knn_algorithm, "auto")) {
    # Executa instrucao do fluxo preservado
    return(sby_knn_algorithm)
  # Executa instrucao do fluxo preservado
  }

  # Executa instrucao do fluxo preservado
  if (sby_predictor_column_count > 15L) {
    # Executa instrucao do fluxo preservado
    "brute"
  # Executa instrucao do fluxo preservado
  } else {
    # Executa instrucao do fluxo preservado
    "kd_tree"
  # Executa instrucao do fluxo preservado
  }
# Executa instrucao do fluxo preservado
}

####
## Fim
#
