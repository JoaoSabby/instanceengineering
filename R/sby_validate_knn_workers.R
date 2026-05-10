
#' Validar numero de workers KNN
#' @noRd
# Executa instrucao do fluxo preservado
sby_validate_knn_workers <- function(sby_knn_workers) {
  # Executa instrucao do fluxo preservado
  if (!is.numeric(sby_knn_workers) || length(sby_knn_workers) != 1L || is.na(sby_knn_workers) || !is.finite(sby_knn_workers) || sby_knn_workers < 1L) {
    # Executa instrucao do fluxo preservado
    sby_over_under_abort("'sby_knn_workers' deve ser inteiro positivo")
  # Executa instrucao do fluxo preservado
  }

  # Executa instrucao do fluxo preservado
  as.integer(sby_knn_workers)
# Executa instrucao do fluxo preservado
}

####
## Fim
#
