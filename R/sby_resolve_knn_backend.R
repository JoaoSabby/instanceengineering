
#' Resolver backend KNN automatico quando solicitado
#' @noRd
<<<<<<< HEAD
sby_resolve_knn_backend <- function(sby_knn_backend, sby_knn_workers) {
  if (!identical(sby_knn_backend, "auto")) {
=======
# Executa instrucao do fluxo preservado
sby_resolve_knn_backend <- function(sby_knn_backend, sby_knn_workers) {
  # Executa instrucao do fluxo preservado
  if (!identical(sby_knn_backend, "auto")) {
    # Executa instrucao do fluxo preservado
>>>>>>> origin/main
    return(sby_knn_backend)
  # Executa instrucao do fluxo preservado
  }

<<<<<<< HEAD
  if (sby_knn_workers > 1L) {
    if (requireNamespace("BiocNeighbors", quietly = TRUE) && requireNamespace("BiocParallel", quietly = TRUE)) {
=======
  # Executa instrucao do fluxo preservado
  if (sby_knn_workers > 1L) {
    # Executa instrucao do fluxo preservado
    if (requireNamespace("BiocNeighbors", quietly = TRUE) && requireNamespace("BiocParallel", quietly = TRUE)) {
      # Executa instrucao do fluxo preservado
>>>>>>> origin/main
      return("BiocNeighbors")
    # Executa instrucao do fluxo preservado
    }

    # Executa instrucao do fluxo preservado
    sby_over_under_warn("'sby_knn_workers > 1' foi solicitado, mas BiocNeighbors/BiocParallel nao estao instalados; usando FNN sem paralelismo.")
  # Executa instrucao do fluxo preservado
  }

  # Executa instrucao do fluxo preservado
  "FNN"
# Executa instrucao do fluxo preservado
}

####
## Fim
#
