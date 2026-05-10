
#' Validar numero de workers KNN
#' @noRd
sby_validate_knn_workers <- function(sby_knn_workers) {
  if (!is.numeric(sby_knn_workers) || length(sby_knn_workers) != 1L || is.na(sby_knn_workers) || !is.finite(sby_knn_workers) || sby_knn_workers < 1L) {
    sby_over_under_abort("'sby_knn_workers' deve ser inteiro positivo")
  }

  as.integer(sby_knn_workers)
}

####
## Fim
#
