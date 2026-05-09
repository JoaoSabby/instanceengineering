
#' Resolver backend KNN automatico quando solicitado
#' @noRd
sby_resolve_knn_backend <- function(sby_knn_backend, sby_knn_workers){
  if(!identical(sby_knn_backend, "auto")){
    return(sby_knn_backend)
  }

  if(sby_knn_workers > 1L){
    if(requireNamespace("BiocNeighbors", quietly = TRUE) && requireNamespace("BiocParallel", quietly = TRUE)){
      return("BiocNeighbors")
    }

    sby_over_under_warn("'sby_knn_workers > 1' foi solicitado, mas BiocNeighbors/BiocParallel nao estao instalados; usando FNN sem paralelismo.")
  }

  "FNN"
}
