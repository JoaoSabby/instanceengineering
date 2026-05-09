
#' Executar consulta KNN usando FNN ou BiocNeighbors/BiocParallel
#' @noRd
sby_get_knnx <- function(
  sby_data,
  sby_query,
  sby_k,
  sby_knn_algorithm,
  sby_knn_backend,
  sby_knn_workers,
  sby_bioc_neighbor_algorithm,
  sby_hnsw_m,
  sby_hnsw_ef
){
  sby_over_under_check_user_interrupt()

  if(identical(sby_knn_backend, "FNN")){
    if(!requireNamespace("FNN", quietly = TRUE)){
      sby_over_under_abort("'sby_knn_backend = FNN' requer o pacote FNN")
    }

    sby_knn_result <- FNN::get.knnx(
      data = sby_data,
      query = sby_query,
      k = sby_k,
      algorithm = sby_knn_algorithm
    )
    sby_over_under_check_user_interrupt()

    return(sby_knn_result)
  }

  if(identical(sby_knn_backend, "RcppHNSW")){
    if(!requireNamespace("RcppHNSW", quietly = TRUE)){
      sby_over_under_abort("'sby_knn_backend = RcppHNSW' requer o pacote RcppHNSW. Instale-o com install.packages('RcppHNSW').")
    }

    sby_effective_ef <- min(max(as.integer(sby_hnsw_ef), as.integer(sby_k)), nrow(sby_data))
    sby_hnsw_index <- RcppHNSW::hnsw_build(
      X = sby_data,
      distance = "euclidean",
      M = as.integer(sby_hnsw_m),
      ef = sby_effective_ef,
      verbose = FALSE,
      progress = "bar",
      n_threads = sby_knn_workers,
      byrow = TRUE
    )
    sby_knn_result <- RcppHNSW::hnsw_search(
      X = sby_query,
      ann = sby_hnsw_index,
      k = sby_k,
      ef = sby_effective_ef,
      verbose = FALSE,
      progress = "bar",
      n_threads = sby_knn_workers,
      byrow = TRUE
    )

    sby_over_under_check_user_interrupt()

    return(list(nn.index = sby_knn_result$idx, nn.dist = sby_knn_result$dist))
  }

  if(!requireNamespace("BiocNeighbors", quietly = TRUE) || !requireNamespace("BiocParallel", quietly = TRUE)){
    sby_over_under_abort("'sby_knn_backend = BiocNeighbors' requer os pacotes BiocNeighbors e BiocParallel. Instale-os com BiocManager::install(c('BiocNeighbors', 'BiocParallel')).")
  }

  sby_neighbor_param <- sby_create_bioc_neighbor_param(sby_bioc_neighbor_algorithm, NCOL(sby_data))
  sby_parallel_param <- sby_create_knn_bioc_parallel_param(sby_knn_workers)
  sby_knn_result <- BiocNeighbors::queryKNN(
    X = sby_data,
    query = sby_query,
    k = sby_k,
    BNPARAM = sby_neighbor_param,
    BPPARAM = sby_parallel_param
  )

  sby_over_under_check_user_interrupt()

  list(nn.index = sby_knn_result$index, nn.dist = sby_knn_result$distance)
}
