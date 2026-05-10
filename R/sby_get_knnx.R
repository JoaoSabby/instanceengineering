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
  sby_hnsw_ef,
  sby_knn_query_chunk_size = getOption("instanceengineering.sby_knn_query_chunk_size", 1000L)
){
  sby_over_under_check_user_interrupt()
  sby_knn_query_chunk_size <- sby_validate_knn_query_chunk_size(sby_knn_query_chunk_size)

  if(identical(sby_knn_backend, "FNN")){
    if(!requireNamespace("FNN", quietly = TRUE)){
      sby_over_under_abort("'sby_knn_backend = FNN' requer o pacote FNN")
    }

    sby_knn_result <- sby_query_knn_in_chunks(
      sby_query = sby_query,
      sby_k = sby_k,
      sby_knn_query_chunk_size = sby_knn_query_chunk_size,
      sby_query_fun = function(sby_query_chunk){
        FNN::get.knnx(
          data = sby_data,
          query = sby_query_chunk,
          k = sby_k,
          algorithm = sby_knn_algorithm
        )
      }
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
    sby_over_under_check_user_interrupt()
    sby_hnsw_query_chunk_size <- sby_validate_knn_query_chunk_size(
      getOption("instanceengineering.sby_hnsw_query_chunk_size", 100L)
    )

    sby_knn_result <- sby_query_knn_in_chunks(
      sby_query = sby_query,
      sby_k = sby_k,
      sby_knn_query_chunk_size = sby_hnsw_query_chunk_size,
      sby_query_fun = function(sby_query_chunk){
        sby_hnsw_result <- RcppHNSW::hnsw_search(
          X = sby_query_chunk,
          ann = sby_hnsw_index,
          k = sby_k,
          ef = sby_effective_ef,
          verbose = FALSE,
          progress = "bar",
          n_threads = sby_knn_workers,
          byrow = TRUE
        )

        list(nn.index = sby_hnsw_result$idx, nn.dist = sby_hnsw_result$dist)
      }
    )

    sby_over_under_check_user_interrupt()

    return(sby_knn_result)
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

#' Executar consultas KNN em blocos interrompiveis
#' @noRd
sby_query_knn_in_chunks <- function(sby_query, sby_k, sby_knn_query_chunk_size, sby_query_fun){
  sby_query_rows <- nrow(sby_query)
  if(sby_query_rows <= sby_knn_query_chunk_size){
    return(sby_query_fun(sby_query))
  }

  sby_nn_index <- matrix(NA_integer_, nrow = sby_query_rows, ncol = sby_k)
  sby_nn_dist <- matrix(NA_real_, nrow = sby_query_rows, ncol = sby_k)
  sby_chunk_starts <- seq.int(1L, sby_query_rows, by = sby_knn_query_chunk_size)

  for(sby_chunk_start in sby_chunk_starts){
    sby_over_under_check_user_interrupt()
    sby_chunk_end <- min(sby_chunk_start + sby_knn_query_chunk_size - 1L, sby_query_rows)
    sby_chunk_index <- seq.int(sby_chunk_start, sby_chunk_end)
    sby_chunk_result <- sby_query_fun(sby_query[sby_chunk_index, , drop = FALSE])
    sby_nn_index[sby_chunk_index, ] <- sby_chunk_result$nn.index
    sby_nn_dist[sby_chunk_index, ] <- sby_chunk_result$nn.dist
    sby_over_under_check_user_interrupt()
  }

  list(nn.index = sby_nn_index, nn.dist = sby_nn_dist)
}
