#' Executar consulta KNN usando FNN ou BiocNeighbors/BiocParallel
#' @noRd
# Executa instrucao do fluxo preservado
sby_get_knnx <- function(
  # Executa instrucao do fluxo preservado
  sby_data,
  # Executa instrucao do fluxo preservado
  sby_query,
  # Executa instrucao do fluxo preservado
  sby_k,
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
  sby_knn_query_chunk_size = getOption("instanceengineering.sby_knn_query_chunk_size", 1000L)
# Executa instrucao do fluxo preservado
) {
  # Executa instrucao do fluxo preservado
  sby_over_under_check_user_interrupt()
  # Executa instrucao do fluxo preservado
  sby_knn_query_chunk_size <- sby_validate_knn_query_chunk_size(sby_knn_query_chunk_size)

  # Executa instrucao do fluxo preservado
  if (identical(sby_knn_backend, "FNN")) {
    # Executa instrucao do fluxo preservado
    if (!requireNamespace("FNN", quietly = TRUE)) {
      # Executa instrucao do fluxo preservado
      sby_over_under_abort("'sby_knn_backend = FNN' requer o pacote FNN")
    # Executa instrucao do fluxo preservado
    }

    # Executa instrucao do fluxo preservado
    sby_knn_result <- sby_query_knn_in_chunks(
      # Executa instrucao do fluxo preservado
      sby_query = sby_query,
      # Executa instrucao do fluxo preservado
      sby_k = sby_k,
      # Executa instrucao do fluxo preservado
      sby_knn_query_chunk_size = sby_knn_query_chunk_size,
      # Executa instrucao do fluxo preservado
      sby_query_fun = function(sby_query_chunk) {
        # Executa instrucao do fluxo preservado
        FNN::get.knnx(
          # Executa instrucao do fluxo preservado
          data = sby_data,
          # Executa instrucao do fluxo preservado
          query = sby_query_chunk,
          # Executa instrucao do fluxo preservado
          k = sby_k,
          # Executa instrucao do fluxo preservado
          algorithm = sby_knn_algorithm
        # Executa instrucao do fluxo preservado
        )
      # Executa instrucao do fluxo preservado
      }
    # Executa instrucao do fluxo preservado
    )
    # Executa instrucao do fluxo preservado
    sby_over_under_check_user_interrupt()

    # Executa instrucao do fluxo preservado
    return(sby_knn_result)
  # Executa instrucao do fluxo preservado
  }

  # Executa instrucao do fluxo preservado
  if (identical(sby_knn_backend, "RcppHNSW")) {
    # Executa instrucao do fluxo preservado
    if (!requireNamespace("RcppHNSW", quietly = TRUE)) {
      # Executa instrucao do fluxo preservado
      sby_over_under_abort("'sby_knn_backend = RcppHNSW' requer o pacote RcppHNSW. Instale-o com install.packages('RcppHNSW').")
    # Executa instrucao do fluxo preservado
    }

    # Executa instrucao do fluxo preservado
    sby_effective_ef <- min(max(as.integer(sby_hnsw_ef), as.integer(sby_k)), nrow(sby_data))
    # Executa instrucao do fluxo preservado
    sby_hnsw_index <- RcppHNSW::hnsw_build(
      # Executa instrucao do fluxo preservado
      X = sby_data,
      # Executa instrucao do fluxo preservado
      distance = "euclidean",
      # Executa instrucao do fluxo preservado
      M = as.integer(sby_hnsw_m),
      # Executa instrucao do fluxo preservado
      ef = sby_effective_ef,
      # Executa instrucao do fluxo preservado
      verbose = FALSE,
      # Executa instrucao do fluxo preservado
      progress = "bar",
      # Executa instrucao do fluxo preservado
      n_threads = sby_knn_workers,
      # Executa instrucao do fluxo preservado
      byrow = TRUE
    # Executa instrucao do fluxo preservado
    )
    # Executa instrucao do fluxo preservado
    sby_over_under_check_user_interrupt()
    # Executa instrucao do fluxo preservado
    sby_hnsw_query_chunk_size <- sby_validate_knn_query_chunk_size(
      # Executa instrucao do fluxo preservado
      getOption("instanceengineering.sby_hnsw_query_chunk_size", 100L)
    # Executa instrucao do fluxo preservado
    )

    # Executa instrucao do fluxo preservado
    sby_knn_result <- sby_query_knn_in_chunks(
      # Executa instrucao do fluxo preservado
      sby_query = sby_query,
      # Executa instrucao do fluxo preservado
      sby_k = sby_k,
      # Executa instrucao do fluxo preservado
      sby_knn_query_chunk_size = sby_hnsw_query_chunk_size,
      # Executa instrucao do fluxo preservado
      sby_query_fun = function(sby_query_chunk) {
        # Executa instrucao do fluxo preservado
        sby_hnsw_result <- RcppHNSW::hnsw_search(
          # Executa instrucao do fluxo preservado
          X = sby_query_chunk,
          # Executa instrucao do fluxo preservado
          ann = sby_hnsw_index,
          # Executa instrucao do fluxo preservado
          k = sby_k,
          # Executa instrucao do fluxo preservado
          ef = sby_effective_ef,
          # Executa instrucao do fluxo preservado
          verbose = FALSE,
          # Executa instrucao do fluxo preservado
          progress = "bar",
          # Executa instrucao do fluxo preservado
          n_threads = sby_knn_workers,
          # Executa instrucao do fluxo preservado
          byrow = TRUE
        # Executa instrucao do fluxo preservado
        )

        # Executa instrucao do fluxo preservado
        list(nn.index = sby_hnsw_result$idx, nn.dist = sby_hnsw_result$dist)
      # Executa instrucao do fluxo preservado
      }
    # Executa instrucao do fluxo preservado
    )

    # Executa instrucao do fluxo preservado
    sby_over_under_check_user_interrupt()

    # Executa instrucao do fluxo preservado
    return(sby_knn_result)
  # Executa instrucao do fluxo preservado
  }

  # Executa instrucao do fluxo preservado
  if (!requireNamespace("BiocNeighbors", quietly = TRUE) || !requireNamespace("BiocParallel", quietly = TRUE)) {
    # Executa instrucao do fluxo preservado
    sby_over_under_abort("'sby_knn_backend = BiocNeighbors' requer os pacotes BiocNeighbors e BiocParallel. Instale-os com BiocManager::install(c('BiocNeighbors', 'BiocParallel')).")
  # Executa instrucao do fluxo preservado
  }

  # Executa instrucao do fluxo preservado
  sby_neighbor_param <- sby_create_bioc_neighbor_param(sby_bioc_neighbor_algorithm, NCOL(sby_data))
  # Executa instrucao do fluxo preservado
  sby_parallel_param <- sby_create_knn_bioc_parallel_param(sby_knn_workers)
  # Executa instrucao do fluxo preservado
  sby_knn_result <- BiocNeighbors::queryKNN(
    # Executa instrucao do fluxo preservado
    X = sby_data,
    # Executa instrucao do fluxo preservado
    query = sby_query,
    # Executa instrucao do fluxo preservado
    k = sby_k,
    # Executa instrucao do fluxo preservado
    BNPARAM = sby_neighbor_param,
    # Executa instrucao do fluxo preservado
    BPPARAM = sby_parallel_param
  # Executa instrucao do fluxo preservado
  )

  # Executa instrucao do fluxo preservado
  sby_over_under_check_user_interrupt()

  # Executa instrucao do fluxo preservado
  list(nn.index = sby_knn_result$index, nn.dist = sby_knn_result$distance)
# Executa instrucao do fluxo preservado
}

#' Executar consultas KNN em blocos interrompiveis
#' @noRd
# Executa instrucao do fluxo preservado
sby_query_knn_in_chunks <- function(sby_query, sby_k, sby_knn_query_chunk_size, sby_query_fun) {
  # Executa instrucao do fluxo preservado
  sby_query_rows <- nrow(sby_query)
  # Executa instrucao do fluxo preservado
  if (sby_query_rows <= sby_knn_query_chunk_size) {
    # Executa instrucao do fluxo preservado
    return(sby_query_fun(sby_query))
  # Executa instrucao do fluxo preservado
  }

  # Executa instrucao do fluxo preservado
  sby_nn_index <- matrix(NA_integer_, nrow = sby_query_rows, ncol = sby_k)
  # Executa instrucao do fluxo preservado
  sby_nn_dist <- matrix(NA_real_, nrow = sby_query_rows, ncol = sby_k)
  # Executa instrucao do fluxo preservado
  sby_chunk_starts <- seq.int(1L, sby_query_rows, by = sby_knn_query_chunk_size)

  # Executa instrucao do fluxo preservado
  for (sby_chunk_start in sby_chunk_starts) {
    # Executa instrucao do fluxo preservado
    sby_over_under_check_user_interrupt()
    # Executa instrucao do fluxo preservado
    sby_chunk_end <- min(sby_chunk_start + sby_knn_query_chunk_size - 1L, sby_query_rows)
    # Executa instrucao do fluxo preservado
    sby_chunk_index <- seq.int(sby_chunk_start, sby_chunk_end)
    # Executa instrucao do fluxo preservado
    sby_chunk_result <- sby_query_fun(sby_query[sby_chunk_index, , drop = FALSE])
    # Executa instrucao do fluxo preservado
    sby_nn_index[sby_chunk_index, ] <- sby_chunk_result$nn.index
    # Executa instrucao do fluxo preservado
    sby_nn_dist[sby_chunk_index, ] <- sby_chunk_result$nn.dist
    # Executa instrucao do fluxo preservado
    sby_over_under_check_user_interrupt()
  # Executa instrucao do fluxo preservado
  }

  # Executa instrucao do fluxo preservado
  list(nn.index = sby_nn_index, nn.dist = sby_nn_dist)
# Executa instrucao do fluxo preservado
}

####
## Fim
#
