
#' Gerar amostras sinteticas ADASYN em matriz ja escalada
#' @noRd
sby_generate_adasyn_samples <- function(sby_x_scaled, sby_target_factor, sby_synthetic_count, sby_k_over, sby_knn_algorithm, sby_knn_backend, sby_knn_workers, sby_bioc_neighbor_algorithm, sby_hnsw_m, sby_hnsw_ef) {
  sby_class_roles <- sby_get_binary_class_roles(sby_target_factor)
  sby_minority_index <- which(sby_target_factor == sby_class_roles$sby_minority_label)
  sby_minority_matrix <- sby_x_scaled[sby_minority_index, , drop = FALSE]

  sby_effective_all_k <- min(as.integer(sby_k_over) + 1L, nrow(sby_x_scaled))
  sby_all_neighbor_result <- sby_get_knnx(
    sby_data = sby_x_scaled,
    sby_query = sby_minority_matrix,
    sby_k = sby_effective_all_k,
    sby_knn_algorithm = sby_knn_algorithm,
    sby_knn_backend = sby_knn_backend,
    sby_knn_workers = sby_knn_workers,
    sby_bioc_neighbor_algorithm = sby_bioc_neighbor_algorithm,
    sby_hnsw_m = sby_hnsw_m,
    sby_hnsw_ef = sby_hnsw_ef
  )
  sby_over_under_check_user_interrupt()

  sby_neighbor_index <- sby_all_neighbor_result$nn.index
  sby_desired_all_k <- min(as.integer(sby_k_over), nrow(sby_x_scaled) - 1L)
  if (sby_effective_all_k > 1L) {
    sby_neighbor_index <- sby_drop_self_neighbor_index(sby_neighbor_index, sby_minority_index, sby_desired_all_k)
  }
  sby_majority_mask <- sby_target_factor[as.vector(sby_neighbor_index)] == sby_class_roles$sby_majority_label
  sby_over_under_check_user_interrupt()
  sby_majority_ratio <- rowMeans(matrix(sby_majority_mask, nrow = nrow(sby_neighbor_index), ncol = ncol(sby_neighbor_index)))
  sby_over_under_check_user_interrupt()
  if (sum(sby_majority_ratio) <= 0) {
    sby_generation_weights <- rep.int(1 / length(sby_minority_index), length(sby_minority_index))
  } else {
    sby_generation_weights <- sby_majority_ratio / sum(sby_majority_ratio)
  }

  sby_raw_counts <- sby_synthetic_count * sby_generation_weights
  sby_synthetic_per_row <- floor(sby_raw_counts)
  sby_remaining <- sby_synthetic_count - sum(sby_synthetic_per_row)
  if (sby_remaining > 0L) {
    sby_fractional_order <- order(sby_raw_counts - sby_synthetic_per_row, decreasing = TRUE)
    sby_synthetic_per_row[sby_fractional_order[seq_len(sby_remaining)]] <- sby_synthetic_per_row[sby_fractional_order[seq_len(sby_remaining)]] + 1L
  }

  sby_effective_minority_k <- min(as.integer(sby_k_over) + 1L, nrow(sby_minority_matrix))
  sby_minority_neighbor_result <- sby_get_knnx(
    sby_data = sby_minority_matrix,
    sby_query = sby_minority_matrix,
    sby_k = sby_effective_minority_k,
    sby_knn_algorithm = sby_knn_algorithm,
    sby_knn_backend = sby_knn_backend,
    sby_knn_workers = sby_knn_workers,
    sby_bioc_neighbor_algorithm = sby_bioc_neighbor_algorithm,
    sby_hnsw_m = sby_hnsw_m,
    sby_hnsw_ef = sby_hnsw_ef
  )
  sby_over_under_check_user_interrupt()

  sby_minority_neighbor_index <- sby_minority_neighbor_result$nn.index
  sby_desired_minority_k <- min(as.integer(sby_k_over), nrow(sby_minority_matrix) - 1L)
  if (sby_effective_minority_k > 1L) {
    sby_minority_neighbor_index <- sby_drop_self_neighbor_index(sby_minority_neighbor_index, seq_len(nrow(sby_minority_matrix)), sby_desired_minority_k)
  }

  if (sby_over_under_native_available()) {
    storage.mode(sby_minority_matrix) <- "double"
    storage.mode(sby_minority_neighbor_index) <- "integer"
    sby_synthetic_matrix <- .Call(
      "OU_GenerateSyntheticAdasynC",
      sby_minority_matrix,
      sby_minority_neighbor_index,
      as.integer(sby_synthetic_per_row),
      PACKAGE = "instanceengineering"
    )
    sby_over_under_check_user_interrupt()
  } else {
    sby_synthetic_matrix <- matrix(0, nrow = sby_synthetic_count, ncol = NCOL(sby_x_scaled))
    sby_write_start <- 1L
    sby_positive_rows <- which(sby_synthetic_per_row > 0L)
    for (i in sby_positive_rows) {
      sby_over_under_check_user_interrupt()
      sby_row_count <- sby_synthetic_per_row[[i]]
      sby_write_end <- sby_write_start + sby_row_count - 1L
      sby_base_rows <- matrix(sby_minority_matrix[i, ], nrow = sby_row_count, ncol = NCOL(sby_x_scaled), byrow = TRUE)
      sby_selected_neighbor_rows <- sby_minority_neighbor_index[i, sample.int(ncol(sby_minority_neighbor_index), sby_row_count, replace = TRUE)]
      sby_neighbor_rows <- sby_minority_matrix[sby_selected_neighbor_rows, , drop = FALSE]
      sby_synthetic_matrix[sby_write_start:sby_write_end, ] <- sby_base_rows + stats::runif (sby_row_count) * (sby_neighbor_rows - sby_base_rows)
      sby_write_start <- sby_write_end + 1L
    }
  }
  colnames(sby_synthetic_matrix) <- colnames(sby_x_scaled)

  list(
    x = rbind(sby_x_scaled, sby_synthetic_matrix),
    y = factor(c(as.character(sby_target_factor), rep.int(sby_class_roles$sby_minority_label, sby_synthetic_count)), levels = levels(sby_target_factor))
  )
}

####
## Fim
#
