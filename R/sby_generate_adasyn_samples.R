
#' Gerar amostras sinteticas ADASYN em matriz ja escalada
#' @noRd
<<<<<<< HEAD
sby_generate_adasyn_samples <- function(sby_x_scaled, sby_target_factor, sby_synthetic_count, sby_k_over, sby_knn_algorithm, sby_knn_backend, sby_knn_workers, sby_bioc_neighbor_algorithm, sby_hnsw_m, sby_hnsw_ef) {
=======
# Executa instrucao do fluxo preservado
sby_generate_adasyn_samples <- function(sby_x_scaled, sby_target_factor, sby_synthetic_count, sby_k_over, sby_knn_algorithm, sby_knn_backend, sby_knn_workers, sby_bioc_neighbor_algorithm, sby_hnsw_m, sby_hnsw_ef) {
  # Executa instrucao do fluxo preservado
>>>>>>> origin/main
  sby_class_roles <- sby_get_binary_class_roles(sby_target_factor)
  # Executa instrucao do fluxo preservado
  sby_minority_index <- which(sby_target_factor == sby_class_roles$sby_minority_label)
  # Executa instrucao do fluxo preservado
  sby_minority_matrix <- sby_x_scaled[sby_minority_index, , drop = FALSE]

  # Executa instrucao do fluxo preservado
  sby_effective_all_k <- min(as.integer(sby_k_over) + 1L, nrow(sby_x_scaled))
  # Executa instrucao do fluxo preservado
  sby_all_neighbor_result <- sby_get_knnx(
    # Executa instrucao do fluxo preservado
    sby_data = sby_x_scaled,
    # Executa instrucao do fluxo preservado
    sby_query = sby_minority_matrix,
    # Executa instrucao do fluxo preservado
    sby_k = sby_effective_all_k,
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
    sby_hnsw_ef = sby_hnsw_ef
  # Executa instrucao do fluxo preservado
  )
  # Executa instrucao do fluxo preservado
  sby_over_under_check_user_interrupt()

  # Executa instrucao do fluxo preservado
  sby_neighbor_index <- sby_all_neighbor_result$nn.index
  # Executa instrucao do fluxo preservado
  sby_desired_all_k <- min(as.integer(sby_k_over), nrow(sby_x_scaled) - 1L)
<<<<<<< HEAD
  if (sby_effective_all_k > 1L) {
=======
  # Executa instrucao do fluxo preservado
  if (sby_effective_all_k > 1L) {
    # Executa instrucao do fluxo preservado
>>>>>>> origin/main
    sby_neighbor_index <- sby_drop_self_neighbor_index(sby_neighbor_index, sby_minority_index, sby_desired_all_k)
  # Executa instrucao do fluxo preservado
  }
  # Executa instrucao do fluxo preservado
  sby_majority_mask <- sby_target_factor[as.vector(sby_neighbor_index)] == sby_class_roles$sby_majority_label
  # Executa instrucao do fluxo preservado
  sby_over_under_check_user_interrupt()
  # Executa instrucao do fluxo preservado
  sby_majority_ratio <- rowMeans(matrix(sby_majority_mask, nrow = nrow(sby_neighbor_index), ncol = ncol(sby_neighbor_index)))
  # Executa instrucao do fluxo preservado
  sby_over_under_check_user_interrupt()
<<<<<<< HEAD
  if (sum(sby_majority_ratio) <= 0) {
=======
  # Executa instrucao do fluxo preservado
  if (sum(sby_majority_ratio) <= 0) {
    # Executa instrucao do fluxo preservado
>>>>>>> origin/main
    sby_generation_weights <- rep.int(1 / length(sby_minority_index), length(sby_minority_index))
  # Executa instrucao do fluxo preservado
  } else {
    # Executa instrucao do fluxo preservado
    sby_generation_weights <- sby_majority_ratio / sum(sby_majority_ratio)
  # Executa instrucao do fluxo preservado
  }

  # Executa instrucao do fluxo preservado
  sby_raw_counts <- sby_synthetic_count * sby_generation_weights
  # Executa instrucao do fluxo preservado
  sby_synthetic_per_row <- floor(sby_raw_counts)
  # Executa instrucao do fluxo preservado
  sby_remaining <- sby_synthetic_count - sum(sby_synthetic_per_row)
<<<<<<< HEAD
  if (sby_remaining > 0L) {
=======
  # Executa instrucao do fluxo preservado
  if (sby_remaining > 0L) {
    # Executa instrucao do fluxo preservado
>>>>>>> origin/main
    sby_fractional_order <- order(sby_raw_counts - sby_synthetic_per_row, decreasing = TRUE)
    # Executa instrucao do fluxo preservado
    sby_synthetic_per_row[sby_fractional_order[seq_len(sby_remaining)]] <- sby_synthetic_per_row[sby_fractional_order[seq_len(sby_remaining)]] + 1L
  # Executa instrucao do fluxo preservado
  }

  # Executa instrucao do fluxo preservado
  sby_effective_minority_k <- min(as.integer(sby_k_over) + 1L, nrow(sby_minority_matrix))
  # Executa instrucao do fluxo preservado
  sby_minority_neighbor_result <- sby_get_knnx(
    # Executa instrucao do fluxo preservado
    sby_data = sby_minority_matrix,
    # Executa instrucao do fluxo preservado
    sby_query = sby_minority_matrix,
    # Executa instrucao do fluxo preservado
    sby_k = sby_effective_minority_k,
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
    sby_hnsw_ef = sby_hnsw_ef
  # Executa instrucao do fluxo preservado
  )
  # Executa instrucao do fluxo preservado
  sby_over_under_check_user_interrupt()

  # Executa instrucao do fluxo preservado
  sby_minority_neighbor_index <- sby_minority_neighbor_result$nn.index
  # Executa instrucao do fluxo preservado
  sby_desired_minority_k <- min(as.integer(sby_k_over), nrow(sby_minority_matrix) - 1L)
<<<<<<< HEAD
  if (sby_effective_minority_k > 1L) {
=======
  # Executa instrucao do fluxo preservado
  if (sby_effective_minority_k > 1L) {
    # Executa instrucao do fluxo preservado
>>>>>>> origin/main
    sby_minority_neighbor_index <- sby_drop_self_neighbor_index(sby_minority_neighbor_index, seq_len(nrow(sby_minority_matrix)), sby_desired_minority_k)
  # Executa instrucao do fluxo preservado
  }

<<<<<<< HEAD
  if (sby_over_under_native_available()) {
=======
  # Executa instrucao do fluxo preservado
  if (sby_over_under_native_available()) {
    # Executa instrucao do fluxo preservado
>>>>>>> origin/main
    storage.mode(sby_minority_matrix) <- "double"
    # Executa instrucao do fluxo preservado
    storage.mode(sby_minority_neighbor_index) <- "integer"
    # Executa instrucao do fluxo preservado
    sby_synthetic_matrix <- .Call(
      # Executa instrucao do fluxo preservado
      "OU_GenerateSyntheticAdasynC",
      # Executa instrucao do fluxo preservado
      sby_minority_matrix,
      # Executa instrucao do fluxo preservado
      sby_minority_neighbor_index,
      # Executa instrucao do fluxo preservado
      as.integer(sby_synthetic_per_row),
      # Executa instrucao do fluxo preservado
      PACKAGE = "instanceengineering"
    # Executa instrucao do fluxo preservado
    )
    # Executa instrucao do fluxo preservado
    sby_over_under_check_user_interrupt()
  # Executa instrucao do fluxo preservado
  } else {
    # Executa instrucao do fluxo preservado
    sby_synthetic_matrix <- matrix(0, nrow = sby_synthetic_count, ncol = NCOL(sby_x_scaled))
    # Executa instrucao do fluxo preservado
    sby_write_start <- 1L
    # Executa instrucao do fluxo preservado
    sby_positive_rows <- which(sby_synthetic_per_row > 0L)
<<<<<<< HEAD
    for (i in sby_positive_rows) {
=======
    # Executa instrucao do fluxo preservado
    for (i in sby_positive_rows) {
      # Executa instrucao do fluxo preservado
>>>>>>> origin/main
      sby_over_under_check_user_interrupt()
      # Executa instrucao do fluxo preservado
      sby_row_count <- sby_synthetic_per_row[[i]]
      # Executa instrucao do fluxo preservado
      sby_write_end <- sby_write_start + sby_row_count - 1L
      # Executa instrucao do fluxo preservado
      sby_base_rows <- matrix(sby_minority_matrix[i, ], nrow = sby_row_count, ncol = NCOL(sby_x_scaled), byrow = TRUE)
      # Executa instrucao do fluxo preservado
      sby_selected_neighbor_rows <- sby_minority_neighbor_index[i, sample.int(ncol(sby_minority_neighbor_index), sby_row_count, replace = TRUE)]
      # Executa instrucao do fluxo preservado
      sby_neighbor_rows <- sby_minority_matrix[sby_selected_neighbor_rows, , drop = FALSE]
<<<<<<< HEAD
      sby_synthetic_matrix[sby_write_start:sby_write_end, ] <- sby_base_rows + stats::runif (sby_row_count) * (sby_neighbor_rows - sby_base_rows)
=======
      # Executa instrucao do fluxo preservado
      sby_synthetic_matrix[sby_write_start:sby_write_end, ] <- sby_base_rows + stats::runif (sby_row_count) * (sby_neighbor_rows - sby_base_rows)
      # Executa instrucao do fluxo preservado
>>>>>>> origin/main
      sby_write_start <- sby_write_end + 1L
    # Executa instrucao do fluxo preservado
    }
  # Executa instrucao do fluxo preservado
  }
  # Executa instrucao do fluxo preservado
  colnames(sby_synthetic_matrix) <- colnames(sby_x_scaled)

  # Executa instrucao do fluxo preservado
  list(
    # Executa instrucao do fluxo preservado
    x = rbind(sby_x_scaled, sby_synthetic_matrix),
    # Executa instrucao do fluxo preservado
    y = factor(c(as.character(sby_target_factor), rep.int(sby_class_roles$sby_minority_label, sby_synthetic_count)), levels = levels(sby_target_factor))
  # Executa instrucao do fluxo preservado
  )
# Executa instrucao do fluxo preservado
}

####
## Fim
#
