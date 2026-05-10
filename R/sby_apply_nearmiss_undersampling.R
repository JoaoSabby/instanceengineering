
#' Aplicar undersampling com sby_nearmiss
#'
#' @title Aplicar undersampling com sby_nearmiss
#' @name sby_nearmiss
#' @param sby_predictor_data data.frame ou matrix numerica.
#' @param sby_target_vector vetor binario.
#' @param sby_under_ratio fracao da maioria retida.
#' @param sby_k_under numero de vizinhos.
#' @param sby_seed semente.
#' @param sby_audit logical; quando TRUE retorna lista completa de auditoria.
#' @param sby_precomputed_scaling lista opcional com centers e scales.
#' @param sby_input_already_scaled logical indicando se sby_predictor_data ja esta escalado.
#' @param sby_restore_types logical; restaura tipos ao final.
#' @param sby_type_info informacao opcional de tipos original.
#' @param sby_knn_algorithm algoritmo para FNN::get.knnx quando sby_knn_backend = "FNN".
#' @param sby_knn_backend backend KNN: "FNN", "BiocNeighbors", "RcppHNSW" ou "auto".
#' @param sby_knn_workers numero de workers para BiocNeighbors/BiocParallel ou RcppHNSW.
#' @param sby_bioc_neighbor_algorithm algoritmo BiocNeighbors.
#' @param sby_hnsw_m conectividade do indice RcppHNSW.
#' @param sby_hnsw_ef tamanho da lista dinamica de construcao/busca RcppHNSW.
#' @return Tibble balanceado quando sby_audit = FALSE; lista de auditoria quando TRUE.
#' @export
<<<<<<< HEAD
sby_nearmiss <- function(
=======
# Executa instrucao do fluxo preservado
sby_nearmiss <- function(
  # Executa instrucao do fluxo preservado
>>>>>>> origin/main
  sby_predictor_data,
  # Executa instrucao do fluxo preservado
  sby_target_vector,
  # Executa instrucao do fluxo preservado
  sby_under_ratio = 0.5,
  # Executa instrucao do fluxo preservado
  sby_k_under = 5L,
  # Executa instrucao do fluxo preservado
  sby_seed = 42L,
  # Executa instrucao do fluxo preservado
  sby_audit = FALSE,
  # Executa instrucao do fluxo preservado
  sby_precomputed_scaling = NULL,
  # Executa instrucao do fluxo preservado
  sby_input_already_scaled = FALSE,
  # Executa instrucao do fluxo preservado
  sby_restore_types = TRUE,
  # Executa instrucao do fluxo preservado
  sby_type_info = NULL,
  # Executa instrucao do fluxo preservado
  sby_knn_algorithm = c("auto", "cover_tree", "kd_tree", "brute"),
  # Executa instrucao do fluxo preservado
  sby_knn_backend = c("auto", "FNN", "BiocNeighbors", "RcppHNSW"),
  # Executa instrucao do fluxo preservado
  sby_knn_workers = 1L,
  # Executa instrucao do fluxo preservado
  sby_bioc_neighbor_algorithm = c("auto", "Kmknn", "Vptree", "Exhaustive", "Annoy", "Hnsw"),
  # Executa instrucao do fluxo preservado
  sby_hnsw_m = 16L,
  # Executa instrucao do fluxo preservado
  sby_hnsw_ef = 200L
<<<<<<< HEAD
) {
=======
# Executa instrucao do fluxo preservado
) {
  # Executa instrucao do fluxo preservado
>>>>>>> origin/main
  sby_over_under_check_user_interrupt()
  # Executa instrucao do fluxo preservado
  sby_audit <- sby_validate_logical_scalar(sby_audit, "sby_audit")
  # Executa instrucao do fluxo preservado
  sby_input_already_scaled <- sby_validate_logical_scalar(sby_input_already_scaled, "sby_input_already_scaled")
  # Executa instrucao do fluxo preservado
  sby_restore_types <- sby_validate_logical_scalar(sby_restore_types, "sby_restore_types")
  # Executa instrucao do fluxo preservado
  sby_knn_algorithm <- match.arg(sby_knn_algorithm)
  # Executa instrucao do fluxo preservado
  sby_knn_backend <- match.arg(sby_knn_backend)
  # Executa instrucao do fluxo preservado
  sby_bioc_neighbor_algorithm <- match.arg(sby_bioc_neighbor_algorithm)
  # Executa instrucao do fluxo preservado
  sby_knn_workers <- sby_validate_knn_workers(sby_knn_workers)
  # Executa instrucao do fluxo preservado
  sby_hnsw_params <- sby_validate_hnsw_params(sby_hnsw_m, sby_hnsw_ef)
  # Executa instrucao do fluxo preservado
  sby_hnsw_m <- sby_hnsw_params$sby_hnsw_m
  # Executa instrucao do fluxo preservado
  sby_hnsw_ef <- sby_hnsw_params$sby_hnsw_ef
  # Executa instrucao do fluxo preservado
  sby_validate_sampling_inputs(sby_predictor_data, sby_target_vector, sby_seed)

<<<<<<< HEAD
  if (!is.numeric(sby_k_under) || length(sby_k_under) != 1L || is.na(sby_k_under) || sby_k_under < 1L) {
=======
  # Executa instrucao do fluxo preservado
  if (!is.numeric(sby_k_under) || length(sby_k_under) != 1L || is.na(sby_k_under) || sby_k_under < 1L) {
    # Executa instrucao do fluxo preservado
>>>>>>> origin/main
    sby_over_under_abort("'sby_k_under' deve ser inteiro positivo")
  # Executa instrucao do fluxo preservado
  }

  # Executa instrucao do fluxo preservado
  sby_x_matrix <- sby_over_under_as_numeric_matrix(sby_predictor_data)
  # Executa instrucao do fluxo preservado
  colnames(sby_x_matrix) <- sby_over_under_get_column_names(sby_predictor_data)
  # Executa instrucao do fluxo preservado
  sby_knn_algorithm <- sby_resolve_knn_algorithm(sby_knn_algorithm, NCOL(sby_x_matrix))
  # Executa instrucao do fluxo preservado
  sby_knn_backend <- sby_resolve_knn_backend(sby_knn_backend, sby_knn_workers)
  # Executa instrucao do fluxo preservado
  sby_target_factor <- as.factor(sby_target_vector)
<<<<<<< HEAD

  # Infere informacoes de tipo quando nao foram precomputadas
  if (is.null(sby_type_info)) {
    sby_type_info <- sby_infer_numeric_column_types(sby_predictor_data)
  }

  if (NCOL(sby_x_matrix) != nrow(sby_type_info)) {
=======
  # Executa instrucao do fluxo preservado
  sby_type_info <- if (is.null(sby_type_info)) sby_infer_numeric_column_types(sby_predictor_data) else sby_type_info
  # Executa instrucao do fluxo preservado
  if (NCOL(sby_x_matrix) != nrow(sby_type_info)) {
    # Executa instrucao do fluxo preservado
>>>>>>> origin/main
    sby_over_under_abort("'sby_type_info' deve ter uma linha por coluna de 'sby_predictor_data'")
  # Executa instrucao do fluxo preservado
  }

<<<<<<< HEAD
  sby_scaling_info <- if (isTRUE(sby_input_already_scaled)) {
    if (is.null(sby_precomputed_scaling)) {
=======
  # Executa instrucao do fluxo preservado
  sby_scaling_info <- if (isTRUE(sby_input_already_scaled)) {
    # Executa instrucao do fluxo preservado
    if (is.null(sby_precomputed_scaling)) {
      # Executa instrucao do fluxo preservado
>>>>>>> origin/main
      sby_over_under_abort("'sby_precomputed_scaling' e obrigatorio quando 'sby_input_already_scaled = TRUE'")
    # Executa instrucao do fluxo preservado
    }
    # Executa instrucao do fluxo preservado
    sby_precomputed_scaling
<<<<<<< HEAD
  } else if (is.null(sby_precomputed_scaling)) {
=======
  # Executa instrucao do fluxo preservado
  } else if (is.null(sby_precomputed_scaling)) {
    # Executa instrucao do fluxo preservado
>>>>>>> origin/main
    sby_compute_z_score_params(sby_x_matrix)
  # Executa instrucao do fluxo preservado
  } else {
    # Executa instrucao do fluxo preservado
    sby_validate_scaling_info(sby_precomputed_scaling, NCOL(sby_x_matrix))
    # Executa instrucao do fluxo preservado
    sby_precomputed_scaling
  # Executa instrucao do fluxo preservado
  }

<<<<<<< HEAD
  # Reutiliza matriz escalada ou aplica parametros de escala validados
  if (isTRUE(sby_input_already_scaled)) {
    sby_x_scaled <- sby_x_matrix
  } else {
    sby_x_scaled <- sby_apply_z_score_scaling_matrix(sby_x_matrix, sby_scaling_info)
  }

=======
  # Executa instrucao do fluxo preservado
  sby_x_scaled <- if (isTRUE(sby_input_already_scaled)) sby_x_matrix else sby_apply_z_score_scaling_matrix(sby_x_matrix, sby_scaling_info)
  # Executa instrucao do fluxo preservado
>>>>>>> origin/main
  sby_over_under_check_user_interrupt()

  # Executa instrucao do fluxo preservado
  sby_class_counts <- table(sby_target_factor)
<<<<<<< HEAD
  if (sby_class_counts[[1L]] == sby_class_counts[[2L]]) {
=======
  # Executa instrucao do fluxo preservado
  if (sby_class_counts[[1L]] == sby_class_counts[[2L]]) {
    # Executa instrucao do fluxo preservado
>>>>>>> origin/main
    sby_retained_index <- seq_len(nrow(sby_x_scaled))
    # Executa instrucao do fluxo preservado
    sby_reduced_scaled <- sby_x_scaled
    # Executa instrucao do fluxo preservado
    sby_reduced_target <- sby_target_factor
  # Executa instrucao do fluxo preservado
  } else {
    # Executa instrucao do fluxo preservado
    sby_class_roles <- sby_get_binary_class_roles(sby_target_factor)
    # Executa instrucao do fluxo preservado
    sby_minority_index <- which(sby_target_factor == sby_class_roles$sby_minority_label)
    # Executa instrucao do fluxo preservado
    sby_majority_index <- which(sby_target_factor == sby_class_roles$sby_majority_label)

    # Executa instrucao do fluxo preservado
    sby_minority_matrix <- sby_x_scaled[sby_minority_index, , drop = FALSE]
    # Executa instrucao do fluxo preservado
    sby_majority_matrix <- sby_x_scaled[sby_majority_index, , drop = FALSE]

    # Executa instrucao do fluxo preservado
    sby_retained_majority_count <- sby_compute_majority_retention_count(sby_target_factor, sby_under_ratio)
    # Executa instrucao do fluxo preservado
    sby_effective_k <- min(as.integer(sby_k_under), nrow(sby_minority_matrix))
<<<<<<< HEAD
    if (sby_effective_k < 1L) {
=======
    # Executa instrucao do fluxo preservado
    if (sby_effective_k < 1L) {
      # Executa instrucao do fluxo preservado
>>>>>>> origin/main
      sby_over_under_abort("Sem linhas minoritarias suficientes para NearMiss")
    # Executa instrucao do fluxo preservado
    }

    # Executa instrucao do fluxo preservado
    set.seed(sby_seed)
    # Executa instrucao do fluxo preservado
    sby_knn_result <- sby_get_knnx(
      # Executa instrucao do fluxo preservado
      sby_data = sby_minority_matrix,
      # Executa instrucao do fluxo preservado
      sby_query = sby_majority_matrix,
      # Executa instrucao do fluxo preservado
      sby_k = sby_effective_k,
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
    sby_mean_distances <- rowMeans(sby_knn_result$nn.dist)
    # Executa instrucao do fluxo preservado
    sby_over_under_check_user_interrupt()
    # Executa instrucao do fluxo preservado
    sby_selected_order <- order(sby_mean_distances, decreasing = FALSE)
    # Executa instrucao do fluxo preservado
    sby_selected_majority_index <- sby_majority_index[sby_selected_order[seq_len(sby_retained_majority_count)]]
    # Executa instrucao do fluxo preservado
    sby_retained_index <- sort(c(sby_minority_index, sby_selected_majority_index))

    # Executa instrucao do fluxo preservado
    sby_reduced_scaled <- sby_x_scaled[sby_retained_index, , drop = FALSE]
    # Executa instrucao do fluxo preservado
    sby_reduced_target <- sby_target_factor[sby_retained_index]
  # Executa instrucao do fluxo preservado
  }

  # Executa instrucao do fluxo preservado
  sby_x_restored <- sby_revert_z_score_scaling_matrix(sby_reduced_scaled, sby_scaling_info)
<<<<<<< HEAD
  sby_final_predictors <- if (sby_restore_types) {
=======
  # Executa instrucao do fluxo preservado
  sby_final_predictors <- if (sby_restore_types) {
    # Executa instrucao do fluxo preservado
>>>>>>> origin/main
    sby_restore_numeric_column_types(sby_x_restored, sby_type_info, sby_as_data_frame = TRUE)
  # Executa instrucao do fluxo preservado
  } else {
    # Executa instrucao do fluxo preservado
    sby_out <- as.data.frame(sby_x_restored, stringsAsFactors = FALSE)
    # Executa instrucao do fluxo preservado
    names(sby_out) <- sby_type_info$sby_column_name
    # Executa instrucao do fluxo preservado
    sby_out
  # Executa instrucao do fluxo preservado
  }

  # Executa instrucao do fluxo preservado
  sby_balanced_data <- sby_build_balanced_tibble(sby_final_predictors, sby_reduced_target)

<<<<<<< HEAD
  # Define metadado do algoritmo BiocNeighbors usado no diagnostico
  sby_diagnostic_bioc_neighbor_algorithm <- ifelse(
    test = identical(sby_knn_backend, "BiocNeighbors"),
    yes = sby_bioc_neighbor_algorithm,
    no = NA_character_
  )

  # Define metadado de conectividade HNSW usado no diagnostico
  sby_diagnostic_hnsw_m <- ifelse(
    test = identical(sby_knn_backend, "RcppHNSW"),
    yes = sby_hnsw_m,
    no = NA_integer_
  )

  # Define metadado de busca HNSW usado no diagnostico
  sby_diagnostic_hnsw_ef <- ifelse(
    test = identical(sby_knn_backend, "RcppHNSW"),
    yes = sby_hnsw_ef,
    no = NA_integer_
  )

=======
  # Executa instrucao do fluxo preservado
>>>>>>> origin/main
  sby_diagnostics <- list(
    # Executa instrucao do fluxo preservado
    sby_input_rows = nrow(sby_x_matrix),
    # Executa instrucao do fluxo preservado
    sby_output_rows = nrow(sby_balanced_data),
    # Executa instrucao do fluxo preservado
    sby_removed_rows = nrow(sby_x_matrix) - nrow(sby_balanced_data),
    # Executa instrucao do fluxo preservado
    sby_knn_backend = sby_knn_backend,
    # Executa instrucao do fluxo preservado
    sby_knn_workers = sby_knn_workers,
<<<<<<< HEAD
    sby_bioc_neighbor_algorithm = sby_diagnostic_bioc_neighbor_algorithm,
    sby_hnsw_m = sby_diagnostic_hnsw_m,
    sby_hnsw_ef = sby_diagnostic_hnsw_ef,
=======
    # Executa instrucao do fluxo preservado
    sby_bioc_neighbor_algorithm = if (identical(sby_knn_backend, "BiocNeighbors")) sby_bioc_neighbor_algorithm else NA_character_,
    # Executa instrucao do fluxo preservado
    sby_hnsw_m = if (identical(sby_knn_backend, "RcppHNSW")) sby_hnsw_m else NA_integer_,
    # Executa instrucao do fluxo preservado
    sby_hnsw_ef = if (identical(sby_knn_backend, "RcppHNSW")) sby_hnsw_ef else NA_integer_,
    # Executa instrucao do fluxo preservado
>>>>>>> origin/main
    sby_input_class_distribution = table(sby_target_factor),
    # Executa instrucao do fluxo preservado
    sby_output_class_distribution = table(as.factor(sby_reduced_target))
  # Executa instrucao do fluxo preservado
  )

  # Executa instrucao do fluxo preservado
  sby_result <- list(
    # Executa instrucao do fluxo preservado
    sby_balanced_data = sby_balanced_data,
    # Executa instrucao do fluxo preservado
    sby_type_info = sby_type_info,
    # Executa instrucao do fluxo preservado
    sby_scaling_info = sby_scaling_info,
    # Executa instrucao do fluxo preservado
    sby_diagnostics = sby_diagnostics,
    # Executa instrucao do fluxo preservado
    sby_balanced_scaled = list(x = sby_reduced_scaled, y = as.factor(sby_reduced_target))
  # Executa instrucao do fluxo preservado
  )

<<<<<<< HEAD
  if (isTRUE(sby_audit)) {
=======
  # Executa instrucao do fluxo preservado
  if (isTRUE(sby_audit)) {
    # Executa instrucao do fluxo preservado
>>>>>>> origin/main
    return(sby_result)
  # Executa instrucao do fluxo preservado
  }

  # Executa instrucao do fluxo preservado
  sby_balanced_data
# Executa instrucao do fluxo preservado
}

####
## Fim
#
