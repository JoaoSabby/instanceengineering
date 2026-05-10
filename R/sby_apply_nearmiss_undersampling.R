
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
sby_nearmiss <- function(
  sby_predictor_data,
  sby_target_vector,
  sby_under_ratio = 0.5,
  sby_k_under = 5L,
  sby_seed = 42L,
  sby_audit = FALSE,
  sby_precomputed_scaling = NULL,
  sby_input_already_scaled = FALSE,
  sby_restore_types = TRUE,
  sby_type_info = NULL,
  sby_knn_algorithm = c("auto", "cover_tree", "kd_tree", "brute"),
  sby_knn_backend = c("auto", "FNN", "BiocNeighbors", "RcppHNSW"),
  sby_knn_workers = 1L,
  sby_bioc_neighbor_algorithm = c("auto", "Kmknn", "Vptree", "Exhaustive", "Annoy", "Hnsw"),
  sby_hnsw_m = 16L,
  sby_hnsw_ef = 200L
) {
  sby_over_under_check_user_interrupt()
  sby_audit <- sby_validate_logical_scalar(sby_audit, "sby_audit")
  sby_input_already_scaled <- sby_validate_logical_scalar(sby_input_already_scaled, "sby_input_already_scaled")
  sby_restore_types <- sby_validate_logical_scalar(sby_restore_types, "sby_restore_types")
  sby_knn_algorithm <- match.arg(sby_knn_algorithm)
  sby_knn_backend <- match.arg(sby_knn_backend)
  sby_bioc_neighbor_algorithm <- match.arg(sby_bioc_neighbor_algorithm)
  sby_knn_workers <- sby_validate_knn_workers(sby_knn_workers)
  sby_hnsw_params <- sby_validate_hnsw_params(sby_hnsw_m, sby_hnsw_ef)
  sby_hnsw_m <- sby_hnsw_params$sby_hnsw_m
  sby_hnsw_ef <- sby_hnsw_params$sby_hnsw_ef
  sby_validate_sampling_inputs(sby_predictor_data, sby_target_vector, sby_seed)

  if (!is.numeric(sby_k_under) || length(sby_k_under) != 1L || is.na(sby_k_under) || sby_k_under < 1L) {
    sby_over_under_abort("'sby_k_under' deve ser inteiro positivo")
  }

  sby_x_matrix <- sby_over_under_as_numeric_matrix(sby_predictor_data)
  colnames(sby_x_matrix) <- sby_over_under_get_column_names(sby_predictor_data)
  sby_knn_algorithm <- sby_resolve_knn_algorithm(sby_knn_algorithm, NCOL(sby_x_matrix))
  sby_knn_backend <- sby_resolve_knn_backend(sby_knn_backend, sby_knn_workers)
  sby_target_factor <- as.factor(sby_target_vector)

  # Infere informacoes de tipo quando nao foram precomputadas
  if (is.null(sby_type_info)) {
    sby_type_info <- sby_infer_numeric_column_types(sby_predictor_data)
  }

  if (NCOL(sby_x_matrix) != nrow(sby_type_info)) {
    sby_over_under_abort("'sby_type_info' deve ter uma linha por coluna de 'sby_predictor_data'")
  }

  sby_scaling_info <- if (isTRUE(sby_input_already_scaled)) {
    if (is.null(sby_precomputed_scaling)) {
      sby_over_under_abort("'sby_precomputed_scaling' e obrigatorio quando 'sby_input_already_scaled = TRUE'")
    }
    sby_precomputed_scaling
  } else if (is.null(sby_precomputed_scaling)) {
    sby_compute_z_score_params(sby_x_matrix)
  } else {
    sby_validate_scaling_info(sby_precomputed_scaling, NCOL(sby_x_matrix))
    sby_precomputed_scaling
  }

  # Reutiliza matriz escalada ou aplica parametros de escala validados
  if (isTRUE(sby_input_already_scaled)) {
    sby_x_scaled <- sby_x_matrix
  } else {
    sby_x_scaled <- sby_apply_z_score_scaling_matrix(sby_x_matrix, sby_scaling_info)
  }

  sby_over_under_check_user_interrupt()

  sby_class_counts <- table(sby_target_factor)
  if (sby_class_counts[[1L]] == sby_class_counts[[2L]]) {
    sby_retained_index <- seq_len(nrow(sby_x_scaled))
    sby_reduced_scaled <- sby_x_scaled
    sby_reduced_target <- sby_target_factor
  } else {
    sby_class_roles <- sby_get_binary_class_roles(sby_target_factor)
    sby_minority_index <- which(sby_target_factor == sby_class_roles$sby_minority_label)
    sby_majority_index <- which(sby_target_factor == sby_class_roles$sby_majority_label)

    sby_minority_matrix <- sby_x_scaled[sby_minority_index, , drop = FALSE]
    sby_majority_matrix <- sby_x_scaled[sby_majority_index, , drop = FALSE]

    sby_retained_majority_count <- sby_compute_majority_retention_count(sby_target_factor, sby_under_ratio)
    sby_effective_k <- min(as.integer(sby_k_under), nrow(sby_minority_matrix))
    if (sby_effective_k < 1L) {
      sby_over_under_abort("Sem linhas minoritarias suficientes para NearMiss")
    }

    set.seed(sby_seed)
    sby_knn_result <- sby_get_knnx(
      sby_data = sby_minority_matrix,
      sby_query = sby_majority_matrix,
      sby_k = sby_effective_k,
      sby_knn_algorithm = sby_knn_algorithm,
      sby_knn_backend = sby_knn_backend,
      sby_knn_workers = sby_knn_workers,
      sby_bioc_neighbor_algorithm = sby_bioc_neighbor_algorithm,
      sby_hnsw_m = sby_hnsw_m,
      sby_hnsw_ef = sby_hnsw_ef
    )
    sby_over_under_check_user_interrupt()

    sby_mean_distances <- rowMeans(sby_knn_result$nn.dist)
    sby_over_under_check_user_interrupt()
    sby_selected_order <- order(sby_mean_distances, decreasing = FALSE)
    sby_selected_majority_index <- sby_majority_index[sby_selected_order[seq_len(sby_retained_majority_count)]]
    sby_retained_index <- sort(c(sby_minority_index, sby_selected_majority_index))

    sby_reduced_scaled <- sby_x_scaled[sby_retained_index, , drop = FALSE]
    sby_reduced_target <- sby_target_factor[sby_retained_index]
  }

  sby_x_restored <- sby_revert_z_score_scaling_matrix(sby_reduced_scaled, sby_scaling_info)
  sby_final_predictors <- if (sby_restore_types) {
    sby_restore_numeric_column_types(sby_x_restored, sby_type_info, sby_as_data_frame = TRUE)
  } else {
    sby_out <- as.data.frame(sby_x_restored, stringsAsFactors = FALSE)
    names(sby_out) <- sby_type_info$sby_column_name
    sby_out
  }

  sby_balanced_data <- sby_build_balanced_tibble(sby_final_predictors, sby_reduced_target)

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

  sby_diagnostics <- list(
    sby_input_rows = nrow(sby_x_matrix),
    sby_output_rows = nrow(sby_balanced_data),
    sby_removed_rows = nrow(sby_x_matrix) - nrow(sby_balanced_data),
    sby_knn_backend = sby_knn_backend,
    sby_knn_workers = sby_knn_workers,
    sby_bioc_neighbor_algorithm = sby_diagnostic_bioc_neighbor_algorithm,
    sby_hnsw_m = sby_diagnostic_hnsw_m,
    sby_hnsw_ef = sby_diagnostic_hnsw_ef,
    sby_input_class_distribution = table(sby_target_factor),
    sby_output_class_distribution = table(as.factor(sby_reduced_target))
  )

  sby_result <- list(
    sby_balanced_data = sby_balanced_data,
    sby_type_info = sby_type_info,
    sby_scaling_info = sby_scaling_info,
    sby_diagnostics = sby_diagnostics,
    sby_balanced_scaled = list(x = sby_reduced_scaled, y = as.factor(sby_reduced_target))
  )

  if (isTRUE(sby_audit)) {
    return(sby_result)
  }

  sby_balanced_data
}

####
## Fim
#
