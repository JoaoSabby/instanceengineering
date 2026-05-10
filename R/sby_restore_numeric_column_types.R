
#' Restaurar tipos numericos inferidos
#' @noRd
# Executa instrucao do fluxo preservado
sby_restore_numeric_column_types <- function(sby_x_matrix, sby_type_info, sby_as_data_frame = TRUE) {
  # Executa instrucao do fluxo preservado
  sby_x_matrix <- sby_over_under_as_numeric_matrix(sby_x_matrix)
  # Executa instrucao do fluxo preservado
  if (NCOL(sby_x_matrix) != nrow(sby_type_info)) {
    # Executa instrucao do fluxo preservado
    sby_over_under_abort("Inconsistencia entre numero de colunas e sby_type_info")
  # Executa instrucao do fluxo preservado
  }

  # Executa instrucao do fluxo preservado
  for (j in seq_len(NCOL(sby_x_matrix))) {
    # Executa instrucao do fluxo preservado
    if (j %% 64L == 1L) {
      # Executa instrucao do fluxo preservado
      sby_over_under_check_user_interrupt()
    # Executa instrucao do fluxo preservado
    }
    # Executa instrucao do fluxo preservado
    sby_inferred_type <- sby_type_info$sby_inferred_type[[j]]
    # Executa instrucao do fluxo preservado
    if (identical(sby_inferred_type, "binary")) {
      # Executa instrucao do fluxo preservado
      sby_x_matrix[, j] <- ifelse(sby_x_matrix[, j] >= 0.5, 1, 0)
    # Executa instrucao do fluxo preservado
    } else if (identical(sby_inferred_type, "integer")) {
      # Executa instrucao do fluxo preservado
      sby_x_matrix[, j] <- round(sby_x_matrix[, j])
    # Executa instrucao do fluxo preservado
    }
  # Executa instrucao do fluxo preservado
  }

  # Executa instrucao do fluxo preservado
  if (!sby_as_data_frame) {
    # Executa instrucao do fluxo preservado
    return(sby_x_matrix)
  # Executa instrucao do fluxo preservado
  }

  # Executa instrucao do fluxo preservado
  sby_out <- as.data.frame(sby_x_matrix, stringsAsFactors = FALSE)
  # Executa instrucao do fluxo preservado
  names(sby_out) <- sby_type_info$sby_column_name

  # Executa instrucao do fluxo preservado
  for (j in seq_len(NCOL(sby_x_matrix))) {
    # Executa instrucao do fluxo preservado
    if (j %% 64L == 1L) {
      # Executa instrucao do fluxo preservado
      sby_over_under_check_user_interrupt()
    # Executa instrucao do fluxo preservado
    }
    # Executa instrucao do fluxo preservado
    sby_inferred_type <- sby_type_info$sby_inferred_type[[j]]
    # Executa instrucao do fluxo preservado
    if (identical(sby_inferred_type, "binary") || identical(sby_inferred_type, "integer")) {
      # Executa instrucao do fluxo preservado
      sby_out[[j]] <- as.integer(sby_out[[j]])
    # Executa instrucao do fluxo preservado
    } else {
      # Executa instrucao do fluxo preservado
      sby_out[[j]] <- as.numeric(sby_out[[j]])
    # Executa instrucao do fluxo preservado
    }
  # Executa instrucao do fluxo preservado
  }

  # Executa instrucao do fluxo preservado
  sby_out
# Executa instrucao do fluxo preservado
}

####
## Fim
#
