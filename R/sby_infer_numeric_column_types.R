
#' Inferir tipos numericos para restauracao posterior
#' @noRd
# Executa instrucao do fluxo preservado
sby_infer_numeric_column_types <- function(sby_data_frame) {
  # Executa instrucao do fluxo preservado
  sby_x_matrix <- sby_over_under_as_numeric_matrix(sby_data_frame)
  # Executa instrucao do fluxo preservado
  sby_column_names <- sby_over_under_get_column_names(sby_data_frame)

  # Executa instrucao do fluxo preservado
  sby_infer_one <- function(sby_column_data) {
    # Executa instrucao do fluxo preservado
    sby_unique_values <- sort(unique(sby_column_data))
    # Executa instrucao do fluxo preservado
    if (length(sby_unique_values) <= 2L && all(sby_unique_values %in% c(0, 1))) {
      # Executa instrucao do fluxo preservado
      return("binary")
    # Executa instrucao do fluxo preservado
    }
    # Executa instrucao do fluxo preservado
    sby_is_integer_like <- all(abs(sby_column_data - round(sby_column_data)) < sqrt(.Machine$double.eps))
    # Executa instrucao do fluxo preservado
    if (sby_is_integer_like) {
      # Executa instrucao do fluxo preservado
      return("integer")
    # Executa instrucao do fluxo preservado
    }
    # Executa instrucao do fluxo preservado
    "double"
  # Executa instrucao do fluxo preservado
  }

  # Executa instrucao do fluxo preservado
  data.frame(
    # Executa instrucao do fluxo preservado
    sby_column_name = sby_column_names,
    # Executa instrucao do fluxo preservado
    sby_inferred_type = vapply(seq_len(NCOL(sby_x_matrix)), function(j) sby_infer_one(sby_x_matrix[, j]), character(1L)),
    # Executa instrucao do fluxo preservado
    stringsAsFactors = FALSE
  # Executa instrucao do fluxo preservado
  )
# Executa instrucao do fluxo preservado
}

####
## Fim
#
