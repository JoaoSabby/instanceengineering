
#' Calcular parametros de z-score
#' @noRd
# Executa instrucao do fluxo preservado
sby_compute_z_score_params <- function(sby_x_matrix) {
  # Executa instrucao do fluxo preservado
  sby_x_matrix <- sby_over_under_as_numeric_matrix(sby_x_matrix)

  # Executa instrucao do fluxo preservado
  if (sby_over_under_native_available()) {
    # Executa instrucao do fluxo preservado
    sby_params <- .Call("OU_ComputeZScoreParamsC", sby_x_matrix, PACKAGE = "instanceengineering")
  # Executa instrucao do fluxo preservado
  } else {
    # Executa instrucao do fluxo preservado
    sby_params <- list(
      # Executa instrucao do fluxo preservado
      centers = Rfast::colmeans(sby_x_matrix),
      # Executa instrucao do fluxo preservado
      scales = Rfast::colVars(sby_x_matrix, std = TRUE)
    # Executa instrucao do fluxo preservado
    )
  # Executa instrucao do fluxo preservado
  }

  # Executa instrucao do fluxo preservado
  sby_invalid <- is.na(sby_params$scales) | !is.finite(sby_params$scales) | sby_params$scales <= 0
  # Executa instrucao do fluxo preservado
  if (any(sby_invalid)) {
    # Executa instrucao do fluxo preservado
    sby_column_names <- colnames(sby_x_matrix)
    # Executa instrucao do fluxo preservado
    if (is.null(sby_column_names)) {
      # Executa instrucao do fluxo preservado
      sby_column_names <- paste0("V", seq_len(NCOL(sby_x_matrix)))
    # Executa instrucao do fluxo preservado
    }
    # Executa instrucao do fluxo preservado
    sby_over_under_abort(paste0(
      # Executa instrucao do fluxo preservado
      "Colunas com desvio padrao zero ou indefinido: ",
      # Executa instrucao do fluxo preservado
      paste(sby_column_names[sby_invalid], collapse = ", ")
    # Executa instrucao do fluxo preservado
    ))
  # Executa instrucao do fluxo preservado
  }

  # Executa instrucao do fluxo preservado
  list(centers = as.numeric(sby_params$centers), scales = as.numeric(sby_params$scales))
# Executa instrucao do fluxo preservado
}

####
## Fim
#
