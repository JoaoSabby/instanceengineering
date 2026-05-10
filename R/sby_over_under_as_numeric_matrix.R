
#' Converter preditores para matrix double sem copias redundantes desnecessarias
#' @noRd
# Executa instrucao do fluxo preservado
sby_over_under_as_numeric_matrix <- function(sby_predictor_data) {
  # Executa instrucao do fluxo preservado
  if (is.matrix(sby_predictor_data)) {
    # Executa instrucao do fluxo preservado
    sby_x_matrix <- sby_predictor_data
    # Executa instrucao do fluxo preservado
    storage.mode(sby_x_matrix) <- "double"
    # Executa instrucao do fluxo preservado
    return(sby_x_matrix)
  # Executa instrucao do fluxo preservado
  }

  # Executa instrucao do fluxo preservado
  sby_x_matrix <- data.matrix(sby_predictor_data)
  # Executa instrucao do fluxo preservado
  storage.mode(sby_x_matrix) <- "double"
  # Executa instrucao do fluxo preservado
  sby_x_matrix
# Executa instrucao do fluxo preservado
}

####
## Fim
#
