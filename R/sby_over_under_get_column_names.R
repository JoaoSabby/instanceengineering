
#' Extrair nomes de colunas de forma segura
#' @noRd
# Executa instrucao do fluxo preservado
sby_over_under_get_column_names <- function(sby_predictor_data) {
  # Executa instrucao do fluxo preservado
  sby_column_names <- colnames(sby_predictor_data)
  # Executa instrucao do fluxo preservado
  if (is.null(sby_column_names) || anyNA(sby_column_names) || any(sby_column_names == "")) {
    # Executa instrucao do fluxo preservado
    sby_column_names <- paste0("V", seq_len(NCOL(sby_predictor_data)))
  # Executa instrucao do fluxo preservado
  }
  # Executa instrucao do fluxo preservado
  sby_column_names
# Executa instrucao do fluxo preservado
}

####
## Fim
#
