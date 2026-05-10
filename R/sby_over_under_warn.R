
#' Emitir aviso padronizado com cli
#' @noRd
# Executa instrucao do fluxo preservado
sby_over_under_warn <- function(sby_message) {
  # Executa instrucao do fluxo preservado
  if (requireNamespace("cli", quietly = TRUE)) {
    # Executa instrucao do fluxo preservado
    return(cli::cli_warn(sby_message, call = NULL))
  # Executa instrucao do fluxo preservado
  }
  # Executa instrucao do fluxo preservado
  warning(sby_message, call. = FALSE)
# Executa instrucao do fluxo preservado
}

####
## Fim
#
