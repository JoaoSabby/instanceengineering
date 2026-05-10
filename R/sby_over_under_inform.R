
#' Emitir mensagem informativa padronizada com cli
#' @noRd
# Executa instrucao do fluxo preservado
sby_over_under_inform <- function(sby_message) {
  # Executa instrucao do fluxo preservado
  if (requireNamespace("cli", quietly = TRUE)) {
    # Executa instrucao do fluxo preservado
    return(cli::cli_inform(sby_message))
  # Executa instrucao do fluxo preservado
  }
  # Executa instrucao do fluxo preservado
  message(sby_message)
# Executa instrucao do fluxo preservado
}

####
## Fim
#
