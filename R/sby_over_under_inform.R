
#' Emitir mensagem informativa padronizada com cli
#' @noRd
sby_over_under_inform <- function(sby_message) {
  if (requireNamespace("cli", quietly = TRUE)) {
    return(cli::cli_inform(sby_message))
  }
  message(sby_message)
}

####
## Fim
#
