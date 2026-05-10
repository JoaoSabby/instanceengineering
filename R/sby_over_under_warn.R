
#' Emitir aviso padronizado com cli
#' @noRd
sby_over_under_warn <- function(sby_message) {
  if (requireNamespace("cli", quietly = TRUE)) {
    return(cli::cli_warn(sby_message, call = NULL))
  }
  warning(sby_message, call. = FALSE)
}

####
## Fim
#
