
#' Emitir erro padronizado com cli
#' @noRd
<<<<<<< HEAD
sby_over_under_abort <- function(sby_message) {
  if (requireNamespace("cli", quietly = TRUE)) {
=======
# Executa instrucao do fluxo preservado
sby_over_under_abort <- function(sby_message) {
  # Executa instrucao do fluxo preservado
  if (requireNamespace("cli", quietly = TRUE)) {
    # Executa instrucao do fluxo preservado
>>>>>>> origin/main
    cli::cli_abort(sby_message, call = NULL)
  # Executa instrucao do fluxo preservado
  }
  # Executa instrucao do fluxo preservado
  stop(sby_message, call. = FALSE)
# Executa instrucao do fluxo preservado
}

####
## Fim
#
