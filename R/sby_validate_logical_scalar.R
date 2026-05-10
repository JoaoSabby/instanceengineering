

#' Validar parametro logico escalar
#' @noRd
<<<<<<< HEAD
sby_validate_logical_scalar <- function(sby_value, sby_name) {
  if (!is.logical(sby_value) || length(sby_value) != 1L || is.na(sby_value)) {
=======
# Executa instrucao do fluxo preservado
sby_validate_logical_scalar <- function(sby_value, sby_name) {
  # Executa instrucao do fluxo preservado
  if (!is.logical(sby_value) || length(sby_value) != 1L || is.na(sby_value)) {
    # Executa instrucao do fluxo preservado
>>>>>>> origin/main
    sby_over_under_abort(paste0("'", sby_name, "' deve ser logical escalar nao ausente"))
  # Executa instrucao do fluxo preservado
  }
  # Executa instrucao do fluxo preservado
  sby_value
# Executa instrucao do fluxo preservado
}

####
## Fim
#
