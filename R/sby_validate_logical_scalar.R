

#' Validar parametro logico escalar
#' @noRd
sby_validate_logical_scalar <- function(sby_value, sby_name) {
  if (!is.logical(sby_value) || length(sby_value) != 1L || is.na(sby_value)) {
    sby_over_under_abort(paste0("'", sby_name, "' deve ser logical escalar nao ausente"))
  }
  sby_value
}

####
## Fim
#
