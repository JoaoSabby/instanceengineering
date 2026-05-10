
#' Validar parametros de escala precomputados
#' @noRd
<<<<<<< HEAD
sby_validate_scaling_info <- function(sby_scaling_info, sby_predictor_column_count) {
  if (!is.list(sby_scaling_info) || is.null(sby_scaling_info$centers) || is.null(sby_scaling_info$scales)) {
=======
# Executa instrucao do fluxo preservado
sby_validate_scaling_info <- function(sby_scaling_info, sby_predictor_column_count) {
  # Executa instrucao do fluxo preservado
  if (!is.list(sby_scaling_info) || is.null(sby_scaling_info$centers) || is.null(sby_scaling_info$scales)) {
    # Executa instrucao do fluxo preservado
>>>>>>> origin/main
    sby_over_under_abort("'sby_precomputed_scaling' deve conter 'centers' e 'scales'")
  # Executa instrucao do fluxo preservado
  }
<<<<<<< HEAD
  if (length(sby_scaling_info$centers) != sby_predictor_column_count || length(sby_scaling_info$scales) != sby_predictor_column_count) {
=======
  # Executa instrucao do fluxo preservado
  if (length(sby_scaling_info$centers) != sby_predictor_column_count || length(sby_scaling_info$scales) != sby_predictor_column_count) {
    # Executa instrucao do fluxo preservado
>>>>>>> origin/main
    sby_over_under_abort("'sby_precomputed_scaling' deve ter um centro e uma escala por coluna")
  # Executa instrucao do fluxo preservado
  }
<<<<<<< HEAD
  if (anyNA(sby_scaling_info$centers) || anyNA(sby_scaling_info$scales) || any(!is.finite(sby_scaling_info$centers)) || any(!is.finite(sby_scaling_info$scales))) {
=======
  # Executa instrucao do fluxo preservado
  if (anyNA(sby_scaling_info$centers) || anyNA(sby_scaling_info$scales) || any(!is.finite(sby_scaling_info$centers)) || any(!is.finite(sby_scaling_info$scales))) {
    # Executa instrucao do fluxo preservado
>>>>>>> origin/main
    sby_over_under_abort("'sby_precomputed_scaling' contem valores ausentes ou infinitos")
  # Executa instrucao do fluxo preservado
  }
<<<<<<< HEAD
  if (any(sby_scaling_info$scales <= 0)) {
=======
  # Executa instrucao do fluxo preservado
  if (any(sby_scaling_info$scales <= 0)) {
    # Executa instrucao do fluxo preservado
>>>>>>> origin/main
    sby_over_under_abort("'sby_precomputed_scaling$scales' deve conter apenas valores positivos")
  # Executa instrucao do fluxo preservado
  }
  # Executa instrucao do fluxo preservado
  invisible(TRUE)
# Executa instrucao do fluxo preservado
}

####
## Fim
#
