
#' Validar entradas de sampling
#' @noRd
# Executa instrucao do fluxo preservado
sby_validate_sampling_inputs <- function(
  # Executa instrucao do fluxo preservado
  sby_predictor_data,
  # Executa instrucao do fluxo preservado
  sby_target_vector,
  # Executa instrucao do fluxo preservado
  sby_seed
# Executa instrucao do fluxo preservado
) {
  # Executa instrucao do fluxo preservado
  sby_over_under_load_packages()

  # Executa instrucao do fluxo preservado
  if (!is.data.frame(sby_predictor_data) && !is.matrix(sby_predictor_data)) {
    # Executa instrucao do fluxo preservado
    sby_over_under_abort("'sby_predictor_data' deve ser data.frame ou matrix")
  # Executa instrucao do fluxo preservado
  }

  # Executa instrucao do fluxo preservado
  if (NROW(sby_predictor_data) == 0L) {
    # Executa instrucao do fluxo preservado
    sby_over_under_abort("'sby_predictor_data' deve conter ao menos uma linha")
  # Executa instrucao do fluxo preservado
  }

  # Executa instrucao do fluxo preservado
  if (length(sby_target_vector) != NROW(sby_predictor_data)) {
    # Executa instrucao do fluxo preservado
    sby_over_under_abort("'sby_target_vector' deve ter o mesmo numero de linhas de 'sby_predictor_data'")
  # Executa instrucao do fluxo preservado
  }

  # Executa instrucao do fluxo preservado
  if (anyNA(sby_predictor_data)) {
    # Executa instrucao do fluxo preservado
    sby_over_under_abort("'sby_predictor_data' nao pode conter NA")
  # Executa instrucao do fluxo preservado
  }

  # Executa instrucao do fluxo preservado
  if (anyNA(sby_target_vector)) {
    # Executa instrucao do fluxo preservado
    sby_over_under_abort("'sby_target_vector' nao pode conter NA")
  # Executa instrucao do fluxo preservado
  }

  # Executa instrucao do fluxo preservado
  sby_x_check <- if (is.matrix(sby_predictor_data)) sby_predictor_data else sby_predictor_data
  # Executa instrucao do fluxo preservado
  sby_is_numeric_column <- if (is.matrix(sby_x_check)) {
    # Executa instrucao do fluxo preservado
    is.numeric(sby_x_check)
  # Executa instrucao do fluxo preservado
  } else {
    # Executa instrucao do fluxo preservado
    vapply(sby_x_check, is.numeric, logical(1L))
  # Executa instrucao do fluxo preservado
  }

  # Executa instrucao do fluxo preservado
  if (!all(sby_is_numeric_column)) {
    # Executa instrucao do fluxo preservado
    sby_over_under_abort("Todos os preditores devem ser numericos")
  # Executa instrucao do fluxo preservado
  }

  # Executa instrucao do fluxo preservado
  sby_target_factor <- as.factor(sby_target_vector)
  # Executa instrucao do fluxo preservado
  if (nlevels(sby_target_factor) != 2L) {
    # Executa instrucao do fluxo preservado
    sby_over_under_abort("'sby_target_vector' deve ser binario")
  # Executa instrucao do fluxo preservado
  }

  # Executa instrucao do fluxo preservado
  sby_class_counts <- table(sby_target_factor)
  # Executa instrucao do fluxo preservado
  if (any(sby_class_counts < 2L)) {
    # Executa instrucao do fluxo preservado
    sby_over_under_abort("Cada classe deve ter ao menos 2 observacoes")
  # Executa instrucao do fluxo preservado
  }

  # Executa instrucao do fluxo preservado
  if (!is.numeric(sby_seed) || length(sby_seed) != 1L || is.na(sby_seed) || !is.finite(sby_seed)) {
    # Executa instrucao do fluxo preservado
    sby_over_under_abort("'sby_seed' deve ser escalar numerico")
  # Executa instrucao do fluxo preservado
  }

  # Executa instrucao do fluxo preservado
  invisible(TRUE)
# Executa instrucao do fluxo preservado
}

####
## Fim
#
