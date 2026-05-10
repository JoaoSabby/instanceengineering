
#' Montar tibble balanceado com TARGET como primeira coluna
#' @noRd
# Executa instrucao do fluxo preservado
sby_build_balanced_tibble <- function(sby_predictor_data, sby_target_vector) {
  # Executa instrucao do fluxo preservado
  sby_predictor_tibble <- tibble::as_tibble(sby_predictor_data)
  # Executa instrucao do fluxo preservado
  if ("TARGET" %in% names(sby_predictor_tibble)) {
    # Executa instrucao do fluxo preservado
    sby_over_under_abort("'sby_predictor_data' nao pode conter coluna chamada TARGET")
  # Executa instrucao do fluxo preservado
  }

  # Executa instrucao do fluxo preservado
  tibble::as_tibble(c(list(TARGET = as.factor(sby_target_vector)), as.list(sby_predictor_tibble)))
# Executa instrucao do fluxo preservado
}

####
## Fim
#
