
#' Calcular quantidade retida da maioria
#' @noRd
# Executa instrucao do fluxo preservado
sby_compute_majority_retention_count <- function(sby_target_factor, sby_under_ratio) {
  # Executa instrucao do fluxo preservado
  if (!is.numeric(sby_under_ratio) || length(sby_under_ratio) != 1L || is.na(sby_under_ratio) || sby_under_ratio <= 0 || sby_under_ratio > 1) {
    # Executa instrucao do fluxo preservado
    sby_over_under_abort("'sby_under_ratio' deve estar no intervalo (0, 1]")
  # Executa instrucao do fluxo preservado
  }

  # Executa instrucao do fluxo preservado
  sby_class_roles <- sby_get_binary_class_roles(sby_target_factor)
  # Executa instrucao do fluxo preservado
  sby_majority_count <- as.integer(sby_class_roles$sby_class_counts[sby_class_roles$sby_majority_label])
  # Executa instrucao do fluxo preservado
  sby_retained_count <- floor(sby_majority_count * sby_under_ratio)

  # Executa instrucao do fluxo preservado
  if (sby_retained_count < 1L) {
    # Executa instrucao do fluxo preservado
    sby_over_under_abort("'sby_under_ratio' reteve zero linhas")
  # Executa instrucao do fluxo preservado
  }

  # Executa instrucao do fluxo preservado
  sby_retained_count
# Executa instrucao do fluxo preservado
}

####
## Fim
#
