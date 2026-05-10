
#' Calcular quantidade sintetica da minoria
#' @noRd
# Executa instrucao do fluxo preservado
sby_compute_minority_expansion_count <- function(sby_target_factor, sby_over_ratio) {
  # Executa instrucao do fluxo preservado
  if (!is.numeric(sby_over_ratio) || length(sby_over_ratio) != 1L || is.na(sby_over_ratio) || sby_over_ratio < 0) {
    # Executa instrucao do fluxo preservado
    sby_over_under_abort("'sby_over_ratio' deve ser escalar numerico nao negativo")
  # Executa instrucao do fluxo preservado
  }

  # Executa instrucao do fluxo preservado
  sby_class_roles <- sby_get_binary_class_roles(sby_target_factor)
  # Executa instrucao do fluxo preservado
  sby_minority_count <- as.integer(sby_class_roles$sby_class_counts[sby_class_roles$sby_minority_label])
  # Executa instrucao do fluxo preservado
  sby_synthetic_count <- floor(sby_minority_count * sby_over_ratio)

  # Executa instrucao do fluxo preservado
  if (sby_synthetic_count < 1L) {
    # Executa instrucao do fluxo preservado
    sby_over_under_abort("'sby_over_ratio' gerou zero linhas sinteticas")
  # Executa instrucao do fluxo preservado
  }

  # Executa instrucao do fluxo preservado
  sby_synthetic_count
# Executa instrucao do fluxo preservado
}

####
## Fim
#
