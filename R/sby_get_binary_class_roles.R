

#' Identificar classes minoritaria e majoritaria
#' @noRd
<<<<<<< HEAD
sby_get_binary_class_roles <- function(sby_target_factor) {
  sby_class_counts <- table(sby_target_factor)
  if (length(sby_class_counts) != 2L) {
=======
# Executa instrucao do fluxo preservado
sby_get_binary_class_roles <- function(sby_target_factor) {
  # Executa instrucao do fluxo preservado
  sby_class_counts <- table(sby_target_factor)
  # Executa instrucao do fluxo preservado
  if (length(sby_class_counts) != 2L) {
    # Executa instrucao do fluxo preservado
>>>>>>> origin/main
    sby_over_under_abort("'sby_target_vector' deve ser binario")
  # Executa instrucao do fluxo preservado
  }
<<<<<<< HEAD
  if (sby_class_counts[[1L]] == sby_class_counts[[2L]]) {
=======
  # Executa instrucao do fluxo preservado
  if (sby_class_counts[[1L]] == sby_class_counts[[2L]]) {
    # Executa instrucao do fluxo preservado
>>>>>>> origin/main
    sby_over_under_abort("As rotinas de sampling requerem classes desbalanceadas")
  # Executa instrucao do fluxo preservado
  }

  # Executa instrucao do fluxo preservado
  list(
    # Executa instrucao do fluxo preservado
    sby_class_counts = sby_class_counts,
    # Executa instrucao do fluxo preservado
    sby_minority_label = names(sby_class_counts)[which.min(sby_class_counts)],
    # Executa instrucao do fluxo preservado
    sby_majority_label = names(sby_class_counts)[which.max(sby_class_counts)]
  # Executa instrucao do fluxo preservado
  )
# Executa instrucao do fluxo preservado
}

####
## Fim
#
