
#' Reverter z-score em matrix double
#' @noRd
<<<<<<< HEAD
sby_revert_z_score_scaling_matrix <- function(sby_x_matrix, sby_scaling_info) {
=======
# Executa instrucao do fluxo preservado
sby_revert_z_score_scaling_matrix <- function(sby_x_matrix, sby_scaling_info) {
  # Executa instrucao do fluxo preservado
>>>>>>> origin/main
  sby_x_matrix <- sby_over_under_as_numeric_matrix(sby_x_matrix)
  # Executa instrucao do fluxo preservado
  sby_validate_scaling_info(sby_scaling_info, NCOL(sby_x_matrix))

<<<<<<< HEAD
  if (sby_over_under_native_available()) {
=======
  # Executa instrucao do fluxo preservado
  if (sby_over_under_native_available()) {
    # Executa instrucao do fluxo preservado
>>>>>>> origin/main
    sby_restored <- .Call("OU_ApplyZScoreC", sby_x_matrix, as.numeric(sby_scaling_info$centers), as.numeric(sby_scaling_info$scales), TRUE, PACKAGE = "instanceengineering")
  # Executa instrucao do fluxo preservado
  } else {
    # Executa instrucao do fluxo preservado
    sby_unscaled <- Rfast::eachrow(sby_x_matrix, sby_scaling_info$scales, oper = "*")
    # Executa instrucao do fluxo preservado
    sby_restored <- Rfast::eachrow(sby_unscaled, sby_scaling_info$centers, oper = "+")
  # Executa instrucao do fluxo preservado
  }

  # Executa instrucao do fluxo preservado
  storage.mode(sby_restored) <- "double"
  # Executa instrucao do fluxo preservado
  colnames(sby_restored) <- colnames(sby_x_matrix)
  # Executa instrucao do fluxo preservado
  sby_restored
# Executa instrucao do fluxo preservado
}

####
## Fim
#
