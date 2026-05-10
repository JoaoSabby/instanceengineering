
#' Aplicar z-score em matrix double
#' @noRd
# Executa instrucao do fluxo preservado
sby_apply_z_score_scaling_matrix <- function(sby_x_matrix, sby_scaling_info) {
  # Executa instrucao do fluxo preservado
  sby_x_matrix <- sby_over_under_as_numeric_matrix(sby_x_matrix)
  # Executa instrucao do fluxo preservado
  sby_validate_scaling_info(sby_scaling_info, NCOL(sby_x_matrix))

  # Executa instrucao do fluxo preservado
  if (sby_over_under_native_available()) {
    # Executa instrucao do fluxo preservado
    sby_scaled <- .Call("OU_ApplyZScoreC", sby_x_matrix, as.numeric(sby_scaling_info$centers), as.numeric(sby_scaling_info$scales), FALSE, PACKAGE = "instanceengineering")
  # Executa instrucao do fluxo preservado
  } else {
    # Executa instrucao do fluxo preservado
    sby_centered <- Rfast::eachrow(sby_x_matrix, sby_scaling_info$centers, oper = "-")
    # Executa instrucao do fluxo preservado
    sby_scaled <- Rfast::eachrow(sby_centered, sby_scaling_info$scales, oper = "/")
  # Executa instrucao do fluxo preservado
  }

  # Executa instrucao do fluxo preservado
  storage.mode(sby_scaled) <- "double"
  # Executa instrucao do fluxo preservado
  colnames(sby_scaled) <- colnames(sby_x_matrix)
  # Executa instrucao do fluxo preservado
  sby_scaled
# Executa instrucao do fluxo preservado
}

####
## Fim
#
