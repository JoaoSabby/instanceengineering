
#' Aplicar z-score em matrix double
#' @noRd
sby_apply_z_score_scaling_matrix <- function(sby_x_matrix, sby_scaling_info) {
  sby_x_matrix <- sby_over_under_as_numeric_matrix(sby_x_matrix)
  sby_validate_scaling_info(sby_scaling_info, NCOL(sby_x_matrix))

  if (sby_over_under_native_available()) {
    sby_scaled <- .Call("OU_ApplyZScoreC", sby_x_matrix, as.numeric(sby_scaling_info$centers), as.numeric(sby_scaling_info$scales), FALSE, PACKAGE = "instanceengineering")
  } else {
    sby_centered <- Rfast::eachrow(sby_x_matrix, sby_scaling_info$centers, oper = "-")
    sby_scaled <- Rfast::eachrow(sby_centered, sby_scaling_info$scales, oper = "/")
  }

  storage.mode(sby_scaled) <- "double"
  colnames(sby_scaled) <- colnames(sby_x_matrix)
  sby_scaled
}

####
## Fim
#
