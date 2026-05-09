
#' Reverter z-score em matrix double
#' @noRd
sby_revert_z_score_scaling_matrix <- function(sby_x_matrix, sby_scaling_info){
  sby_x_matrix <- sby_over_under_as_numeric_matrix(sby_x_matrix)
  sby_validate_scaling_info(sby_scaling_info, NCOL(sby_x_matrix))

  if(sby_over_under_native_available()){
    sby_restored <- .Call("OU_ApplyZScoreC", sby_x_matrix, as.numeric(sby_scaling_info$centers), as.numeric(sby_scaling_info$scales), TRUE, PACKAGE = "instanceengineering")
  } else {
    sby_unscaled <- Rfast::eachrow(sby_x_matrix, sby_scaling_info$scales, oper = "*")
    sby_restored <- Rfast::eachrow(sby_unscaled, sby_scaling_info$centers, oper = "+")
  }

  storage.mode(sby_restored) <- "double"
  colnames(sby_restored) <- colnames(sby_x_matrix)
  sby_restored
}
