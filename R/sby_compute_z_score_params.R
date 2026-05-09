
#' Calcular parametros de z-score
#' @noRd
sby_compute_z_score_params <- function(sby_x_matrix){
  sby_x_matrix <- sby_over_under_as_numeric_matrix(sby_x_matrix)

  if(sby_over_under_native_available()){
    sby_params <- .Call("OU_ComputeZScoreParamsC", sby_x_matrix, PACKAGE = "instanceengineering")
  } else {
    sby_params <- list(
      centers = Rfast::colmeans(sby_x_matrix),
      scales = Rfast::colVars(sby_x_matrix, std = TRUE)
    )
  }

  sby_invalid <- is.na(sby_params$scales) | !is.finite(sby_params$scales) | sby_params$scales <= 0
  if(any(sby_invalid)){
    sby_column_names <- colnames(sby_x_matrix)
    if(is.null(sby_column_names)){
      sby_column_names <- paste0("V", seq_len(NCOL(sby_x_matrix)))
    }
    sby_over_under_abort(paste0(
      "Colunas com desvio padrao zero ou indefinido: ",
      paste(sby_column_names[sby_invalid], collapse = ", ")
    ))
  }

  list(centers = as.numeric(sby_params$centers), scales = as.numeric(sby_params$scales))
}
