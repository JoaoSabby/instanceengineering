
#' Converter preditores para matrix double sem copias redundantes desnecessarias
#' @noRd
sby_over_under_as_numeric_matrix <- function(sby_predictor_data) {
  if (is.matrix(sby_predictor_data)) {
    sby_x_matrix <- sby_predictor_data
    storage.mode(sby_x_matrix) <- "double"
    return(sby_x_matrix)
  }

  sby_x_matrix <- data.matrix(sby_predictor_data)
  storage.mode(sby_x_matrix) <- "double"
  sby_x_matrix
}

####
## Fim
#
