
#' Inferir tipos numericos para restauracao posterior
#' @noRd
sby_infer_numeric_column_types <- function(sby_data_frame){
  sby_x_matrix <- sby_over_under_as_numeric_matrix(sby_data_frame)
  sby_column_names <- sby_over_under_get_column_names(sby_data_frame)

  sby_infer_one <- function(sby_column_data){
    sby_unique_values <- sort(unique(sby_column_data))
    if(length(sby_unique_values) <= 2L && all(sby_unique_values %in% c(0, 1))){
      return("binary")
    }
    sby_is_integer_like <- all(abs(sby_column_data - round(sby_column_data)) < sqrt(.Machine$double.eps))
    if(sby_is_integer_like){
      return("integer")
    }
    "double"
  }

  data.frame(
    sby_column_name = sby_column_names,
    sby_inferred_type = vapply(seq_len(NCOL(sby_x_matrix)), function(j) sby_infer_one(sby_x_matrix[, j]), character(1L)),
    stringsAsFactors = FALSE
  )
}
