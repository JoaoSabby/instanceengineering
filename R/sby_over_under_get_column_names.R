
#' Extrair nomes de colunas de forma segura
#' @noRd
sby_over_under_get_column_names <- function(sby_predictor_data) {
  sby_column_names <- colnames(sby_predictor_data)
  if (is.null(sby_column_names) || anyNA(sby_column_names) || any(sby_column_names == "")) {
    sby_column_names <- paste0("V", seq_len(NCOL(sby_predictor_data)))
  }
  sby_column_names
}

####
## Fim
#
