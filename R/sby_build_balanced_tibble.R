
#' Montar tibble balanceado com TARGET como primeira coluna
#' @noRd
sby_build_balanced_tibble <- function(sby_predictor_data, sby_target_vector){
  sby_predictor_tibble <- tibble::as_tibble(sby_predictor_data)
  if("TARGET" %in% names(sby_predictor_tibble)){
    sby_over_under_abort("'sby_predictor_data' nao pode conter coluna chamada TARGET")
  }

  tibble::as_tibble(c(list(TARGET = as.factor(sby_target_vector)), as.list(sby_predictor_tibble)))
}
