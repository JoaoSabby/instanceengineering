
#' Validar entradas de sampling
#' @noRd
sby_validate_sampling_inputs <- function(
  sby_predictor_data,
  sby_target_vector,
  sby_seed
){
  sby_over_under_load_packages()

  if(!is.data.frame(sby_predictor_data) && !is.matrix(sby_predictor_data)){
    sby_over_under_abort("'sby_predictor_data' deve ser data.frame ou matrix")
  }

  if(NROW(sby_predictor_data) == 0L){
    sby_over_under_abort("'sby_predictor_data' deve conter ao menos uma linha")
  }

  if(length(sby_target_vector) != NROW(sby_predictor_data)){
    sby_over_under_abort("'sby_target_vector' deve ter o mesmo numero de linhas de 'sby_predictor_data'")
  }

  if(anyNA(sby_predictor_data)){
    sby_over_under_abort("'sby_predictor_data' nao pode conter NA")
  }

  if(anyNA(sby_target_vector)){
    sby_over_under_abort("'sby_target_vector' nao pode conter NA")
  }

  sby_x_check <- if(is.matrix(sby_predictor_data)) sby_predictor_data else sby_predictor_data
  sby_is_numeric_column <- if(is.matrix(sby_x_check)) {
    is.numeric(sby_x_check)
  } else {
    vapply(sby_x_check, is.numeric, logical(1L))
  }

  if(!all(sby_is_numeric_column)){
    sby_over_under_abort("Todos os preditores devem ser numericos")
  }

  sby_target_factor <- as.factor(sby_target_vector)
  if(nlevels(sby_target_factor) != 2L){
    sby_over_under_abort("'sby_target_vector' deve ser binario")
  }

  sby_class_counts <- table(sby_target_factor)
  if(any(sby_class_counts < 2L)){
    sby_over_under_abort("Cada classe deve ter ao menos 2 observacoes")
  }

  if(!is.numeric(sby_seed) || length(sby_seed) != 1L || is.na(sby_seed) || !is.finite(sby_seed)){
    sby_over_under_abort("'sby_seed' deve ser escalar numerico")
  }

  invisible(TRUE)
}
