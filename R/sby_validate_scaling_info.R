
#' Validar parametros de escala precomputados
#' @noRd
sby_validate_scaling_info <- function(sby_scaling_info, sby_predictor_column_count){
  if(!is.list(sby_scaling_info) || is.null(sby_scaling_info$centers) || is.null(sby_scaling_info$scales)){
    sby_over_under_abort("'sby_precomputed_scaling' deve conter 'centers' e 'scales'")
  }
  if(length(sby_scaling_info$centers) != sby_predictor_column_count || length(sby_scaling_info$scales) != sby_predictor_column_count){
    sby_over_under_abort("'sby_precomputed_scaling' deve ter um centro e uma escala por coluna")
  }
  if(anyNA(sby_scaling_info$centers) || anyNA(sby_scaling_info$scales) || any(!is.finite(sby_scaling_info$centers)) || any(!is.finite(sby_scaling_info$scales))){
    sby_over_under_abort("'sby_precomputed_scaling' contem valores ausentes ou infinitos")
  }
  if(any(sby_scaling_info$scales <= 0)){
    sby_over_under_abort("'sby_precomputed_scaling$scales' deve conter apenas valores positivos")
  }
  invisible(TRUE)
}
