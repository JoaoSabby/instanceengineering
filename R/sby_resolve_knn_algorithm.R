
#' Resolver algoritmo KNN automatico quando solicitado
#' @noRd
sby_resolve_knn_algorithm <- function(sby_knn_algorithm, sby_predictor_column_count){
  if(!identical(sby_knn_algorithm, "auto")){
    return(sby_knn_algorithm)
  }

  if(sby_predictor_column_count > 15L){
    "brute"
  } else {
    "kd_tree"
  }
}
