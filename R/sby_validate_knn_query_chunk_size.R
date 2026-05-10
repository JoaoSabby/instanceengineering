#' Validar tamanho de bloco para consultas KNN
#' @noRd
sby_validate_knn_query_chunk_size <- function(sby_knn_query_chunk_size){
  if(is.null(sby_knn_query_chunk_size)){
    sby_knn_query_chunk_size <- 1000L
  }

  if(!is.numeric(sby_knn_query_chunk_size) || length(sby_knn_query_chunk_size) != 1L || is.na(sby_knn_query_chunk_size) || sby_knn_query_chunk_size < 1L || sby_knn_query_chunk_size > .Machine$integer.max){
    sby_over_under_abort("'sby_knn_query_chunk_size' deve ser inteiro positivo")
  }

  as.integer(floor(sby_knn_query_chunk_size))
}
