#' Validar tamanho de bloco para consultas KNN
#' @noRd
# Executa instrucao do fluxo preservado
sby_validate_knn_query_chunk_size <- function(sby_knn_query_chunk_size) {
  # Executa instrucao do fluxo preservado
  if (is.null(sby_knn_query_chunk_size)) {
    # Executa instrucao do fluxo preservado
    sby_knn_query_chunk_size <- 1000L
  # Executa instrucao do fluxo preservado
  }

  # Executa instrucao do fluxo preservado
  if (!is.numeric(sby_knn_query_chunk_size) || length(sby_knn_query_chunk_size) != 1L || is.na(sby_knn_query_chunk_size) || sby_knn_query_chunk_size < 1L || sby_knn_query_chunk_size > .Machine$integer.max) {
    # Executa instrucao do fluxo preservado
    sby_over_under_abort("'sby_knn_query_chunk_size' deve ser inteiro positivo")
  # Executa instrucao do fluxo preservado
  }

  # Executa instrucao do fluxo preservado
  as.integer(floor(sby_knn_query_chunk_size))
# Executa instrucao do fluxo preservado
}

####
## Fim
#
