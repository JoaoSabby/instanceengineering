
#' Criar parametro BiocNeighbors para busca exata por padrao
#' @noRd
# Executa instrucao do fluxo preservado
sby_create_bioc_neighbor_param <- function(sby_bioc_neighbor_algorithm, sby_predictor_column_count) {
  # Executa instrucao do fluxo preservado
  if (identical(sby_bioc_neighbor_algorithm, "auto")) {
    # Executa instrucao do fluxo preservado
    sby_bioc_neighbor_algorithm <- if (sby_predictor_column_count > 15L) "Exhaustive" else "Kmknn"
  # Executa instrucao do fluxo preservado
  }

  # Executa instrucao do fluxo preservado
  switch(
    # Executa instrucao do fluxo preservado
    sby_bioc_neighbor_algorithm,
    # Executa instrucao do fluxo preservado
    Kmknn = BiocNeighbors::KmknnParam(),
    # Executa instrucao do fluxo preservado
    Vptree = BiocNeighbors::VptreeParam(),
    # Executa instrucao do fluxo preservado
    Exhaustive = BiocNeighbors::ExhaustiveParam(),
    # Executa instrucao do fluxo preservado
    Annoy = BiocNeighbors::AnnoyParam(),
    # Executa instrucao do fluxo preservado
    Hnsw = BiocNeighbors::HnswParam(),
    # Executa instrucao do fluxo preservado
    sby_over_under_abort("'sby_bioc_neighbor_algorithm' invalido")
  # Executa instrucao do fluxo preservado
  )
# Executa instrucao do fluxo preservado
}

####
## Fim
#
