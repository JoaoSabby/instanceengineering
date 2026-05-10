
#' Criar parametro BiocNeighbors para busca exata por padrao
#' @noRd
sby_create_bioc_neighbor_param <- function(sby_bioc_neighbor_algorithm, sby_predictor_column_count) {
  if (identical(sby_bioc_neighbor_algorithm, "auto")) {
    sby_bioc_neighbor_algorithm <- ifelse(
      test = sby_predictor_column_count > 15L,
      yes = "Exhaustive",
      no = "Kmknn"
    )
  }

  switch(
    sby_bioc_neighbor_algorithm,
    Kmknn = BiocNeighbors::KmknnParam(),
    Vptree = BiocNeighbors::VptreeParam(),
    Exhaustive = BiocNeighbors::ExhaustiveParam(),
    Annoy = BiocNeighbors::AnnoyParam(),
    Hnsw = BiocNeighbors::HnswParam(),
    sby_over_under_abort("'sby_bioc_neighbor_algorithm' invalido")
  )
}

####
## Fim
#
