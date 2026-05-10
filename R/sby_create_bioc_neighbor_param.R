#' Criar parametro BiocNeighbors para busca exata por padrao
#'
#' @details
#' A funcao implementa uma unidade interna do fluxo de balanceamento com contrato de entrada explicito e retorno controlado
#' A documentacao descreve a intencao operacional para apoiar manutencao, auditoria e revisao tecnica do pacote
#'
#' @param sby_bioc_neighbor_algorithm Algoritmo BiocNeighbors solicitado
#' @param sby_predictor_column_count Quantidade de colunas preditoras
#'
#' @return Objeto de parametros BiocNeighbors para consulta KNN
#' @noRd
sby_create_bioc_neighbor_param <- function(sby_bioc_neighbor_algorithm, sby_predictor_column_count){
  # Resolve algoritmo BiocNeighbors automatico por dimensionalidade
  if(identical(
    x = sby_bioc_neighbor_algorithm,
    y = "auto"
  )){

    # Seleciona estrategia exata conforme quantidade de preditores
    sby_bioc_neighbor_algorithm <- ifelse(
      test = sby_predictor_column_count > 15L,
      yes  = "Exhaustive",
      no   = "Kmknn"
    )
  }

  # Retorna objeto de parametros correspondente ao algoritmo selecionado
  return(switch(
    sby_bioc_neighbor_algorithm,
    Kmknn = BiocNeighbors::KmknnParam(),
    Vptree = BiocNeighbors::VptreeParam(),
    Exhaustive = BiocNeighbors::ExhaustiveParam(),
    Annoy = BiocNeighbors::AnnoyParam(),
    Hnsw = BiocNeighbors::HnswParam(),
    sby_over_under_abort(
      sby_message = "'sby_bioc_neighbor_algorithm' invalido"
    )
  ))
}
####
## Fim
#
