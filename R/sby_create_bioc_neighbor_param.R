#' Criar parametro BiocNeighbors para busca KNN unificada
#'
#' @details
#' A funcao implementa a traducao interna entre o algoritmo KNN declarado em `sby_knn_algorithm` e o objeto de parametros exigido pelo engine BiocNeighbors.
#' O modo `auto` seleciona uma estrategia exata conforme a dimensionalidade dos preditores, enquanto algoritmos explicitos preservam integralmente a escolha do chamador.
#'
#' @param sby_knn_algorithm Algoritmo KNN solicitado para o engine BiocNeighbors
#' @param sby_predictor_column_count Quantidade de colunas preditoras
#' @param sby_knn_distance_metric Metrica de distancia usada na consulta KNN
#'
#' @return Objeto de parametros BiocNeighbors para consulta KNN
#' @noRd
sby_create_bioc_neighbor_param <- function(sby_knn_algorithm, sby_predictor_column_count, sby_knn_distance_metric){
  
  # Resolve algoritmo BiocNeighbors automatico por dimensionalidade
  if(identical(
    x = sby_knn_algorithm,
    y = "auto"
  )){

    # Seleciona estrategia exata conforme quantidade de preditores
    sby_knn_algorithm <- ifelse(
      test = sby_predictor_column_count > 15L,
      yes  = "Exhaustive",
      no   = "Kmknn"
    )
  }

  # Bloqueia algoritmos exclusivos de FNN no engine BiocNeighbors
  if(sby_knn_algorithm %in% c("kd_tree", "cover_tree", "brute")){

    # Aborta combinacao de algoritmo e engine incompatvel
    sby_adanear_abort(
      sby_message = "'sby_knn_algorithm' deve ser um de 'auto', 'Kmknn', 'Vptree', 'Exhaustive', 'Annoy' ou 'Hnsw' quando 'sby_knn_engine = BiocNeighbors'"
    )
  }

  # Bloqueia produto interno porque BiocNeighbors nao implementa essa metrica
  if(identical(
    x = sby_knn_distance_metric,
    y = "ip"
  )){

    # Aborta metrica incompatvel com o engine BiocNeighbors
    sby_adanear_abort(
      sby_message = "'sby_knn_distance_metric = ip' e suportado apenas por 'sby_knn_engine = RcppHNSW'"
    )
  }

  # Define nome de distancia aceito pelo BiocNeighbors
  sbyBiocDistance <- switch(
    sby_knn_distance_metric,
    euclidean = "Euclidean",
    cosine = "Cosine",
    sby_adanear_abort(
      sby_message = "'sby_knn_distance_metric' invalido para BiocNeighbors"
    )
  )

  # Retorna objeto de parametros correspondente ao algoritmo selecionado
  return(switch(
    sby_knn_algorithm,
    Kmknn = BiocNeighbors::KmknnParam(distance = sbyBiocDistance),
    Vptree = BiocNeighbors::VptreeParam(distance = sbyBiocDistance),
    Exhaustive = BiocNeighbors::ExhaustiveParam(distance = sbyBiocDistance),
    Annoy = BiocNeighbors::AnnoyParam(distance = sbyBiocDistance),
    Hnsw = BiocNeighbors::HnswParam(distance = sbyBiocDistance),
    sby_adanear_abort(
      sby_message = "'sby_knn_algorithm' invalido para BiocNeighbors"
    )
  ))
}
####
## Fim
#
