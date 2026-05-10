#' Resolver algoritmo KNN automatico quando solicitado
#'
#' @details
#' A funcao implementa uma unidade interna do fluxo de balanceamento com contrato de entrada explicito e retorno controlado.
#' A resolucao considera o engine efetivo para que `auto` produza algoritmos compativeis com FNN ou BiocNeighbors.
#'
#' @param sby_knn_algorithm Algoritmo KNN informado pelo chamador
#' @param sby_predictor_column_count Quantidade de colunas preditoras
#' @param sby_knn_engine Engine KNN resolvido
#'
#' @return Nome do algoritmo KNN resolvido
#' @noRd
sby_resolve_knn_algorithm <- function(sby_knn_algorithm, sby_predictor_column_count, sby_knn_engine){
  
  # Retorna algoritmo explicito quando modo automatico nao foi solicitado
  if(!identical(
    x = sby_knn_algorithm,
    y = "auto"
  )){

    # Mantem a escolha explicita do chamador
    return(sby_knn_algorithm)
  }

  # RcppHNSW gerencia seu algoritmo internamente
  if(identical(
    x = sby_knn_engine,
    y = "RcppHNSW"
  )){

    # Mantem marcador automatico porque o engine ignora algoritmos externos
    return("auto")
  }

  # Seleciona algoritmo FNN por dimensionalidade quando o modo automatico e usado
  if(identical(
    x = sby_knn_engine,
    y = "FNN"
  )){

    # Usa busca bruta para dimensionalidade mais alta
    if(sby_predictor_column_count > 15L){
      return("brute")
    }

    # Usa kd-tree para dimensionalidade mais baixa
    return("kd_tree")
  }

  # Seleciona algoritmo BiocNeighbors por dimensionalidade quando o modo automatico e usado
  if(sby_predictor_column_count > 15L){
    return("Exhaustive")
  }

  # Usa Kmknn como caminho exato padrao em menor dimensionalidade
  return("Kmknn")
}
####
## Fim
#
