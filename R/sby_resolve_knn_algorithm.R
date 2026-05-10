#' Resolver algoritmo KNN automatico quando solicitado
#'
#' @details
#' A funcao implementa uma unidade interna do fluxo de balanceamento com contrato de entrada explicito e retorno controlado
#' A documentacao descreve a intencao operacional para apoiar manutencao, auditoria e revisao tecnica do pacote
#'
#' @param sby_knn_algorithm Algoritmo KNN informado pelo chamador
#' @param sby_predictor_column_count Quantidade de colunas preditoras
#'
#' @return Nome do algoritmo KNN resolvido
#' @noRd
sby_resolve_knn_algorithm <- function(sby_knn_algorithm, sby_predictor_column_count){
  
  # Retorna algoritmo explicito quando modo automatico nao foi solicitado
  if(!identical(
    x = sby_knn_algorithm,
    y = "auto"
  )){

    # Mantem a escolha explicita do chamador
    return(sby_knn_algorithm)
  }

  # Seleciona algoritmo por dimensionalidade quando o modo automatico e usado
  if(sby_predictor_column_count > 15L){

    # Retorna busca exaustiva para dados de maior dimensionalidade
    return("brute")
  }

  # Retorna arvore kd para dados de menor dimensionalidade
  return("kd_tree")
}
####
## Fim
#
