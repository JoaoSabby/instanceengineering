#' Resolver backend KNN automatico quando solicitado
#'
#' @details
#' A funcao implementa uma unidade interna do fluxo de balanceamento com contrato de entrada explicito e retorno controlado
#' A documentacao descreve a intencao operacional para apoiar manutencao, auditoria e revisao tecnica do pacote
#'
#' @param sby_knn_backend Backend KNN informado pelo chamador
#' @param sby_knn_workers Numero de workers validado para consulta KNN
#'
#' @return Nome do backend KNN resolvido
#' @noRd
sby_resolve_knn_backend <- function(sby_knn_backend, sby_knn_workers){
  
  # Retorna backend explicito quando modo automatico nao foi solicitado
  if(!identical(
    x = sby_knn_backend,
    y = "auto"
  )){

    # Mantem a escolha explicita do chamador
    return(sby_knn_backend)
  }

  # Seleciona backend paralelizavel quando mais de um worker foi solicitado
  if(sby_knn_workers > 1L){

    # Retorna backend BiocNeighbors para execucao com workers multiplos
    return("BiocNeighbors")
  }

  # Retorna backend FNN como caminho padrao sequencial
  return("FNN")
}
####
## Fim
#
