#' Resolver engine KNN automatico quando solicitado
#'
#' @details
#' A funcao implementa uma unidade interna do fluxo de balanceamento com contrato de entrada explicito e retorno controlado
#' A documentacao descreve a intencao operacional para apoiar manutencao, auditoria e revisao tecnica do pacote
#'
#' @param sby_knn_engine Engine KNN informado pelo chamador
#' @param sby_knn_workers Numero de workers validado para consulta KNN
#'
#' @return Nome do engine KNN resolvido
#' @noRd
sby_resolve_knn_engine <- function(sby_knn_engine, sby_knn_workers){
  
  # Retorna engine explicito quando modo automatico nao foi solicitado
  if(!identical(
    x = sby_knn_engine,
    y = "auto"
  )){

    # Mantem a escolha explicita do chamador
    return(sby_knn_engine)
  }

  # Seleciona engine paralelizavel quando mais de um worker foi solicitado
  if(sby_knn_workers > 1L){

    # Informa ao usuario a selecao automatica e a justificativa da decisao
    sby_adanear_inform(
      sby_message = paste0(
        "KNN automático: sby_knn_engine = \"BiocNeighbors\". ",
        "Justificativa: sby_knn_workers é maior que 1, então o pacote escolheu ",
        "BiocNeighbors para permitir execução paralela da busca de vizinhos."
      )
    )

    # Retorna engine BiocNeighbors para execucao com workers multiplos
    return("BiocNeighbors")
  }

  # Informa ao usuario a selecao automatica e a justificativa da decisao
  sby_adanear_inform(
    sby_message = paste0(
      "KNN automático: sby_knn_engine = \"FNN\". ",
      "Justificativa: sby_knn_workers é igual a 1, então o pacote escolheu ",
      "FNN como implementação sequencial padrão, simples e eficiente."
    )
  )

  # Retorna engine FNN como caminho padrao sequencial
  return("FNN")
}
####
## Fim
#
