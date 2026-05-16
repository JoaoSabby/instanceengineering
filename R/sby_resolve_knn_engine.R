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

  # Informa ao usuario a selecao automatica e a justificativa da decisao
  sby_adanear_inform(
    sby_message = paste0(
      "KNN automático: sby_knn_engine = \"FNN\". ",
      "Justificativa: FNN é o engine exato padrão; quando sby_knn_workers é ",
      "maior que 1, o pacote paraleliza a consulta por blocos sem depender ",
      "de dependencias adicionais."
    )
  )

  # Retorna FNN como caminho padrao exato, sequencial ou paralelo por blocos
  return("FNN")
}
####
## Fim
#
