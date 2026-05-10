#' Criar parametro BiocParallel seguro para consultas KNN
#'
#' @details
#' A funcao implementa uma unidade interna do fluxo de balanceamento com contrato de entrada explicito e retorno controlado
#' A documentacao descreve a intencao operacional para apoiar manutencao, auditoria e revisao tecnica do pacote
#'
#' @param sby_knn_workers Numero de workers validado para consulta KNN
#'
#' @return Objeto de parametros BiocParallel para consulta KNN
#' @noRd
sby_create_knn_bioc_parallel_param <- function(sby_knn_workers){
  # Usa execucao serial quando apenas um worker esta configurado
  if(sby_knn_workers <= 1L){

    # Retorna parametro serial do BiocParallel
    return(BiocParallel::SerialParam())
  }

  # Seleciona backend paralelo compativel com o sistema operacional
  if(identical(
    x = .Platform$OS.type,
    y = "windows"
  )){

    # Retorna backend socket para compatibilidade com Windows
    return(BiocParallel::SnowParam(
      workers = sby_knn_workers,
      type    = "SOCK"
    ))
  }

  # Retorna backend multicore para sistemas nao Windows
  return(BiocParallel::MulticoreParam(
    workers = sby_knn_workers
  ))
}
####
## Fim
#
