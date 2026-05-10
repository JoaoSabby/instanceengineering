#' Imprimir etapa de balanceamento NearMiss
#'
#' @details
#' A funcao compoe a interface publica do pacote e valida os contratos de entrada antes de executar a etapa principal
#' O processamento preserva a semantica dos dados, registra pontos de diagnostico quando aplicavel e mantem retornos explicitos para facilitar auditoria em ambientes de producao
#' As chamadas auxiliares usam argumentos nomeados para reduzir ambiguidades durante manutencao e revisao de codigo
#'
#' @param x Objeto de etapa `sby_step_adanear`
#' @param width Largura de exibicao usada pelo metodo de impressao
#' @param ... Argumentos adicionais preservados para compatibilidade S3
#'
#' @return Retorna invisivelmente o objeto de etapa informado
#' @export
print.step_sby_step_adanear <- function(x, width = max(20, options()$width - 30), ...){
  
  # Normaliza argumentos S3 para nomes internos do pacote
  sby_x     <- x
  sby_width <- width
  sby_title <- "Balanceamento NearMiss-1 usando "

  # Define colunas exibidas conforme estado de treinamento
  if(isTRUE(sby_x$sby_trained)){

    # Usa colunas resolvidas durante prep para etapa treinada
    sby_columns <- sby_x$sby_columns
  }else{

    # Converte seletores recipes em texto para etapa nao treinada
    sby_columns <- recipes::sel2char(
      x = sby_x$sby_terms
    )
  }

  # Imprime resumo padronizado da etapa recipes
  recipes::print_step(
    tr_obj = sby_columns,
    untr_obj = sby_x$sby_terms,
    trained = sby_x$sby_trained,
    title = sby_title,
    width = sby_width
  )

  # Retorna objeto original invisivelmente apos impressao
  return(invisible(sby_x))
}
####
## Fim
#
