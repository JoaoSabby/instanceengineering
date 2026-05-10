#' Organizar metadados da etapa de balanceamento NearMiss
#'
#' @details
#' A funcao compoe a interface publica do pacote e valida os contratos de entrada antes de executar a etapa principal
#' O processamento preserva a semantica dos dados, registra pontos de diagnostico quando aplicavel e mantem retornos explicitos para facilitar auditoria em ambientes de producao
#' As chamadas auxiliares usam argumentos nomeados para reduzir ambiguidades durante manutencao e revisao de codigo
#'
#' @param x Objeto de etapa `sby_step_balance`
#' @param ... Argumentos adicionais preservados para compatibilidade S3
#'
#' @return Data frame com metadados principais da etapa
#' @export
tidy.step_sby_step_balance <- function(x, ...){
  # Normaliza argumento S3 para nome interno do pacote
  sby_x <- x

  # Define termos exibidos conforme estado de treinamento
  if(isTRUE(sby_x$sby_trained)){

    # Usa colunas resolvidas durante prep para etapa treinada
    sby_terms <- sby_x$sby_columns
  }else{

    # Converte seletores recipes em texto para etapa nao treinada
    sby_terms <- recipes::sel2char(
      x = sby_x$sby_terms
    )
  }

  # Retorna metadados tabulares da etapa
  return(data.frame(
    sby_terms        = sby_terms,
    sby_under_ratio  = sby_x$sby_under_ratio,
    sby_k_under      = sby_x$sby_k_under,
    sby_audit        = sby_x$sby_audit,
    sby_id           = sby_x$sby_id,
    stringsAsFactors = FALSE
  ))
}
####
## Fim
#
