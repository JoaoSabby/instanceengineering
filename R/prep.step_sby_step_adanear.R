#' Preparar etapa de balanceamento NearMiss
#'
#' @details
#' A funcao compoe a interface publica do pacote e valida os contratos de entrada antes de executar a etapa principal
#' O processamento preserva a semantica dos dados, registra pontos de diagnostico quando aplicavel e mantem retornos explicitos para facilitar auditoria em ambientes de producao
#' As chamadas auxiliares usam argumentos nomeados para reduzir ambiguidades durante manutencao e revisao de codigo
#'
#' @param x Objeto de etapa `sby_step_adanear` nao treinado
#' @param training Dados de treinamento usados pelo `prep()`
#' @param info Metadados opcionais de variaveis fornecidos por recipes
#' @param ... Argumentos adicionais preservados para compatibilidade S3
#'
#' @return Objeto de etapa `sby_step_adanear` treinado
#' @export
prep.step_sby_step_adanear <- function(x, training, info = NULL, ...){
  
  # Normaliza argumentos S3 para nomes internos do pacote
  sby_x        <- x
  sby_training <- training
  sby_info     <- info

  # Verifica se ha solicitacao de interrupcao antes da selecao de colunas
  sby_adanear_check_user_interrupt()

  # Avalia seletores recipes para identificar coluna de desfecho
  sby_selected_columns <- recipes::recipes_eval_select(
    quos = sby_x$sby_terms,
    data = sby_training,
    info = sby_info
  )

  # Verifica se exatamente uma coluna de desfecho foi selecionada
  if(length(sby_selected_columns) != 1L){

    # Aborta quando a selecao do desfecho nao e univoca
    sby_adanear_abort(
      sby_message = "'sby_step_adanear()' deve selecionar exatamente uma coluna de desfecho"
    )
  }

  # Retorna nova etapa marcada como treinada com a coluna selecionada
  return(sby_step_adanear_new(
    sby_terms                   = sby_x$sby_terms,
    sby_role                    = sby_x$sby_role,
    sby_trained                 = TRUE,
    sby_columns                 = names(sby_selected_columns),
    sby_under_ratio             = sby_x$sby_under_ratio,
    sby_k_under                 = sby_x$sby_k_under,
    sby_seed                    = sby_x$sby_seed,
    sby_audit                   = sby_x$sby_audit,
    sby_restore_types           = sby_x$sby_restore_types,
    sby_knn_algorithm           = sby_x$sby_knn_algorithm,
    sby_knn_engine             = sby_x$sby_knn_engine,
    sby_distance_metric         = sby_x$sby_distance_metric,
    sby_knn_workers             = sby_x$sby_knn_workers,
    sby_hnsw_m                  = sby_x$sby_hnsw_m,
    sby_hnsw_ef                 = sby_x$sby_hnsw_ef,
    sby_skip                    = sby_x$sby_skip,
    sby_id                      = sby_x$sby_id
  ))
}
####
## Fim
#
