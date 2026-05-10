#' Aplicar etapa de balanceamento NearMiss em novos dados
#'
#' @details
#' A funcao compoe a interface publica do pacote e valida os contratos de entrada antes de executar a etapa principal
#' O processamento preserva a semantica dos dados, registra pontos de diagnostico quando aplicavel e mantem retornos explicitos para facilitar auditoria em ambientes de producao
#' As chamadas auxiliares usam argumentos nomeados para reduzir ambiguidades durante manutencao e revisao de codigo
#'
#' @param object Objeto de etapa `sby_step_adanear` treinado
#' @param new_data Dados novos fornecidos ao `bake()` da recipe
#' @param ... Argumentos adicionais preservados para compatibilidade S3
#'
#' @return Tibble balanceado ou lista de auditoria quando configurado
#' @export
bake.step_sby_step_adanear <- function(object, new_data, ...){
  
  # Normaliza argumentos S3 para nomes internos do pacote
  sby_object   <- object
  sby_new_data <- new_data

  # Verifica se ha solicitacao de interrupcao antes do balanceamento
  sby_adanear_check_user_interrupt()

  # Verifica se a etapa foi treinada antes de aplicar bake
  if(!isTRUE(sby_object$sby_trained)){

    # Aborta quando a etapa nao passou por prep
    sby_adanear_abort(
      sby_message = "'sby_step_adanear()' precisa ser treinado com prep() antes de bake()"
    )
  }

  # Recupera coluna de desfecho selecionada no treinamento
  sby_target_column <- sby_object$sby_columns[[1L]]

  # Verifica se a coluna de desfecho existe nos novos dados
  if(!sby_target_column %in% names(sby_new_data)){

    # Aborta quando o desfecho selecionado esta ausente em new_data
    sby_adanear_abort(
      sby_message = paste0(
        "Coluna de desfecho nao encontrada em 'new_data': ",
        sby_target_column
      )
    )
  }

  # Define nomes de preditores excluindo a coluna de desfecho
  sby_original_names   <- names(sby_new_data)
  sby_predictor_names  <- setdiff(
    x = sby_original_names,
    y = sby_target_column
  )

  # Verifica se existe ao menos uma coluna preditora
  if(length(sby_predictor_names) < 1L){

    # Aborta quando apenas o desfecho esta disponivel
    sby_adanear_abort(
      sby_message = "'sby_step_adanear()' requer ao menos uma coluna preditora"
    )
  }

  # Executa NearMiss com a configuracao treinada na etapa recipes
  sby_sampling_result <- sby_nearmiss(
    sby_predictor_data          = as.data.frame(
      x = sby_new_data[, sby_predictor_names, drop = FALSE]
    ),
    sby_target_vector           = sby_new_data[[sby_target_column]],
    sby_under_ratio             = sby_object$sby_under_ratio,
    sby_k_under                 = sby_object$sby_k_under,
    sby_seed                    = sby_object$sby_seed,
    sby_audit                   = TRUE,
    sby_restore_types           = sby_object$sby_restore_types,
    sby_knn_algorithm           = sby_object$sby_knn_algorithm,
    sby_knn_backend             = sby_object$sby_knn_backend,
    sby_knn_workers             = sby_object$sby_knn_workers,
    sby_bioc_neighbor_algorithm = sby_object$sby_bioc_neighbor_algorithm,
    sby_hnsw_m                  = sby_object$sby_hnsw_m,
    sby_hnsw_ef                 = sby_object$sby_hnsw_ef
  )

  # Verifica se ha solicitacao de interrupcao apos o balanceamento
  sby_adanear_check_user_interrupt()

  # Retorna auditoria completa quando configurado na etapa
  if(isTRUE(sby_object$sby_audit)){

    # Entrega resultado auditavel ao chamador
    return(sby_sampling_result)
  }

  # Retorna apenas dados balanceados no fluxo recipes padrao
  return(sby_sampling_result$sby_balanced_data)
}
####
## Fim
#
