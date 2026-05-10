#' Calcular quantidade retida da maioria
#'
#' @details
#' A funcao implementa uma unidade interna do fluxo de balanceamento com contrato de entrada explicito e retorno controlado
#' A documentacao descreve a intencao operacional para apoiar manutencao, auditoria e revisao tecnica do pacote
#'
#' @param sby_target_factor Fator binario com as classes observadas
#' @param sby_under_ratio Fracao da classe majoritaria a reter
#'
#' @return Quantidade inteira de linhas majoritarias a manter
#' @noRd
sby_compute_majority_retention_count <- function(sby_target_factor, sby_under_ratio){
  # Verifica se a fracao de retencao esta no intervalo permitido
  if(!(is.numeric(sby_under_ratio) && length(sby_under_ratio) == 1L && !is.na(sby_under_ratio) && sby_under_ratio > 0 && sby_under_ratio <= 1)){

    # Aborta quando a fracao de undersampling e invalida
    sby_over_under_abort(
      sby_message = "'sby_under_ratio' deve estar no intervalo (0, 1]"
    )
  }

  # Identifica papeis de classe para calcular o tamanho majoritario
  sby_class_roles    <- sby_get_binary_class_roles(
    sby_target_factor = sby_target_factor
  )
  sby_majority_count <- as.integer(sby_class_roles$sby_class_counts[sby_class_roles$sby_majority_label])
  sby_retained_count <- floor(sby_majority_count * sby_under_ratio)

  # Verifica se a fracao informada reteve ao menos uma linha
  if(sby_retained_count < 1L){

    # Aborta quando a configuracao removeria toda a classe majoritaria
    sby_over_under_abort(
      sby_message = "'sby_under_ratio' reteve zero linhas"
    )
  }

  # Retorna quantidade majoritaria retida pelo criterio configurado
  return(sby_retained_count)
}
####
## Fim
#
