#' Identificar classes minoritaria e majoritaria
#'
#' @details
#' A funcao implementa uma unidade interna do fluxo de balanceamento com contrato de entrada explicito e retorno controlado
#' A documentacao descreve a intencao operacional para apoiar manutencao, auditoria e revisao tecnica do pacote
#'
#' @param sby_target_factor Fator binario com as classes observadas
#'
#' @return Lista com contagens e rotulos das classes minoritaria e majoritaria
#' @noRd
sby_get_binary_class_roles <- function(sby_target_factor){
  # Calcula distribuicao de frequencias por classe
  sby_class_counts <- table(
    sby_target_factor
  )

  # Verifica se o alvo possui exatamente duas classes
  if(length(sby_class_counts) != 2L){

    # Aborta quando o alvo nao e binario
    sby_over_under_abort(
      sby_message = "'sby_target_vector' deve ser binario"
    )
  }

  # Verifica se ha desbalanceamento entre as classes
  if(sby_class_counts[[1L]] == sby_class_counts[[2L]]){

    # Aborta quando a rotina de balanceamento nao tem classe majoritaria definida
    sby_over_under_abort(
      sby_message = "As rotinas de sampling requerem classes desbalanceadas"
    )
  }

  # Retorna papeis de classe calculados a partir das frequencias
  return(list(
    sby_class_counts   = sby_class_counts,
    sby_minority_label = names(sby_class_counts)[which.min(sby_class_counts)],
    sby_majority_label = names(sby_class_counts)[which.max(sby_class_counts)]
  ))
}
####
## Fim
#
