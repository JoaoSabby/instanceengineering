#' Converter preditores para matriz numerica
#'
#' @details
#' A funcao implementa uma unidade interna do fluxo de balanceamento com contrato de entrada explicito e retorno controlado
#' A documentacao descreve a intencao operacional para apoiar manutencao, auditoria e revisao tecnica do pacote
#'
#' @param sby_predictor_data Dados preditores em data frame ou matriz
#'
#' @return Matriz numerica com nomes de colunas preservados
#' @noRd
sby_over_under_as_numeric_matrix <- function(sby_predictor_data){
  # Converte a entrada para matriz quando os preditores estao em data frame
  if(is.data.frame(sby_predictor_data)){

    # Materializa data frame como matriz numerica
    sby_x_matrix <- data.matrix(
      frame = sby_predictor_data
    )
  }else{

    # Reutiliza matriz de entrada para conversao de armazenamento
    sby_x_matrix <- as.matrix(
      x = sby_predictor_data
    )
  }

  # Garante armazenamento double para calculos numericos posteriores
  storage.mode(sby_x_matrix) <- "double"

  # Preserva nomes de colunas existentes ou gerados para a matriz numerica
  colnames(sby_x_matrix) <- sby_over_under_get_column_names(
    sby_predictor_data = sby_predictor_data
  )

  # Retorna matriz numerica normalizada
  return(sby_x_matrix)
}
####
## Fim
#
