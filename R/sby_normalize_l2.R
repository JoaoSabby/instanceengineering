#' Normalizar linhas de uma matriz pela norma L2
#'
#' @details
#' A funcao aplica normalizacao vetorial linha a linha usando apenas primitivas base de R.
#' Linhas com norma zero sao preservadas com divisor unitario para evitar propagacao de valores infinitos ou indefinidos.
#'
#' @param sby_x_matrix Matriz numerica a normalizar
#'
#' @return Matriz numerica com linhas normalizadas pela norma L2
#' @noRd
SbyNormalizeL2 <- function(sby_x_matrix){
  
  # Calcula norma euclidiana por linha com operacoes vetorizadas base
  sbyRowNorm <- sqrt(rowSums(sby_x_matrix^2))

  # Protege linhas nulas contra divisao por zero sem alterar sua direcao nula
  sbyRowNorm[sbyRowNorm == 0] <- 1

  # Divide cada linha pela respectiva norma L2
  return(sby_x_matrix / sbyRowNorm)
}
####
## Fim
#
