#' Listar dependencias comuns das etapas recipes de sampling
#'
#' @details
#' Centraliza a declaracao de pacotes usados pelas etapas para evitar divergencia
#' entre ADASYN, NearMiss e ADANEAR. Dependencias opcionais de engines sugeridos
#' continuam validadas no momento em que o engine e selecionado.
#'
#' @return Vetor de nomes de pacotes requeridos pelas etapas recipes
#' @noRd
sby_required_sampling_pkgs <- function(){
  # Retorna dependencias necessarias para construir e executar as etapas padrao
  return(c(
    "instenginer",
    "recipes",
    "generics",
    "rlang",
    "cli",
    "Rfast",
    "tibble",
    "FNN",
    "RcppHNSW"
  ))
}
####
## Fim
#
