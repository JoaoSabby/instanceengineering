
#' Verificar se a biblioteca nativa do pacote esta carregada
#' @noRd
sby_over_under_native_available <- function() {
  is.loaded("OU_GenerateSyntheticAdasynC", PACKAGE = "instanceengineering")
}

####
## Fim
#
