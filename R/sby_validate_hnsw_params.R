
#' Validar parametros do backend RcppHNSW
#' @noRd
# Executa instrucao do fluxo preservado
sby_validate_hnsw_params <- function(sby_hnsw_m, sby_hnsw_ef) {
  # Executa instrucao do fluxo preservado
  if (!is.numeric(sby_hnsw_m) || length(sby_hnsw_m) != 1L || is.na(sby_hnsw_m) || !is.finite(sby_hnsw_m) || sby_hnsw_m < 2L) {
    # Executa instrucao do fluxo preservado
    sby_over_under_abort("'sby_hnsw_m' deve ser inteiro >= 2")
  # Executa instrucao do fluxo preservado
  }
  # Executa instrucao do fluxo preservado
  if (!is.numeric(sby_hnsw_ef) || length(sby_hnsw_ef) != 1L || is.na(sby_hnsw_ef) || !is.finite(sby_hnsw_ef) || sby_hnsw_ef < 1L) {
    # Executa instrucao do fluxo preservado
    sby_over_under_abort("'sby_hnsw_ef' deve ser inteiro positivo")
  # Executa instrucao do fluxo preservado
  }

  # Executa instrucao do fluxo preservado
  list(sby_hnsw_m = as.integer(sby_hnsw_m), sby_hnsw_ef = as.integer(sby_hnsw_ef))
# Executa instrucao do fluxo preservado
}

####
## Fim
#
