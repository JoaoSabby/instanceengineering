
#' Verificar interrupcao solicitada pelo usuario
#' @noRd
# Executa instrucao do fluxo preservado
sby_over_under_check_user_interrupt <- function() {
  # Executa instrucao do fluxo preservado
  if (sby_over_under_native_available()) {
    # Executa instrucao do fluxo preservado
    .Call("OU_CheckUserInterruptC", PACKAGE = "instanceengineering")
  # Executa instrucao do fluxo preservado
  } else {
    # Executa instrucao do fluxo preservado
    Sys.sleep(0)
  # Executa instrucao do fluxo preservado
  }

  # Executa instrucao do fluxo preservado
  invisible(TRUE)
# Executa instrucao do fluxo preservado
}

####
## Fim
#
