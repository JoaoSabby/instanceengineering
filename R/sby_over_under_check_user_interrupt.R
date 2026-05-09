
#' Verificar interrupcao solicitada pelo usuario
#' @noRd
sby_over_under_check_user_interrupt <- function(){
  if(sby_over_under_native_available()){
    .Call("OU_CheckUserInterruptC", PACKAGE = "instanceengineering")
  } else {
    Sys.sleep(0)
  }

  invisible(TRUE)
}
