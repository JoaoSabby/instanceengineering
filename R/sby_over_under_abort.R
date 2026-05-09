
#' Emitir erro padronizado com cli
#' @noRd
sby_over_under_abort <- function(sby_message){
  if(requireNamespace("cli", quietly = TRUE)){
    cli::cli_abort(sby_message, call = NULL)
  }
  stop(sby_message, call. = FALSE)
}
