
#' @export
<<<<<<< HEAD
print.step_sby_step_balance <- function(x, width = max(20, options()$width - 30), ...) {
=======
# Executa instrucao do fluxo preservado
print.step_sby_step_balance <- function(x, width = max(20, options()$width - 30), ...) {
  # Executa instrucao do fluxo preservado
>>>>>>> origin/main
  sby_x <- x
  # Executa instrucao do fluxo preservado
  sby_width <- width
  # Executa instrucao do fluxo preservado
  sby_title <- "Balanceamento NearMiss-1 usando "
<<<<<<< HEAD
  if (isTRUE(sby_x$sby_trained)) {
    sby_columns <- sby_x$sby_columns
  } else {
    sby_columns <- recipes::sel2char(sby_x$sby_terms)
  }
=======
  # Executa instrucao do fluxo preservado
  sby_columns <- if (isTRUE(sby_x$sby_trained)) sby_x$sby_columns else recipes::sel2char(sby_x$sby_terms)
  # Executa instrucao do fluxo preservado
>>>>>>> origin/main
  recipes::print_step(sby_columns, sby_x$sby_terms, sby_x$sby_trained, sby_title, sby_width)
  # Executa instrucao do fluxo preservado
  invisible(sby_x)
# Executa instrucao do fluxo preservado
}

####
## Fim
#
