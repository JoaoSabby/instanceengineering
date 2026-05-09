
#' @export
print.step_sby_step_balance <- function(x, width = max(20, options()$width - 30), ...){
  sby_x <- x
  sby_width <- width
  sby_title <- "Balanceamento NearMiss-1 usando "
  sby_columns <- if(isTRUE(sby_x$sby_trained)) sby_x$sby_columns else recipes::sel2char(sby_x$sby_terms)
  recipes::print_step(sby_columns, sby_x$sby_terms, sby_x$sby_trained, sby_title, sby_width)
  invisible(sby_x)
}
