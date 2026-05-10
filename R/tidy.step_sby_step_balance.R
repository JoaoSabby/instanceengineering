
#' @export
tidy.step_sby_step_balance <- function(x, ...) {
  sby_x <- x
  if (isTRUE(sby_x$sby_trained)) {
    sby_terms <- sby_x$sby_columns
  } else {
    sby_terms <- recipes::sel2char(sby_x$sby_terms)
  }
  data.frame(
    sby_terms = sby_terms,
    sby_under_ratio = sby_x$sby_under_ratio,
    sby_k_under = sby_x$sby_k_under,
    sby_audit = sby_x$sby_audit,
    sby_id = sby_x$sby_id,
    stringsAsFactors = FALSE
  )
}

####
## Fim
#
