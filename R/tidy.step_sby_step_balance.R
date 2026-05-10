
#' @export
# Executa instrucao do fluxo preservado
tidy.step_sby_step_balance <- function(x, ...) {
  # Executa instrucao do fluxo preservado
  sby_x <- x
  # Executa instrucao do fluxo preservado
  sby_terms <- if (isTRUE(sby_x$sby_trained)) sby_x$sby_columns else recipes::sel2char(sby_x$sby_terms)
  # Executa instrucao do fluxo preservado
  data.frame(
    # Executa instrucao do fluxo preservado
    sby_terms = sby_terms,
    # Executa instrucao do fluxo preservado
    sby_under_ratio = sby_x$sby_under_ratio,
    # Executa instrucao do fluxo preservado
    sby_k_under = sby_x$sby_k_under,
    # Executa instrucao do fluxo preservado
    sby_audit = sby_x$sby_audit,
    # Executa instrucao do fluxo preservado
    sby_id = sby_x$sby_id,
    # Executa instrucao do fluxo preservado
    stringsAsFactors = FALSE
  # Executa instrucao do fluxo preservado
  )
# Executa instrucao do fluxo preservado
}

####
## Fim
#
