
#' Carregar dependencias do fluxo de sampling
#' @noRd
sby_over_under_load_packages <- function() {
  if (isTRUE(sby_over_under_state$sby_packages_loaded)) {
    return(invisible(TRUE))
  }

  sby_package_names <- c("cli", "Rfast")
  for (sby_package_name in sby_package_names) {
    if (!requireNamespace(sby_package_name, quietly = TRUE)) {
      sby_over_under_abort(paste0("Pacote necessario nao encontrado: ", sby_package_name))
    }
  }

  sby_over_under_state$sby_packages_loaded <- TRUE
  invisible(TRUE)
}

####
## Fim
#
