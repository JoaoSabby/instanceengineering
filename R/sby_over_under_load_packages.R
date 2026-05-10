
#' Carregar dependencias do fluxo de sampling
#' @noRd
<<<<<<< HEAD
sby_over_under_load_packages <- function() {
  if (isTRUE(sby_over_under_state$sby_packages_loaded)) {
=======
# Executa instrucao do fluxo preservado
sby_over_under_load_packages <- function() {
  # Executa instrucao do fluxo preservado
  if (isTRUE(sby_over_under_state$sby_packages_loaded)) {
    # Executa instrucao do fluxo preservado
>>>>>>> origin/main
    return(invisible(TRUE))
  # Executa instrucao do fluxo preservado
  }

  # Executa instrucao do fluxo preservado
  sby_package_names <- c("cli", "Rfast")
<<<<<<< HEAD
  for (sby_package_name in sby_package_names) {
    if (!requireNamespace(sby_package_name, quietly = TRUE)) {
=======
  # Executa instrucao do fluxo preservado
  for (sby_package_name in sby_package_names) {
    # Executa instrucao do fluxo preservado
    if (!requireNamespace(sby_package_name, quietly = TRUE)) {
      # Executa instrucao do fluxo preservado
>>>>>>> origin/main
      sby_over_under_abort(paste0("Pacote necessario nao encontrado: ", sby_package_name))
    # Executa instrucao do fluxo preservado
    }
  # Executa instrucao do fluxo preservado
  }

  # Executa instrucao do fluxo preservado
  sby_over_under_state$sby_packages_loaded <- TRUE
  # Executa instrucao do fluxo preservado
  invisible(TRUE)
# Executa instrucao do fluxo preservado
}

####
## Fim
#
