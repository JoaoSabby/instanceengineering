
#' Criar parametro BiocParallel seguro para consultas KNN
#' @noRd
<<<<<<< HEAD
sby_create_knn_bioc_parallel_param <- function(sby_knn_workers) {
  if (sby_knn_workers <= 1L) {
=======
# Executa instrucao do fluxo preservado
sby_create_knn_bioc_parallel_param <- function(sby_knn_workers) {
  # Executa instrucao do fluxo preservado
  if (sby_knn_workers <= 1L) {
    # Executa instrucao do fluxo preservado
>>>>>>> origin/main
    return(BiocParallel::SerialParam())
  # Executa instrucao do fluxo preservado
  }

<<<<<<< HEAD
  if (.Platform$OS.type == "windows") {
=======
  # Executa instrucao do fluxo preservado
  if (.Platform$OS.type == "windows") {
    # Executa instrucao do fluxo preservado
>>>>>>> origin/main
    BiocParallel::SnowParam(workers = sby_knn_workers, type = "SOCK")
  # Executa instrucao do fluxo preservado
  } else {
    # Executa instrucao do fluxo preservado
    BiocParallel::MulticoreParam(workers = sby_knn_workers)
  # Executa instrucao do fluxo preservado
  }
# Executa instrucao do fluxo preservado
}

####
## Fim
#
