
#' Criar parametro BiocParallel seguro para consultas KNN
#' @noRd
sby_create_knn_bioc_parallel_param <- function(sby_knn_workers){
  if(sby_knn_workers <= 1L){
    return(BiocParallel::SerialParam())
  }

  if(.Platform$OS.type == "windows"){
    BiocParallel::SnowParam(workers = sby_knn_workers, type = "SOCK")
  } else {
    BiocParallel::MulticoreParam(workers = sby_knn_workers)
  }
}
