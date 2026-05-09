
#' Balancear dados em uma recipe com NearMiss-1
#'
#' @description
#' `sby_step_balance()` adiciona uma etapa de balanceamento para recipes/tidymodels.
#' A etapa executa `sby_apply_nearmiss_undersampling()` e, por padrao,
#' e pulada em novos dados (`sby_skip = TRUE`), pois altera o numero de linhas.
#'
#' @param sby_recipe Objeto recipe.
#' @param ... Seletores recipes/tidyselect que identificam uma unica coluna de desfecho binario.
#' @param sby_role Role da etapa. Mantido por compatibilidade com recipes.
#' @param sby_trained Logical indicando se a etapa ja foi treinada.
#' @param sby_columns Coluna de desfecho selecionada durante `prep()`.
#' @param sby_under_ratio Fracao da classe majoritaria retida pelo NearMiss-1.
#' @param sby_k_under Numero de vizinhos usado pelo NearMiss-1.
#' @param sby_seed Semente usada pela rotina de undersampling.
#' @param sby_audit Logical; quando TRUE o bake retorna a lista de auditoria.
#' @param sby_restore_types Logical; restaura tipos numericos inferidos ao final.
#' @param sby_knn_algorithm Algoritmo para `FNN::get.knnx()` quando `sby_knn_backend = "FNN"`.
#' @param sby_knn_backend Backend KNN: `"FNN"`, `"BiocNeighbors"`, `"RcppHNSW"` ou `"auto"`.
#' @param sby_knn_workers Numero de workers para backends com suporte.
#' @param sby_bioc_neighbor_algorithm Algoritmo BiocNeighbors.
#' @param sby_hnsw_m,sby_hnsw_ef Parametros do indice RcppHNSW.
#' @param sby_skip Logical; a etapa deve ser pulada ao aplicar a recipe em novos dados.
#' @param sby_id Identificador unico da etapa.
#' @return Um objeto recipe com a etapa adicionada.
#' @export
sby_step_balance <- function(
  sby_recipe,
  ...,
  sby_role = NA,
  sby_trained = FALSE,
  sby_columns = NULL,
  sby_under_ratio = 0.5,
  sby_k_under = 5L,
  sby_seed = 42L,
  sby_audit = FALSE,
  sby_restore_types = TRUE,
  sby_knn_algorithm = c("auto", "cover_tree", "kd_tree", "brute"),
  sby_knn_backend = c("auto", "FNN", "BiocNeighbors", "RcppHNSW"),
  sby_knn_workers = 1L,
  sby_bioc_neighbor_algorithm = c("auto", "Kmknn", "Vptree", "Exhaustive", "Annoy", "Hnsw"),
  sby_hnsw_m = 16L,
  sby_hnsw_ef = 200L,
  sby_skip = TRUE,
  sby_id = recipes::rand_id("balance")
){
  sby_over_under_check_user_interrupt()
  recipes::recipes_pkg_check(required_pkgs.step_sby_step_balance())

  sby_terms <- rlang::enquos(...)
  sby_audit <- sby_validate_logical_scalar(sby_audit, "sby_audit")
  sby_restore_types <- sby_validate_logical_scalar(sby_restore_types, "sby_restore_types")
  sby_skip <- sby_validate_logical_scalar(sby_skip, "sby_skip")
  sby_knn_algorithm <- match.arg(sby_knn_algorithm)
  sby_knn_backend <- match.arg(sby_knn_backend)
  sby_bioc_neighbor_algorithm <- match.arg(sby_bioc_neighbor_algorithm)
  sby_knn_workers <- sby_validate_knn_workers(sby_knn_workers)
  sby_hnsw_params <- sby_validate_hnsw_params(sby_hnsw_m, sby_hnsw_ef)

  recipes::add_step(
    sby_recipe,
    sby_step_balance_new(
      sby_terms = sby_terms,
      sby_role = sby_role,
      sby_trained = sby_trained,
      sby_columns = sby_columns,
      sby_under_ratio = sby_under_ratio,
      sby_k_under = sby_k_under,
      sby_seed = sby_seed,
      sby_audit = sby_audit,
      sby_restore_types = sby_restore_types,
      sby_knn_algorithm = sby_knn_algorithm,
      sby_knn_backend = sby_knn_backend,
      sby_knn_workers = sby_knn_workers,
      sby_bioc_neighbor_algorithm = sby_bioc_neighbor_algorithm,
      sby_hnsw_m = sby_hnsw_params$sby_hnsw_m,
      sby_hnsw_ef = sby_hnsw_params$sby_hnsw_ef,
      sby_skip = sby_skip,
      sby_id = sby_id
    )
  )
}
