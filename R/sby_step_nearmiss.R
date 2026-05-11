#' Adicionar uma etapa recipes de balanceamento NearMiss-1
#'
#' @description
#' `sby_step_nearmiss()` adiciona a uma `recipe` uma etapa supervisionada de
#' subamostragem NearMiss-1 para problemas de classificação binária. A etapa é
#' ajustada durante `prep()` a partir de uma única coluna de desfecho selecionada
#' e, por padrão, é ignorada em novos dados durante `bake()` porque modifica o
#' número de linhas do conjunto processado.
#'
#' @details
#' A opção `sby_knn_distance_metric` define a geometria usada pela busca KNN da
#' etapa e deve ser compatível com `sby_knn_engine` e `sby_knn_algorithm`:
#'
#' - `"euclidean"`: distância euclidiana padrão. É exata com `FNN` usando
#'   `"auto"`, `"kd_tree"`, `"cover_tree"` ou `"brute"`; pode ser exata em
#'   `BiocNeighbors` com `"Kmknn"`, `"Vptree"` ou `"Exhaustive"`; pode ser
#'   aproximada em `BiocNeighbors` com `"Annoy"` ou `"Hnsw"`; e é aproximada com
#'   `RcppHNSW`.
#' - `"cosine"`: distância angular. Não é aceita por `FNN`; pode ser exata ou
#'   aproximada em `BiocNeighbors` conforme o algoritmo (`"Kmknn"`, `"Vptree"` e
#'   `"Exhaustive"` exatos; `"Annoy"` e `"Hnsw"` aproximados); e é aproximada em
#'   `RcppHNSW`. O pacote aplica normalização L2 antes da consulta.
#' - `"ip"`: produto interno transformado em distância. É suportado somente por
#'   `RcppHNSW`, sempre com busca aproximada por HNSW; não é compatível com `FNN`
#'   nem `BiocNeighbors`, independentemente de `sby_knn_algorithm`. O pacote
#'   aplica normalização L2 antes da consulta.
#'
#' @param recipe Objeto `recipe` que receberá a etapa de balanceamento.
#' @param ... Seletores `recipes` ou `tidyselect` usados para identificar exatamente uma coluna de desfecho binário.
#' @param role Papel armazenado na etapa para compatibilidade com `recipes`.
#' @param trained Indicador lógico escalar que informa se a etapa já passou por `prep()`.
#' @param columns Vetor de caracteres ou `NULL` com o nome da coluna de desfecho resolvida durante `prep()`.
#' @param sby_under_ratio Valor numérico escalar que controla a fração da classe majoritária retida.
#' @param sby_knn_under_k Número inteiro positivo de vizinhos usados pela etapa NearMiss-1.
#' @param sby_seed Valor numérico inteiro usado para inicializar o gerador pseudoaleatório.
#' @param sby_audit Indicador lógico escalar que controla se metadados de auditoria devem ser preservados.
#' @param sby_restore_types Indicador lógico escalar que define se tipos numéricos originais devem ser restaurados.
#' @param sby_knn_algorithm Algoritmo KNN configurado.
#' @param sby_knn_engine Engine KNN configurado.
#' @param sby_knn_distance_metric Métrica de distância KNN configurada. Consulte os detalhes para compatibilidade entre métrica, engine, algoritmo e tipo de busca.
#' @param sby_knn_workers Número de workers KNN configurado.
#' @param sby_knn_hnsw_m Conectividade HNSW configurada.
#' @param sby_knn_hnsw_ef Lista dinâmica HNSW configurada.
#' @param skip Indicador lógico escalar que define se a etapa deve ser ignorada em novos dados.
#' @param id Identificador recipes da etapa.
#'
#' @return Objeto `recipe` com uma etapa `sby_step_nearmiss` adicionada ao pipeline.
#' @export
sby_step_nearmiss <- function(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  columns = NULL,
  sby_under_ratio = 0.5,
  sby_knn_under_k = 5L,
  sby_seed = sample.int(10L^5L, 1L),
  sby_audit = FALSE,
  sby_restore_types = TRUE,
  sby_knn_algorithm = c("auto", "kd_tree", "cover_tree", "brute", "Kmknn", "Vptree", "Exhaustive", "Annoy", "Hnsw"),
  sby_knn_engine = c("auto", "FNN", "BiocNeighbors", "RcppHNSW"),
  sby_knn_distance_metric = c("euclidean", "ip", "cosine"),
  sby_knn_workers = 1L,
  sby_knn_hnsw_m = 16L,
  sby_knn_hnsw_ef = 200L,
  skip = TRUE,
  id = recipes::rand_id("nearmiss")
){
  
  # Verifica se ha solicitacao de interrupcao antes de configurar a etapa
  sby_adanear_check_user_interrupt()

  # Valida dependencias declaradas para a etapa recipes
  recipes::recipes_pkg_check(
    required_pkgs.step_sby_step_nearmiss()
  )

  # Captura seletores de desfecho informados pelo chamador
  sby_terms <- rlang::enquos(...)

  # Valida indicadores logicos de controle da etapa
  sby_audit <- sby_validate_logical_scalar(sby_value = sby_audit, sby_name = "sby_audit")
  sby_restore_types <- sby_validate_logical_scalar(sby_value = sby_restore_types, sby_name = "sby_restore_types")
  skip <- sby_validate_logical_scalar(sby_value = skip, sby_name = "skip")

  # Resolve opcoes declaradas de algoritmo, engine e metrica KNN
  sby_knn_algorithm <- match.arg(arg = sby_knn_algorithm)
  sby_knn_engine <- match.arg(arg = sby_knn_engine)
  sby_knn_distance_metric <- match.arg(arg = sby_knn_distance_metric)

  # Valida recursos paralelos e parametros HNSW
  sby_knn_workers <- sby_validate_knn_workers(sby_knn_workers = sby_knn_workers)
  sby_hnsw_params <- sby_validate_hnsw_params(sby_knn_hnsw_m = sby_knn_hnsw_m, sby_knn_hnsw_ef = sby_knn_hnsw_ef)

  # Adiciona etapa configurada ao objeto recipe
  return(recipes::add_step(
    rec = recipe,
    object = sby_step_sampling_new(
      sby_subclass                = "sby_step_nearmiss",
      sby_sampling_method         = "nearmiss",
      sby_terms                   = sby_terms,
      sby_role                    = role,
      sby_trained                 = trained,
      sby_columns                 = columns,
      sby_over_ratio              = NA_real_,
      sby_under_ratio             = sby_under_ratio,
      sby_knn_over_k              = NA_integer_,
      sby_knn_under_k             = sby_knn_under_k,
      sby_seed                    = sby_seed,
      sby_audit                   = sby_audit,
      sby_restore_types           = sby_restore_types,
      sby_knn_algorithm           = sby_knn_algorithm,
      sby_knn_engine              = sby_knn_engine,
      sby_knn_distance_metric     = sby_knn_distance_metric,
      sby_knn_workers             = sby_knn_workers,
      sby_knn_hnsw_m              = sby_hnsw_params$sby_knn_hnsw_m,
      sby_knn_hnsw_ef             = sby_hnsw_params$sby_knn_hnsw_ef,
      sby_skip                    = skip,
      sby_id                      = id
    )
  ))
}
####
## Fim
#
