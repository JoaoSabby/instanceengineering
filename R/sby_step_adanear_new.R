#' Construtor interno de sby_step_adanear
#'
#' @details
#' A funcao implementa uma unidade interna do fluxo de balanceamento com contrato de entrada explicito e retorno controlado
#' A documentacao descreve a intencao operacional para apoiar manutencao, auditoria e revisao tecnica do pacote
#'
#' @param sby_terms Seletores recipes capturados para a etapa
#' @param sby_role Papel da etapa no objeto recipes
#' @param sby_trained Indicador logico de etapa treinada
#' @param sby_columns Colunas selecionadas durante treinamento
#' @param sby_under_ratio Fracao da classe majoritaria retida
#' @param sby_k_under Numero de vizinhos do NearMiss
#' @param sby_seed Semente usada no balanceamento
#' @param sby_audit Indicador logico para retorno de auditoria
#' @param sby_restore_types Indicador logico para restauracao de tipos
#' @param sby_knn_algorithm Algoritmo KNN configurado
#' @param sby_knn_engine Engine KNN configurado
#' @param sby_distance_metric Metrica de distancia KNN configurada
#' @param sby_knn_workers Numero de workers KNN configurado
#' @param sby_hnsw_m Conectividade HNSW configurada
#' @param sby_hnsw_ef Lista dinamica HNSW configurada
#' @param sby_skip Indicador logico de pulo da etapa
#' @param sby_id Identificador recipes da etapa
#'
#' @return Objeto de etapa recipes configurado
#' @noRd
sby_step_adanear_new <- function(
  sby_terms,
  sby_role,
  sby_trained,
  sby_columns,
  sby_under_ratio,
  sby_k_under,
  sby_seed,
  sby_audit,
  sby_restore_types,
  sby_knn_algorithm,
  sby_knn_engine,
  sby_distance_metric,
  sby_knn_workers,
  sby_hnsw_m,
  sby_hnsw_ef,
  sby_skip,
  sby_id
){
  
  # Constroi objeto de etapa recipes com metadados internos de balanceamento
  return(recipes::step(
    subclass                    = "sby_step_adanear",
    terms                       = sby_terms,
    role                        = sby_role,
    trained                     = sby_trained,
    skip                        = sby_skip,
    id                          = sby_id,
    sby_terms                   = sby_terms,
    sby_role                    = sby_role,
    sby_trained                 = sby_trained,
    sby_columns                 = sby_columns,
    sby_under_ratio             = sby_under_ratio,
    sby_k_under                 = sby_k_under,
    sby_seed                    = sby_seed,
    sby_audit                   = sby_audit,
    sby_restore_types           = sby_restore_types,
    sby_knn_algorithm           = sby_knn_algorithm,
    sby_knn_engine             = sby_knn_engine,
    sby_distance_metric         = sby_distance_metric,
    sby_knn_workers             = sby_knn_workers,
    sby_hnsw_m                  = sby_hnsw_m,
    sby_hnsw_ef                 = sby_hnsw_ef,
    sby_skip                    = sby_skip,
    sby_id                      = sby_id
  ))
}
####
## Fim
#
