\name{sby_step_nearmiss}
\alias{sby_step_nearmiss}
\title{Adicionar uma etapa recipes de balanceamento NearMiss-1}
\description{
\code{sby_step_nearmiss()} adiciona a uma \code{recipe} uma etapa supervisionada de subamostragem NearMiss-1.
}
\usage{
sby_step_nearmiss(
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
)
}
\arguments{
\item{recipe}{Objeto \code{recipe} que receberá a etapa de balanceamento.}
\item{...}{Seletores \code{recipes} ou \code{tidyselect} usados para identificar exatamente uma coluna de desfecho binário.}
\item{role}{Papel armazenado na etapa para compatibilidade com \code{recipes}.}
\item{trained}{Indicador lógico escalar que informa se a etapa já passou por \code{prep()}.}
\item{columns}{Vetor de caracteres ou \code{NULL} com o nome da coluna de desfecho resolvida durante \code{prep()}.}
\item{sby_seed}{Valor numérico inteiro usado para inicializar o gerador pseudoaleatório. O padrão é \code{sample.int(10L^5L, 1L)}.}
\item{sby_audit}{Indicador lógico escalar que controla se metadados de auditoria devem ser preservados.}
\item{sby_restore_types}{Indicador lógico escalar que define se tipos numéricos originais devem ser restaurados.}
\item{sby_knn_algorithm}{Algoritmo KNN configurado.}
\item{sby_knn_engine}{Engine KNN configurado.}
\item{sby_knn_distance_metric}{Métrica de distância KNN configurada.}
\item{sby_knn_workers}{Número de workers KNN configurado.}
\item{sby_knn_hnsw_m}{Conectividade HNSW configurada.}
\item{sby_knn_hnsw_ef}{Lista dinâmica HNSW configurada.}
\item{skip}{Indicador lógico escalar que define se a etapa deve ser ignorada em novos dados.}
\item{id}{Identificador recipes da etapa.}
\item{sby_under_ratio}{Valor numérico escalar que controla a fração da classe majoritária retida.}
\item{sby_knn_under_k}{Número inteiro positivo de vizinhos usados pela etapa NearMiss-1.}
}
\value{Objeto \code{recipe} com uma etapa \code{sby_step_nearmiss} adicionada ao pipeline.}
