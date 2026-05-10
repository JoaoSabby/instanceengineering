\name{sby_over_under_sampling}
\alias{sby_over_under_sampling}
\alias{sby_apply_adasyn_oversampling}
\alias{sby_apply_nearmiss_undersampling}
\title{High-performance binary over/under sampling}
\description{
Runs binary class-balancing routines using ADASYN oversampling, NearMiss-1
undersampling, or the combined ADASYN + NearMiss pipeline. The package is still
in development and does not preserve compatibility with the previous API names
or return structure.
}
\usage{
sby_over_under_sampling(
  sby_predictor_data,
  sby_target_vector,
  sby_over_ratio = 0.2,
  sby_under_ratio = 0.5,
  sby_k_over = 5L,
  sby_k_under = 5L,
  sby_seed = 42L,
  sby_audit = FALSE,
  sby_restore_types = TRUE,
  sby_knn_algorithm = c("auto", "cover_tree", "kd_tree", "brute"),
  sby_knn_backend = c("auto", "FNN", "BiocNeighbors", "RcppHNSW"),
  sby_knn_workers = 1L,
  sby_bioc_neighbor_algorithm = c("auto", "Kmknn", "Vptree", "Exhaustive", "Annoy", "Hnsw"),
  sby_hnsw_m = 16L,
  sby_hnsw_ef = 200L
)

sby_apply_adasyn_oversampling(
  sby_predictor_data,
  sby_target_vector,
  sby_over_ratio = 0.2,
  sby_k_over = 5L,
  sby_seed = 42L,
  sby_audit = FALSE,
  sby_return_scaled = FALSE,
  sby_restore_types = TRUE,
  sby_knn_algorithm = c("auto", "cover_tree", "kd_tree", "brute"),
  sby_knn_backend = c("auto", "FNN", "BiocNeighbors", "RcppHNSW"),
  sby_knn_workers = 1L,
  sby_bioc_neighbor_algorithm = c("auto", "Kmknn", "Vptree", "Exhaustive", "Annoy", "Hnsw"),
  sby_hnsw_m = 16L,
  sby_hnsw_ef = 200L
)

sby_apply_nearmiss_undersampling(
  sby_predictor_data,
  sby_target_vector,
  sby_under_ratio = 0.5,
  sby_k_under = 5L,
  sby_seed = 42L,
  sby_audit = FALSE,
  sby_precomputed_scaling = NULL,
  sby_input_already_scaled = FALSE,
  sby_restore_types = TRUE,
  sby_type_info = NULL,
  sby_knn_algorithm = c("auto", "cover_tree", "kd_tree", "brute"),
  sby_knn_backend = c("auto", "FNN", "BiocNeighbors", "RcppHNSW"),
  sby_knn_workers = 1L,
  sby_bioc_neighbor_algorithm = c("auto", "Kmknn", "Vptree", "Exhaustive", "Annoy", "Hnsw"),
  sby_hnsw_m = 16L,
  sby_hnsw_ef = 200L
)
}
\arguments{
\item{sby_predictor_data}{A numeric \code{data.frame} or matrix without missing values.}
\item{sby_target_vector}{A binary target vector with at least two observations per class.}
\item{sby_over_ratio}{Minority expansion factor for ADASYN.}
\item{sby_under_ratio}{Fraction of majority observations retained by NearMiss-1.}
\item{sby_k_over, sby_k_under}{Numbers of nearest neighbors used by ADASYN and NearMiss-1.}
\item{sby_seed}{Numeric random seed.}
\item{sby_audit}{Logical. When \code{FALSE}, return only the balanced tibble. When \code{TRUE}, return a list with balanced data, diagnostics, scaling metadata, and type metadata.}
\item{sby_return_scaled}{Logical. When \code{TRUE} and \code{sby_audit = TRUE}, include the scaled ADASYN matrix used by the combined pipeline.}
\item{sby_restore_types}{Whether to restore inferred integer/binary numeric columns.}
\item{sby_precomputed_scaling}{Optional list with \code{centers} and \code{scales}.}
\item{sby_input_already_scaled}{Whether \code{sby_predictor_data} is already scaled.}
\item{sby_type_info}{Optional type metadata produced by these routines.}
\item{sby_knn_algorithm}{FNN algorithm when \code{sby_knn_backend = "FNN"}.}
\item{sby_knn_backend}{Nearest-neighbor backend: \code{"FNN"}, \code{"BiocNeighbors"}, \code{"RcppHNSW"}, or \code{"auto"}.}
\item{sby_knn_workers}{Number of workers/threads for backends that support them.}
\item{sby_bioc_neighbor_algorithm}{BiocNeighbors algorithm. Defaults to exact methods when \code{"auto"}.}
\item{sby_hnsw_m, sby_hnsw_ef}{RcppHNSW index connectivity and search/build effort parameters.}
}
\value{
When \code{sby_audit = FALSE}, a tibble containing the balanced data. The outcome
column is always named \code{TARGET} and is always the first column. When
\code{sby_audit = TRUE}, a list is returned; its \code{sby_balanced_data} element
is the same tibble.
}
\details{
All public function names and parameters use the \code{sby_} prefix and
\code{snake_case}. Previous CamelCase function names are intentionally not kept
because the package is under active development and has not been released to
clients.
Long KNN queries are split into chunks so user interrupts such as Ctrl+C are
checked between chunks. The general chunk size defaults to 1000 query rows and
can be changed with \code{options(instanceengineering.sby_knn_query_chunk_size = 1000L)}.
For \code{sby_knn_backend = "RcppHNSW"}, search chunks default to 100 query rows
and can be changed with \code{options(instanceengineering.sby_hnsw_query_chunk_size = 100L)};
smaller values improve interrupt responsiveness at the cost of more overhead.
}
\examples{
set.seed(1)
sby_x <- data.frame(sby_a = rnorm(40), sby_b = rnorm(40))
sby_y <- factor(c(rep("minority", 10), rep("majority", 30)))
sby_res <- sby_over_under_sampling(sby_x, sby_y, sby_over_ratio = 0.5, sby_under_ratio = 0.8)
sby_res
}
