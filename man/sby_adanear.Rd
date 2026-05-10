\name{sby_adanear}
\alias{sby_adanear}
\alias{sby_adasyn}
\alias{sby_nearmiss}
\title{High-performance binary over/under sampling}
\description{Runs ADASYN oversampling, NearMiss-1 undersampling, or the combined ADASYN + NearMiss pipeline with a unified KNN engine, algorithm, and distance-metric interface.}
\usage{
sby_adanear(sby_predictor_data, sby_target_vector, sby_over_ratio = 0.2, sby_under_ratio = 0.5, sby_k_over = 5L, sby_k_under = 5L, sby_seed = 42L, sby_audit = FALSE, sby_restore_types = TRUE, sby_knn_algorithm = c("auto", "kd_tree", "cover_tree", "brute", "Kmknn", "Vptree", "Exhaustive", "Annoy", "Hnsw"), sby_knn_engine = c("auto", "FNN", "BiocNeighbors", "RcppHNSW"), sby_distance_metric = c("euclidean", "ip", "cosine"), sby_knn_workers = 1L, sby_hnsw_m = 16L, sby_hnsw_ef = 200L)
sby_adasyn(sby_predictor_data, sby_target_vector, sby_over_ratio = 0.2, sby_k_over = 5L, sby_seed = 42L, sby_audit = FALSE, sby_return_scaled = FALSE, sby_restore_types = TRUE, sby_knn_algorithm = c("auto", "kd_tree", "cover_tree", "brute", "Kmknn", "Vptree", "Exhaustive", "Annoy", "Hnsw"), sby_knn_engine = c("auto", "FNN", "BiocNeighbors", "RcppHNSW"), sby_distance_metric = c("euclidean", "ip", "cosine"), sby_knn_workers = 1L, sby_hnsw_m = 16L, sby_hnsw_ef = 200L)
sby_nearmiss(sby_predictor_data, sby_target_vector, sby_under_ratio = 0.5, sby_k_under = 5L, sby_seed = 42L, sby_audit = FALSE, sby_precomputed_scaling = NULL, sby_input_already_scaled = FALSE, sby_restore_types = TRUE, sby_type_info = NULL, sby_knn_algorithm = c("auto", "kd_tree", "cover_tree", "brute", "Kmknn", "Vptree", "Exhaustive", "Annoy", "Hnsw"), sby_knn_engine = c("auto", "FNN", "BiocNeighbors", "RcppHNSW"), sby_distance_metric = c("euclidean", "ip", "cosine"), sby_knn_workers = 1L, sby_hnsw_m = 16L, sby_hnsw_ef = 200L)
}
\arguments{
\item{sby_predictor_data}{Numeric predictors as a data frame or matrix.}
\item{sby_target_vector}{Binary target vector.}
\item{sby_over_ratio, sby_under_ratio}{Oversampling expansion and majority-retention ratios.}
\item{sby_k_over, sby_k_under}{Nearest-neighbor counts for ADASYN and NearMiss-1.}
\item{sby_seed}{Random seed.}
\item{sby_audit, sby_return_scaled, sby_restore_types}{Logical controls for audit output, scaled matrix retention, and numeric type restoration.}
\item{sby_precomputed_scaling, sby_input_already_scaled, sby_type_info}{Optional scaling and type metadata for internal chaining.}
\item{sby_knn_algorithm}{Unified KNN algorithm selector.}
\item{sby_knn_engine}{KNN engine: \code{"auto"}, \code{"FNN"}, \code{"BiocNeighbors"}, or \code{"RcppHNSW"}.}
\item{sby_distance_metric}{Distance metric: \code{"euclidean"}, \code{"ip"}, or \code{"cosine"}.}
\item{sby_knn_workers}{Number of KNN workers/threads.}
\item{sby_hnsw_m, sby_hnsw_ef}{RcppHNSW index connectivity and effort parameters.}
}
\value{A balanced tibble when \code{sby_audit = FALSE}; otherwise an audit list.}
\details{The KNN interface is unified by \code{sby_knn_engine}, \code{sby_knn_algorithm}, and
\code{sby_distance_metric}. Z-score standardization is applied before nearest-neighbor
search; when \code{sby_distance_metric} is \code{"ip"} or \code{"cosine"}, the reference
and query matrices are also internally L2-normalized after Z-score scaling and before
submission to the KNN engine.

\tabular{llll}{
Engine \tab Algorithms \tab Search type \tab Supported metrics\cr
FNN \tab \code{auto}, \code{kd_tree}, \code{cover_tree}, \code{brute} \tab Exact \tab \code{euclidean}\cr
BiocNeighbors \tab \code{Kmknn}, \code{Vptree}, \code{Exhaustive} \tab Exact \tab \code{euclidean}, \code{cosine}\cr
BiocNeighbors \tab \code{Annoy}, \code{Hnsw} \tab Approximate \tab \code{euclidean}, \code{cosine}\cr
RcppHNSW \tab internally managed \tab Approximate \tab \code{euclidean}, \code{cosine}, \code{ip}\cr
}

\code{euclidean} is the straight-line distance in continuous space,
\eqn{d(x, y) = \sqrt{\sum (x_i - y_i)^2}}. \code{ip} is inner-product distance,
\eqn{d(x, y) = 1 - \sum x_i y_i}, and triggers mandatory internal L2 normalization.
\code{cosine} is angular distance,
\eqn{d(x, y) = 1 - \frac{\sum x_i y_i}{\sqrt{\sum x_i^2} \sqrt{\sum y_i^2}}},
and also uses internal L2 normalization for the non-Euclidean search path. FNN is
explicitly blocked for \code{ip} and \code{cosine}; \code{ip} is accepted only by
\code{sby_knn_engine = "RcppHNSW"}.}
\references{
He, H., Bai, Y., Garcia, E. A., \& Li, S. (2008). ADASYN: Adaptive synthetic sampling approach for imbalanced learning. IEEE International Joint Conference on Neural Networks.

Malkov, Y. A., \& Yashunin, D. A. (2018). Efficient and robust approximate nearest neighbor search using hierarchical navigable small world graphs. IEEE Transactions on Pattern Analysis and Machine Intelligence.
}
