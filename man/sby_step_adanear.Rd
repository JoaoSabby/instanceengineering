\name{sby_step_adanear}
\alias{sby_step_adanear}
\alias{prep.step_sby_step_adanear}
\alias{bake.step_sby_step_adanear}
\alias{tidy.step_sby_step_adanear}
\alias{print.step_sby_step_adanear}
\alias{required_pkgs.step_sby_step_adanear}
\title{Balance binary classes in recipes with NearMiss-1}
\description{Adds a recipes/tidymodels step that runs \code{sby_nearmiss()} with the unified KNN configuration.}
\usage{sby_step_adanear(sby_recipe, ..., sby_role = NA, sby_trained = FALSE, sby_columns = NULL, sby_under_ratio = 0.5, sby_k_under = 5L, sby_seed = 42L, sby_audit = FALSE, sby_restore_types = TRUE, sby_knn_algorithm = c("auto", "kd_tree", "cover_tree", "brute", "Kmknn", "Vptree", "Exhaustive", "Annoy", "Hnsw"), sby_knn_engine = c("auto", "FNN", "BiocNeighbors", "RcppHNSW"), sby_distance_metric = c("euclidean", "ip", "cosine"), sby_knn_workers = 1L, sby_hnsw_m = 16L, sby_hnsw_ef = 200L, sby_skip = TRUE, sby_id = recipes::rand_id("balance"))}
\arguments{
\item{sby_recipe}{A \code{recipes::recipe()} object.}
\item{...}{Selectors identifying exactly one binary outcome column.}
\item{sby_role, sby_trained, sby_columns, sby_skip, sby_id}{Recipes step metadata.}
\item{sby_under_ratio, sby_k_under, sby_seed, sby_audit, sby_restore_types}{NearMiss and output controls.}
\item{sby_knn_algorithm, sby_knn_engine, sby_distance_metric, sby_knn_workers}{Unified KNN configuration.}
\item{sby_hnsw_m, sby_hnsw_ef}{RcppHNSW index connectivity and effort parameters.}
}
\value{A recipe with an added \code{sby_step_adanear} step.}
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
