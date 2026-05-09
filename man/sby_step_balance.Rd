\name{sby_step_balance}
\alias{sby_step_balance}
\alias{prep.step_sby_step_balance}
\alias{bake.step_sby_step_balance}
\alias{tidy.step_sby_step_balance}
\alias{print.step_sby_step_balance}
\alias{required_pkgs.step_sby_step_balance}
\title{Balance binary classes in recipes with NearMiss-1}
\description{
Adds a recipes/tidymodels step that runs \code{sby_apply_nearmiss_undersampling()}
during recipe processing. The step changes the number of rows and therefore
defaults to \code{sby_skip = TRUE} for new data.
}
\usage{
sby_step_balance(
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
)
}
\arguments{
\item{sby_recipe}{A \code{recipes::recipe()} object.}
\item{...}{Recipes/tidyselect selectors that identify exactly one binary outcome column.}
\item{sby_role}{Step role retained for recipes compatibility.}
\item{sby_trained}{Whether the step has been trained by \code{prep()}.}
\item{sby_columns}{Outcome column selected during \code{prep()}.}
\item{sby_under_ratio}{Fraction of majority observations retained by NearMiss-1.}
\item{sby_k_under}{Number of nearest minority neighbors used by NearMiss-1.}
\item{sby_seed}{Numeric random seed.}
\item{sby_audit}{Logical. When \code{FALSE}, \code{bake()} returns the balanced tibble. When \code{TRUE}, \code{bake()} returns the full audit list produced by \code{sby_apply_nearmiss_undersampling()}.}
\item{sby_restore_types}{Whether to restore inferred integer/binary numeric predictors.}
\item{sby_knn_algorithm}{FNN algorithm when \code{sby_knn_backend = "FNN"}.}
\item{sby_knn_backend}{Nearest-neighbor backend: \code{"FNN"}, \code{"BiocNeighbors"}, \code{"RcppHNSW"}, or \code{"auto"}.}
\item{sby_knn_workers}{Number of workers/threads for backends that support them.}
\item{sby_bioc_neighbor_algorithm}{BiocNeighbors algorithm.}
\item{sby_hnsw_m, sby_hnsw_ef}{RcppHNSW index connectivity and search/build effort parameters.}
\item{sby_skip}{Whether to skip the step when baking new data. Defaults to \code{TRUE}.}
\item{sby_id}{Unique step identifier.}
}
\value{
A recipe with an added \code{sby_step_balance} step. When baked with
\code{sby_audit = FALSE}, the output is a tibble with the \code{TARGET} outcome column first.
When baked with \code{sby_audit = TRUE}, the output is the audit list.
}
\details{
Select the binary outcome in \code{...}, for example
\code{sby_step_balance(sby_recipe, y)}. All remaining columns are passed to
\code{sby_apply_nearmiss_undersampling()} as numeric predictors, so categorical
predictors should be encoded by earlier recipe steps when needed.
}
\examples{
if (requireNamespace("recipes", quietly = TRUE)) {
  set.seed(1)
  sby_dat <- data.frame(sby_a = rnorm(40), sby_b = rnorm(40), y = factor(c(rep("no", 30), rep("yes", 10))))
  sby_rec <- sby_step_balance(recipes::recipe(y ~ ., data = sby_dat), y, sby_under_ratio = 0.8)
}
}
