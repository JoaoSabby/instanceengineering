\name{step_balance}
\alias{step_balance}
\alias{prep.step_balance}
\alias{bake.step_balance}
\alias{tidy.step_balance}
\alias{required_pkgs.step_balance}
\title{Balance binary classes in recipes with NearMiss-1}
\description{
Adds a recipes/tidymodels step that runs only
\code{ApplyNearmissUndersampling()} during recipe processing. The step changes
the number of rows and therefore defaults to \code{skip = TRUE} for new data.
}
\usage{
step_balance(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  columns = NULL,
  underRatio = 0.5,
  kUnder = 5L,
  seed = 42L,
  restoreTypes = TRUE,
  knnAlgorithm = c("auto", "cover_tree", "kd_tree", "brute"),
  knnBackend = c("auto", "FNN", "BiocNeighbors", "RcppHNSW"),
  knnWorkers = 1L,
  biocNeighborAlgorithm = c("auto", "Kmknn", "Vptree", "Exhaustive", "Annoy", "Hnsw"),
  hnswM = 16L,
  hnswEf = 200L,
  skip = TRUE,
  id = recipes::rand_id("balance")
)
}
\arguments{
\item{recipe}{A \code{recipes::recipe()} object.}
\item{...}{Recipes/tidyselect selectors that identify exactly one binary outcome column.}
\item{role}{Step role retained for recipes compatibility.}
\item{trained}{Whether the step has been trained by \code{prep()}.}
\item{columns}{Outcome column selected during \code{prep()}.}
\item{underRatio}{Fraction of majority observations retained by NearMiss-1.}
\item{kUnder}{Number of nearest minority neighbors used by NearMiss-1.}
\item{seed}{Numeric random seed.}
\item{restoreTypes}{Whether to restore inferred integer/binary numeric predictors.}
\item{knnAlgorithm}{FNN algorithm when \code{knnBackend = "FNN"}.}
\item{knnBackend}{Nearest-neighbor backend: \code{"FNN"}, \code{"BiocNeighbors"}, \code{"RcppHNSW"}, or \code{"auto"}.}
\item{knnWorkers}{Number of workers/threads for backends that support them.}
\item{biocNeighborAlgorithm}{BiocNeighbors algorithm.}
\item{hnswM, hnswEf}{RcppHNSW index connectivity and search/build effort parameters.}
\item{skip}{Whether to skip the step when baking new data. Defaults to \code{TRUE}.}
\item{id}{Unique step identifier.}
}
\value{
A recipe with an added \code{step_balance} step. When baked on analysis/training
data, the output is a data frame with majority rows removed according to
NearMiss-1 and with the original column order preserved.
}
\details{
Select the binary outcome in \code{...}, for example
\code{step_balance(recipe, class)}. All remaining columns are passed to
\code{ApplyNearmissUndersampling()} as numeric predictors, so categorical
predictors should be encoded by earlier recipe steps when needed.
}
\examples{
if (requireNamespace("recipes", quietly = TRUE)) {
  set.seed(1)
  dat <- data.frame(a = rnorm(40), b = rnorm(40), y = factor(c(rep("no", 30), rep("yes", 10))))
  rec <- step_balance(recipes::recipe(y ~ ., data = dat), y, underRatio = 0.8)
}
}
