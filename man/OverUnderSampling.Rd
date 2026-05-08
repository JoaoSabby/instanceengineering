\name{OverUnderSampling}
\alias{OverUnderSampling}
\alias{ApplyAdasynOversampling}
\alias{ApplyNearmissUndersampling}
\title{High-performance binary over/under sampling}
\description{
Apply ADASYN oversampling, NearMiss-1 undersampling, or the combined pipeline
for binary classification problems with numeric predictors.
}
\usage{
OverUnderSampling(
  predictorData,
  targetVector,
  overRatio = 0.2,
  underRatio = 0.5,
  kOver = 5L,
  kUnder = 5L,
  seed = 42L,
  restoreTypes = TRUE,
  output = c("data.frame", "matrix"),
  knnAlgorithm = c("auto", "cover_tree", "kd_tree", "brute"),
  knnBackend = c("auto", "FNN", "BiocNeighbors", "RcppHNSW"),
  knnWorkers = 1L,
  biocNeighborAlgorithm = c("auto", "Kmknn", "Vptree", "Exhaustive", "Annoy", "Hnsw"),
  hnswM = 16L,
  hnswEf = 200L
)

ApplyAdasynOversampling(
  predictorData,
  targetVector,
  overRatio = 0.2,
  kOver = 5L,
  seed = 42L,
  returnScaled = FALSE,
  restoreTypes = TRUE,
  output = c("data.frame", "matrix"),
  knnAlgorithm = c("auto", "cover_tree", "kd_tree", "brute"),
  knnBackend = c("auto", "FNN", "BiocNeighbors", "RcppHNSW"),
  knnWorkers = 1L,
  biocNeighborAlgorithm = c("auto", "Kmknn", "Vptree", "Exhaustive", "Annoy", "Hnsw"),
  hnswM = 16L,
  hnswEf = 200L
)

ApplyNearmissUndersampling(
  predictorData,
  targetVector,
  underRatio = 0.5,
  kUnder = 5L,
  seed = 42L,
  precomputedScaling = NULL,
  inputAlreadyScaled = FALSE,
  restoreTypes = TRUE,
  typeInfo = NULL,
  output = c("data.frame", "matrix"),
  knnAlgorithm = c("auto", "cover_tree", "kd_tree", "brute"),
  knnBackend = c("auto", "FNN", "BiocNeighbors", "RcppHNSW"),
  knnWorkers = 1L,
  biocNeighborAlgorithm = c("auto", "Kmknn", "Vptree", "Exhaustive", "Annoy", "Hnsw"),
  hnswM = 16L,
  hnswEf = 200L
)
}
\arguments{
\item{predictorData}{A numeric \code{data.frame} or matrix without missing values.}
\item{targetVector}{A binary target vector with at least two observations per class.}
\item{overRatio}{Minority expansion factor for ADASYN.}
\item{underRatio}{Fraction of majority observations retained by NearMiss-1.}
\item{kOver, kUnder}{Numbers of nearest neighbors used by ADASYN and NearMiss-1.}
\item{seed}{Numeric random seed.}
\item{returnScaled}{Whether to include the scaled ADASYN output.}
\item{restoreTypes}{Whether to restore inferred integer/binary numeric columns.}
\item{precomputedScaling}{Optional list with \code{centers} and \code{scales}.}
\item{inputAlreadyScaled}{Whether \code{predictorData} is already scaled.}
\item{typeInfo}{Optional type metadata produced by these routines.}
\item{output}{Output format: \code{"data.frame"} or \code{"matrix"}.}
\item{knnAlgorithm}{FNN algorithm when \code{knnBackend = "FNN"}.}
\item{knnBackend}{Nearest-neighbor backend: \code{"FNN"}, \code{"BiocNeighbors"}, \code{"RcppHNSW"}, or \code{"auto"}.}
\item{knnWorkers}{Number of workers/threads for backends that support them.}
\item{biocNeighborAlgorithm}{BiocNeighbors algorithm. Defaults to exact methods when \code{"auto"}.}
\item{hnswM, hnswEf}{RcppHNSW index connectivity and search/build effort parameters.}
}
\value{
A list containing balanced data, diagnostics, scaling information, and type
metadata. Matrix output returns \code{list(x, y)} in \code{balancedData};
data-frame output appends a \code{TARGET} factor column.
}
\details{
The package uses native C routines for scaling and ADASYN generation when the
installed shared library is available. Otherwise it falls back to Rfast. The
\code{RcppHNSW} backend is approximate and can change selected neighbors; use it
only when that trade-off is acceptable.
}
\examples{
set.seed(1)
x <- data.frame(a = rnorm(40), b = rnorm(40))
y <- factor(c(rep("minority", 10), rep("majority", 30)))
res <- OverUnderSampling(x, y, overRatio = 0.5, underRatio = 0.8, output = "matrix")
res$diagnostics
}
