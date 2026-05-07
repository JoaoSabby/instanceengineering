#' OverUnderSampling
#'
#' @description
#' Rotinas de over e undersampling binario com foco em robustez, baixo overhead
#' de memoria e interoperabilidade com pipelines de modelagem de alto desempenho.
#'
#' @details
#' Principais decisoes de implementacao:
#' - Evita tibble e dplyr no caminho critico.
#' - Opera internamente em matrix double sempre que possivel.
#' - Permite manter dados escalados entre etapas para reduzir recomputacao.
#' - Permite retornar dados em formato data.frame ou matrix.
#' - Mantem restauracao opcional de tipos para interpretabilidade.
#'
#' @keywords internal
NULL

.OverUnderState <- new.env(parent = emptyenv())
.OverUnderState$packagesLoaded <- FALSE

#' Carregar dependencias do fluxo de sampling
#' @noRd
OverUnderLoadPackages <- function(){
  if(isTRUE(.OverUnderState$packagesLoaded)){
    return(invisible(TRUE))
  }

  packageNames <- c("FNN", "Matrix")
  for(packageName in packageNames){
    if(!requireNamespace(packageName, quietly = TRUE)){
      stop("Pacote necessario nao encontrado: ", packageName, call. = FALSE)
    }
  }

  .OverUnderState$packagesLoaded <- TRUE
  invisible(TRUE)
}

#' Validar entradas de sampling
#' @noRd
ValidateSamplingInputs <- function(
  predictorData,
  targetVector,
  seed
){
  OverUnderLoadPackages()

  if(!is.data.frame(predictorData) && !is.matrix(predictorData)){
    stop("'predictorData' deve ser data.frame ou matrix", call. = FALSE)
  }

  if(NROW(predictorData) == 0L){
    stop("'predictorData' deve conter ao menos uma linha", call. = FALSE)
  }

  if(length(targetVector) != NROW(predictorData)){
    stop("'targetVector' deve ter o mesmo numero de linhas de 'predictorData'", call. = FALSE)
  }

  if(anyNA(predictorData)){
    stop("'predictorData' nao pode conter NA", call. = FALSE)
  }

  if(anyNA(targetVector)){
    stop("'targetVector' nao pode conter NA", call. = FALSE)
  }

  xCheck <- if(is.matrix(predictorData)) predictorData else predictorData
  isNumericColumn <- if(is.matrix(xCheck)) {
    is.numeric(xCheck)
  } else {
    vapply(xCheck, is.numeric, logical(1L))
  }

  if(!all(isNumericColumn)){
    stop("Todos os preditores devem ser numericos", call. = FALSE)
  }

  targetFactor <- as.factor(targetVector)
  if(nlevels(targetFactor) != 2L){
    stop("'targetVector' deve ser binario", call. = FALSE)
  }

  classCounts <- table(targetFactor)
  if(any(classCounts < 2L)){
    stop("Cada classe deve ter ao menos 2 observacoes", call. = FALSE)
  }

  if(!is.numeric(seed) || length(seed) != 1L || is.na(seed)){
    stop("'seed' deve ser escalar numerico", call. = FALSE)
  }

  invisible(TRUE)
}

#' Extrair nomes de colunas de forma segura
#' @noRd
OverUnderGetColumnNames <- function(predictorData){
  columnNames <- colnames(predictorData)
  if(is.null(columnNames) || anyNA(columnNames) || any(columnNames == "")){
    columnNames <- paste0("V", seq_len(NCOL(predictorData)))
  }
  columnNames
}

#' Converter preditores para matrix double sem copias redundantes desnecessarias
#' @noRd
OverUnderAsNumericMatrix <- function(predictorData){
  if(is.matrix(predictorData)){
    xMatrix <- predictorData
    storage.mode(xMatrix) <- "double"
    return(xMatrix)
  }

  xMatrix <- data.matrix(predictorData)
  storage.mode(xMatrix) <- "double"
  xMatrix
}

#' Inferir tipos numericos para restauracao posterior
#' @noRd
InferNumericColumnTypes <- function(dataFrame){
  xMatrix <- OverUnderAsNumericMatrix(dataFrame)
  columnNames <- OverUnderGetColumnNames(dataFrame)

  inferOne <- function(columnData){
    uniqueValues <- sort(unique(columnData))
    if(length(uniqueValues) <= 2L && all(uniqueValues %in% c(0, 1))){
      return("binary")
    }
    isIntegerLike <- all(abs(columnData - round(columnData)) < sqrt(.Machine$double.eps))
    if(isIntegerLike){
      return("integer")
    }
    "double"
  }

  data.frame(
    columnName = columnNames,
    inferredType = vapply(seq_len(NCOL(xMatrix)), function(j) inferOne(xMatrix[, j]), character(1L)),
    stringsAsFactors = FALSE
  )
}

#' Calcular parametros de z-score
#' @noRd
ComputeZScoreParams <- function(xMatrix){
  xMatrix <- OverUnderAsNumericMatrix(xMatrix)
  centers <- colMeans(xMatrix)
  scales <- apply(xMatrix, 2L, stats::sd)

  invalid <- is.na(scales) | scales <= 0
  if(any(invalid)){
    stop(
      paste0(
        "Colunas com desvio padrao zero ou indefinido: ",
        paste(colnames(xMatrix)[invalid], collapse = ", ")
      ),
      call. = FALSE
    )
  }

  list(centers = centers, scales = scales)
}

#' Aplicar z-score em matrix double
#' @noRd
ApplyZScoreScalingMatrix <- function(xMatrix, scalingInfo){
  xMatrix <- OverUnderAsNumericMatrix(xMatrix)
  scaled <- sweep(xMatrix, 2L, scalingInfo$centers, FUN = "-")
  scaled <- sweep(scaled, 2L, scalingInfo$scales, FUN = "/")
  storage.mode(scaled) <- "double"
  scaled
}

#' Reverter z-score em matrix double
#' @noRd
RevertZScoreScalingMatrix <- function(xMatrix, scalingInfo){
  xMatrix <- OverUnderAsNumericMatrix(xMatrix)
  restored <- sweep(xMatrix, 2L, scalingInfo$scales, FUN = "*")
  restored <- sweep(restored, 2L, scalingInfo$centers, FUN = "+")
  storage.mode(restored) <- "double"
  restored
}

#' Restaurar tipos numericos inferidos
#' @noRd
RestoreNumericColumnTypes <- function(xMatrix, typeInfo, asDataFrame = TRUE){
  xMatrix <- OverUnderAsNumericMatrix(xMatrix)
  if(NCOL(xMatrix) != nrow(typeInfo)){
    stop("Inconsistencia entre numero de colunas e typeInfo", call. = FALSE)
  }

  for(j in seq_len(NCOL(xMatrix))){
    inferredType <- typeInfo$inferredType[[j]]
    if(identical(inferredType, "binary")){
      xMatrix[, j] <- ifelse(xMatrix[, j] >= 0.5, 1, 0)
    } else if(identical(inferredType, "integer")){
      xMatrix[, j] <- round(xMatrix[, j])
    }
  }

  if(!asDataFrame){
    return(xMatrix)
  }

  out <- as.data.frame(xMatrix, stringsAsFactors = FALSE)
  names(out) <- typeInfo$columnName

  for(j in seq_len(NCOL(xMatrix))){
    inferredType <- typeInfo$inferredType[[j]]
    if(identical(inferredType, "binary") || identical(inferredType, "integer")){
      out[[j]] <- as.integer(out[[j]])
    } else {
      out[[j]] <- as.numeric(out[[j]])
    }
  }

  out
}

#' Calcular quantidade sintetica da minoria
#' @noRd
ComputeMinorityExpansionCount <- function(targetFactor, overRatio){
  if(!is.numeric(overRatio) || length(overRatio) != 1L || is.na(overRatio) || overRatio < 0){
    stop("'overRatio' deve ser escalar numerico nao negativo", call. = FALSE)
  }

  classCounts <- sort(table(targetFactor))
  minorityCount <- as.integer(classCounts[[1L]])
  syntheticCount <- floor(minorityCount * overRatio)

  if(syntheticCount < 1L){
    stop("'overRatio' gerou zero linhas sinteticas", call. = FALSE)
  }

  syntheticCount
}

#' Calcular quantidade retida da maioria
#' @noRd
ComputeMajorityRetentionCount <- function(targetFactor, underRatio){
  if(!is.numeric(underRatio) || length(underRatio) != 1L || is.na(underRatio) || underRatio <= 0 || underRatio > 1){
    stop("'underRatio' deve estar no intervalo (0, 1]", call. = FALSE)
  }

  classCounts <- sort(table(targetFactor))
  majorityCount <- as.integer(classCounts[[2L]])
  retainedCount <- floor(majorityCount * underRatio)

  if(retainedCount < 1L){
    stop("'underRatio' reteve zero linhas", call. = FALSE)
  }

  retainedCount
}

#' Extrair resultado ADASYN de forma defensiva
#' @noRd
ExtractAdasynResult <- function(adasynResult, predictorColumnCount){
  resultNames <- names(adasynResult)
  predictorMatrixBalanced <- NULL
  targetBalanced <- NULL

  if("X_new" %in% resultNames){
    predictorMatrixBalanced <- adasynResult$X_new
  } else if("X" %in% resultNames){
    predictorMatrixBalanced <- adasynResult$X
  } else if("data" %in% resultNames){
    predictorMatrixBalanced <- adasynResult$data[, seq_len(predictorColumnCount), drop = FALSE]
  } else {
    stop("Nao foi possivel identificar a saida de preditores do ADASYN", call. = FALSE)
  }

  if("target_new" %in% resultNames){
    targetBalanced <- adasynResult$target_new
  } else if("target" %in% resultNames){
    targetBalanced <- adasynResult$target
  } else if("data" %in% resultNames){
    targetBalanced <- adasynResult$data[, predictorColumnCount + 1L, drop = TRUE]
  } else {
    stop("Nao foi possivel identificar a saida do target do ADASYN", call. = FALSE)
  }

  list(
    predictorMatrixBalanced = OverUnderAsNumericMatrix(predictorMatrixBalanced),
    targetBalanced = as.factor(targetBalanced)
  )
}

#' Aplicar oversampling com ADASYN
#'
#' @param predictorData data.frame ou matrix numerica
#' @param targetVector vetor binario
#' @param overRatio fator de expansao relativa da minoria
#' @param kOver numero de vizinhos do ADASYN
#' @param seed semente
#' @param returnScaled logical; retorna tambem matriz escalada
#' @param restoreTypes logical; restaura tipos ao final
#' @param output formato de saida: "data.frame" ou "matrix"
#' @export
ApplyAdasynOversampling <- function(
  predictorData,
  targetVector,
  overRatio = 0.2,
  kOver = 5L,
  seed = 42L,
  returnScaled = FALSE,
  restoreTypes = TRUE,
  output = c("data.frame", "matrix")
){
  output <- match.arg(output)
  ValidateSamplingInputs(predictorData, targetVector, seed)

  if(!requireNamespace("SMOTEWB", quietly = TRUE)){
    stop("Pacote necessario nao encontrado: SMOTEWB", call. = FALSE)
  }

  if(!is.numeric(kOver) || length(kOver) != 1L || is.na(kOver) || kOver < 1L){
    stop("'kOver' deve ser inteiro positivo", call. = FALSE)
  }

  xMatrix <- OverUnderAsNumericMatrix(predictorData)
  colnames(xMatrix) <- OverUnderGetColumnNames(predictorData)
  targetFactor <- as.factor(targetVector)
  typeInfo <- InferNumericColumnTypes(predictorData)
  scalingInfo <- ComputeZScoreParams(xMatrix)
  xScaled <- ApplyZScoreScalingMatrix(xMatrix, scalingInfo)
  syntheticCount <- ComputeMinorityExpansionCount(targetFactor, overRatio)

  set.seed(seed)
  adasynResult <- SMOTEWB::ADASYN(
    x = xScaled,
    y = targetFactor,
    k = as.integer(kOver),
    ovRate = as.integer(syntheticCount)
  )

  extracted <- ExtractAdasynResult(adasynResult, predictorColumnCount = NCOL(xScaled))
  colnames(extracted$predictorMatrixBalanced) <- colnames(xMatrix)

  xRestored <- RevertZScoreScalingMatrix(extracted$predictorMatrixBalanced, scalingInfo)

  finalPredictors <- if(restoreTypes) {
    RestoreNumericColumnTypes(xRestored, typeInfo, asDataFrame = identical(output, "data.frame"))
  } else if(identical(output, "data.frame")) {
    as.data.frame(xRestored, stringsAsFactors = FALSE)
  } else {
    xRestored
  }

  balancedData <- if(identical(output, "data.frame")) {
    out <- finalPredictors
    out$TARGET <- as.factor(extracted$targetBalanced)
    out
  } else {
    list(x = OverUnderAsNumericMatrix(finalPredictors), y = as.factor(extracted$targetBalanced))
  }

  diagnostics <- list(
    inputRows = NROW(xMatrix),
    outputRows = if(identical(output, "data.frame")) nrow(balancedData) else nrow(balancedData$x),
    generatedRows = if(identical(output, "data.frame")) nrow(balancedData) - nrow(xMatrix) else nrow(balancedData$x) - nrow(xMatrix),
    inputClassDistribution = table(targetFactor),
    outputClassDistribution = table(as.factor(extracted$targetBalanced))
  )

  result <- list(
    balancedData = balancedData,
    typeInfo = typeInfo,
    scalingInfo = scalingInfo,
    diagnostics = diagnostics
  )

  if(isTRUE(returnScaled)){
    result$balancedScaled <- list(
      x = extracted$predictorMatrixBalanced,
      y = as.factor(extracted$targetBalanced)
    )
  }

  result
}

#' Aplicar undersampling NearMiss-1
#'
#' @param predictorData data.frame ou matrix numerica
#' @param targetVector vetor binario
#' @param underRatio fracao da maioria retida
#' @param kUnder numero de vizinhos
#' @param seed semente
#' @param precomputedScaling lista opcional com centers e scales
#' @param inputAlreadyScaled logical indicando se predictorData ja esta escalado
#' @param restoreTypes logical; restaura tipos ao final
#' @param output formato de saida: "data.frame" ou "matrix"
#' @param knnAlgorithm algoritmo para FNN::get.knnx
#' @export
ApplyNearmissUndersampling <- function(
  predictorData,
  targetVector,
  underRatio = 0.5,
  kUnder = 5L,
  seed = 42L,
  precomputedScaling = NULL,
  inputAlreadyScaled = FALSE,
  restoreTypes = TRUE,
  output = c("data.frame", "matrix"),
  knnAlgorithm = c("cover_tree", "kd_tree", "brute")
){
  output <- match.arg(output)
  knnAlgorithm <- match.arg(knnAlgorithm)
  ValidateSamplingInputs(predictorData, targetVector, seed)

  if(!is.numeric(kUnder) || length(kUnder) != 1L || is.na(kUnder) || kUnder < 1L){
    stop("'kUnder' deve ser inteiro positivo", call. = FALSE)
  }

  xMatrix <- OverUnderAsNumericMatrix(predictorData)
  colnames(xMatrix) <- OverUnderGetColumnNames(predictorData)
  targetFactor <- as.factor(targetVector)
  typeInfo <- InferNumericColumnTypes(predictorData)

  scalingInfo <- if(isTRUE(inputAlreadyScaled)) {
    precomputedScaling
  } else if(is.null(precomputedScaling)) {
    ComputeZScoreParams(xMatrix)
  } else {
    precomputedScaling
  }

  xScaled <- if(isTRUE(inputAlreadyScaled)) xMatrix else ApplyZScoreScalingMatrix(xMatrix, scalingInfo)

  classCounts <- table(targetFactor)
  minorityLabel <- names(classCounts)[which.min(classCounts)]
  majorityLabel <- names(classCounts)[which.max(classCounts)]

  minorityIndex <- which(targetFactor == minorityLabel)
  majorityIndex <- which(targetFactor == majorityLabel)

  minorityMatrix <- xScaled[minorityIndex, , drop = FALSE]
  majorityMatrix <- xScaled[majorityIndex, , drop = FALSE]

  retainedMajorityCount <- ComputeMajorityRetentionCount(targetFactor, underRatio)
  effectiveK <- min(as.integer(kUnder), nrow(minorityMatrix))
  if(effectiveK < 1L){
    stop("Sem linhas minoritarias suficientes para NearMiss", call. = FALSE)
  }

  set.seed(seed)
  knnResult <- FNN::get.knnx(
    data = minorityMatrix,
    query = majorityMatrix,
    k = effectiveK,
    algorithm = knnAlgorithm
  )

  meanDistances <- rowMeans(knnResult$nn.dist)
  selectedOrder <- order(meanDistances, decreasing = FALSE)
  selectedMajorityIndex <- majorityIndex[selectedOrder[seq_len(retainedMajorityCount)]]
  retainedIndex <- sort(c(minorityIndex, selectedMajorityIndex))

  reducedScaled <- xScaled[retainedIndex, , drop = FALSE]
  reducedTarget <- targetFactor[retainedIndex]
  xRestored <- if(isTRUE(inputAlreadyScaled)) {
    RevertZScoreScalingMatrix(reducedScaled, scalingInfo)
  } else {
    RevertZScoreScalingMatrix(reducedScaled, scalingInfo)
  }

  finalPredictors <- if(restoreTypes) {
    RestoreNumericColumnTypes(xRestored, typeInfo, asDataFrame = identical(output, "data.frame"))
  } else if(identical(output, "data.frame")) {
    as.data.frame(xRestored, stringsAsFactors = FALSE)
  } else {
    xRestored
  }

  balancedData <- if(identical(output, "data.frame")) {
    out <- finalPredictors
    out$TARGET <- as.factor(reducedTarget)
    out
  } else {
    list(x = OverUnderAsNumericMatrix(finalPredictors), y = as.factor(reducedTarget))
  }

  diagnostics <- list(
    inputRows = nrow(xMatrix),
    outputRows = if(identical(output, "data.frame")) nrow(balancedData) else nrow(balancedData$x),
    removedRows = nrow(xMatrix) - if(identical(output, "data.frame")) nrow(balancedData) else nrow(balancedData$x),
    inputClassDistribution = table(targetFactor),
    outputClassDistribution = table(as.factor(reducedTarget))
  )

  list(
    balancedData = balancedData,
    typeInfo = typeInfo,
    scalingInfo = scalingInfo,
    diagnostics = diagnostics,
    balancedScaled = list(x = reducedScaled, y = as.factor(reducedTarget))
  )
}

#' Pipeline combinado ADASYN + NearMiss
#'
#' @param predictorData data.frame ou matrix numerica
#' @param targetVector vetor binario
#' @param overRatio fator de oversampling
#' @param underRatio fator de undersampling
#' @param kOver vizinhos ADASYN
#' @param kUnder vizinhos NearMiss
#' @param seed semente
#' @param restoreTypes logical; restaura tipos na saida final
#' @param output formato final
#' @param knnAlgorithm algoritmo FNN
#' @export
OverUnderSampling <- function(
  predictorData,
  targetVector,
  overRatio = 0.2,
  underRatio = 0.5,
  kOver = 5L,
  kUnder = 5L,
  seed = 42L,
  restoreTypes = TRUE,
  output = c("data.frame", "matrix"),
  knnAlgorithm = c("cover_tree", "kd_tree", "brute")
){
  output <- match.arg(output)
  knnAlgorithm <- match.arg(knnAlgorithm)

  overResult <- ApplyAdasynOversampling(
    predictorData = predictorData,
    targetVector = targetVector,
    overRatio = overRatio,
    kOver = kOver,
    seed = seed,
    returnScaled = TRUE,
    restoreTypes = FALSE,
    output = "matrix"
  )

  underResult <- ApplyNearmissUndersampling(
    predictorData = overResult$balancedScaled$x,
    targetVector = overResult$balancedScaled$y,
    underRatio = underRatio,
    kUnder = kUnder,
    seed = seed,
    precomputedScaling = overResult$scalingInfo,
    inputAlreadyScaled = TRUE,
    restoreTypes = restoreTypes,
    output = output,
    knnAlgorithm = knnAlgorithm
  )

  diagnostics <- list(
    originalRows = NROW(predictorData),
    afterOversamplingRows = nrow(overResult$balancedScaled$x),
    finalRows = if(identical(output, "data.frame")) nrow(underResult$balancedData) else nrow(underResult$balancedData$x),
    originalClassDistribution = table(as.factor(targetVector)),
    afterOversamplingClassDistribution = table(overResult$balancedScaled$y),
    finalClassDistribution = table(if(identical(output, "data.frame")) underResult$balancedData$TARGET else underResult$balancedData$y)
  )

  list(
    oversamplingResult = overResult,
    undersamplingResult = underResult,
    balancedData = underResult$balancedData,
    diagnostics = diagnostics
  )
}
