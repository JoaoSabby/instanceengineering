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

  packageNames <- "FNN"
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

  if(!is.numeric(seed) || length(seed) != 1L || is.na(seed) || !is.finite(seed)){
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


#' Identificar classes minoritaria e majoritaria
#' @noRd
GetBinaryClassRoles <- function(targetFactor){
  classCounts <- table(targetFactor)
  if(length(classCounts) != 2L){
    stop("'targetVector' deve ser binario", call. = FALSE)
  }
  if(classCounts[[1L]] == classCounts[[2L]]){
    stop("As rotinas de sampling requerem classes desbalanceadas", call. = FALSE)
  }

  list(
    classCounts = classCounts,
    minorityLabel = names(classCounts)[which.min(classCounts)],
    majorityLabel = names(classCounts)[which.max(classCounts)]
  )
}

#' Validar parametros de escala precomputados
#' @noRd
ValidateScalingInfo <- function(scalingInfo, predictorColumnCount){
  if(!is.list(scalingInfo) || is.null(scalingInfo$centers) || is.null(scalingInfo$scales)){
    stop("'precomputedScaling' deve conter 'centers' e 'scales'", call. = FALSE)
  }
  if(length(scalingInfo$centers) != predictorColumnCount || length(scalingInfo$scales) != predictorColumnCount){
    stop("'precomputedScaling' deve ter um centro e uma escala por coluna", call. = FALSE)
  }
  if(anyNA(scalingInfo$centers) || anyNA(scalingInfo$scales) || any(!is.finite(scalingInfo$centers)) || any(!is.finite(scalingInfo$scales))){
    stop("'precomputedScaling' contem valores ausentes ou infinitos", call. = FALSE)
  }
  if(any(scalingInfo$scales <= 0)){
    stop("'precomputedScaling$scales' deve conter apenas valores positivos", call. = FALSE)
  }
  invisible(TRUE)
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

  centered <- sweep(xMatrix, 2L, centers, FUN = "-")
  scales <- sqrt(colSums(centered * centered) / (nrow(xMatrix) - 1L))

  invalid <- is.na(scales) | !is.finite(scales) | scales <= 0
  if(any(invalid)){
    columnNames <- colnames(xMatrix)
    if(is.null(columnNames)){
      columnNames <- paste0("V", seq_len(NCOL(xMatrix)))
    }
    stop(
      paste0(
        "Colunas com desvio padrao zero ou indefinido: ",
        paste(columnNames[invalid], collapse = ", ")
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
  ValidateScalingInfo(scalingInfo, NCOL(xMatrix))
  scaled <- sweep(xMatrix, 2L, scalingInfo$centers, FUN = "-")
  scaled <- sweep(scaled, 2L, scalingInfo$scales, FUN = "/")
  storage.mode(scaled) <- "double"
  scaled
}

#' Reverter z-score em matrix double
#' @noRd
RevertZScoreScalingMatrix <- function(xMatrix, scalingInfo){
  xMatrix <- OverUnderAsNumericMatrix(xMatrix)
  ValidateScalingInfo(scalingInfo, NCOL(xMatrix))
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

  classRoles <- GetBinaryClassRoles(targetFactor)
  minorityCount <- as.integer(classRoles$classCounts[classRoles$minorityLabel])
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

  classRoles <- GetBinaryClassRoles(targetFactor)
  majorityCount <- as.integer(classRoles$classCounts[classRoles$majorityLabel])
  retainedCount <- floor(majorityCount * underRatio)

  if(retainedCount < 1L){
    stop("'underRatio' reteve zero linhas", call. = FALSE)
  }

  retainedCount
}

#' Gerar amostras sinteticas ADASYN em matriz ja escalada
#' @noRd
GenerateAdasynSamples <- function(xScaled, targetFactor, syntheticCount, kOver, knnAlgorithm){
  classRoles <- GetBinaryClassRoles(targetFactor)
  minorityIndex <- which(targetFactor == classRoles$minorityLabel)
  minorityMatrix <- xScaled[minorityIndex, , drop = FALSE]

  effectiveAllK <- min(as.integer(kOver) + 1L, nrow(xScaled))
  allNeighborResult <- FNN::get.knnx(
    data = xScaled,
    query = minorityMatrix,
    k = effectiveAllK,
    algorithm = knnAlgorithm
  )

  neighborIndex <- allNeighborResult$nn.index
  if(effectiveAllK > 1L){
    neighborIndex <- neighborIndex[, -1L, drop = FALSE]
  }
  majorityMask <- targetFactor[as.vector(neighborIndex)] == classRoles$majorityLabel
  majorityRatio <- rowMeans(matrix(majorityMask, nrow = nrow(neighborIndex), ncol = ncol(neighborIndex)))
  if(sum(majorityRatio) <= 0){
    generationWeights <- rep.int(1 / length(minorityIndex), length(minorityIndex))
  } else {
    generationWeights <- majorityRatio / sum(majorityRatio)
  }

  rawCounts <- syntheticCount * generationWeights
  syntheticPerRow <- floor(rawCounts)
  remaining <- syntheticCount - sum(syntheticPerRow)
  if(remaining > 0L){
    fractionalOrder <- order(rawCounts - syntheticPerRow, decreasing = TRUE)
    syntheticPerRow[fractionalOrder[seq_len(remaining)]] <- syntheticPerRow[fractionalOrder[seq_len(remaining)]] + 1L
  }

  effectiveMinorityK <- min(as.integer(kOver) + 1L, nrow(minorityMatrix))
  minorityNeighborResult <- FNN::get.knnx(
    data = minorityMatrix,
    query = minorityMatrix,
    k = effectiveMinorityK,
    algorithm = knnAlgorithm
  )
  minorityNeighborIndex <- minorityNeighborResult$nn.index
  if(effectiveMinorityK > 1L){
    minorityNeighborIndex <- minorityNeighborIndex[, -1L, drop = FALSE]
  }

  syntheticMatrix <- matrix(0, nrow = syntheticCount, ncol = NCOL(xScaled))
  writeStart <- 1L
  for(i in which(syntheticPerRow > 0L)){
    rowCount <- syntheticPerRow[[i]]
    writeEnd <- writeStart + rowCount - 1L
    baseRows <- matrix(minorityMatrix[i, ], nrow = rowCount, ncol = NCOL(xScaled), byrow = TRUE)
    selectedNeighborRows <- minorityNeighborIndex[i, sample.int(ncol(minorityNeighborIndex), rowCount, replace = TRUE)]
    neighborRows <- minorityMatrix[selectedNeighborRows, , drop = FALSE]
    syntheticMatrix[writeStart:writeEnd, ] <- baseRows + stats::runif(rowCount) * (neighborRows - baseRows)
    writeStart <- writeEnd + 1L
  }
  colnames(syntheticMatrix) <- colnames(xScaled)

  list(
    x = rbind(xScaled, syntheticMatrix),
    y = factor(c(as.character(targetFactor), rep.int(classRoles$minorityLabel, syntheticCount)), levels = levels(targetFactor))
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
#' @param knnAlgorithm algoritmo para FNN::get.knnx
#' @export
ApplyAdasynOversampling <- function(
  predictorData,
  targetVector,
  overRatio = 0.2,
  kOver = 5L,
  seed = 42L,
  returnScaled = FALSE,
  restoreTypes = TRUE,
  output = c("data.frame", "matrix"),
  knnAlgorithm = c("cover_tree", "kd_tree", "brute")
){
  output <- match.arg(output)
  knnAlgorithm <- match.arg(knnAlgorithm)
  ValidateSamplingInputs(predictorData, targetVector, seed)

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
  adasynResult <- GenerateAdasynSamples(
    xScaled = xScaled,
    targetFactor = targetFactor,
    syntheticCount = syntheticCount,
    kOver = kOver,
    knnAlgorithm = knnAlgorithm
  )
  colnames(adasynResult$x) <- colnames(xMatrix)

  xRestored <- RevertZScoreScalingMatrix(adasynResult$x, scalingInfo)

  finalPredictors <- if(restoreTypes) {
    RestoreNumericColumnTypes(xRestored, typeInfo, asDataFrame = identical(output, "data.frame"))
  } else if(identical(output, "data.frame")) {
    as.data.frame(xRestored, stringsAsFactors = FALSE)
  } else {
    xRestored
  }

  balancedData <- if(identical(output, "data.frame")) {
    out <- finalPredictors
    out$TARGET <- as.factor(adasynResult$y)
    out
  } else {
    list(x = OverUnderAsNumericMatrix(finalPredictors), y = as.factor(adasynResult$y))
  }

  diagnostics <- list(
    inputRows = NROW(xMatrix),
    outputRows = if(identical(output, "data.frame")) nrow(balancedData) else nrow(balancedData$x),
    generatedRows = (if(identical(output, "data.frame")) nrow(balancedData) else nrow(balancedData$x)) - nrow(xMatrix),
    inputClassDistribution = table(targetFactor),
    outputClassDistribution = table(as.factor(adasynResult$y))
  )

  result <- list(
    balancedData = balancedData,
    typeInfo = typeInfo,
    scalingInfo = scalingInfo,
    diagnostics = diagnostics
  )

  if(isTRUE(returnScaled)){
    result$balancedScaled <- list(
      x = adasynResult$x,
      y = as.factor(adasynResult$y)
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
#' @param typeInfo informacao opcional de tipos original
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
  typeInfo = NULL,
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
  typeInfo <- if(is.null(typeInfo)) InferNumericColumnTypes(predictorData) else typeInfo
  if(NCOL(xMatrix) != nrow(typeInfo)){
    stop("'typeInfo' deve ter uma linha por coluna de 'predictorData'", call. = FALSE)
  }

  scalingInfo <- if(isTRUE(inputAlreadyScaled)) {
    if(is.null(precomputedScaling)){
      stop("'precomputedScaling' e obrigatorio quando 'inputAlreadyScaled = TRUE'", call. = FALSE)
    }
    precomputedScaling
  } else if(is.null(precomputedScaling)) {
    ComputeZScoreParams(xMatrix)
  } else {
    ValidateScalingInfo(precomputedScaling, NCOL(xMatrix))
    precomputedScaling
  }

  xScaled <- if(isTRUE(inputAlreadyScaled)) xMatrix else ApplyZScoreScalingMatrix(xMatrix, scalingInfo)

  classCounts <- table(targetFactor)
  if(classCounts[[1L]] == classCounts[[2L]]){
    retainedIndex <- seq_len(nrow(xScaled))
    reducedScaled <- xScaled
    reducedTarget <- targetFactor
  } else {
    classRoles <- GetBinaryClassRoles(targetFactor)
    minorityIndex <- which(targetFactor == classRoles$minorityLabel)
    majorityIndex <- which(targetFactor == classRoles$majorityLabel)

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
  }
  xRestored <- RevertZScoreScalingMatrix(reducedScaled, scalingInfo)

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
    removedRows = nrow(xMatrix) - (if(identical(output, "data.frame")) nrow(balancedData) else nrow(balancedData$x)),
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
    output = "matrix",
    knnAlgorithm = knnAlgorithm
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
    typeInfo = overResult$typeInfo,
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
