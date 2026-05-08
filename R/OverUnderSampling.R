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

#' Emitir erro padronizado com cli
#' @noRd
OverUnderAbort <- function(message){
  if(requireNamespace("cli", quietly = TRUE)){
    cli::cli_abort(message, call = NULL)
  }
  stop(message, call. = FALSE)
}

#' Emitir aviso padronizado com cli
#' @noRd
OverUnderWarn <- function(message){
  if(requireNamespace("cli", quietly = TRUE)){
    return(cli::cli_warn(message, call = NULL))
  }
  warning(message, call. = FALSE)
}

#' Emitir mensagem informativa padronizada com cli
#' @noRd
OverUnderInform <- function(message){
  if(requireNamespace("cli", quietly = TRUE)){
    return(cli::cli_inform(message))
  }
  message(message)
}

#' Carregar dependencias do fluxo de sampling
#' @noRd
OverUnderLoadPackages <- function(){
  if(isTRUE(.OverUnderState$packagesLoaded)){
    return(invisible(TRUE))
  }

  packageNames <- c("cli", "Rfast")
  for(packageName in packageNames){
    if(!requireNamespace(packageName, quietly = TRUE)){
      OverUnderAbort(paste0("Pacote necessario nao encontrado: ", packageName))
    }
  }

  .OverUnderState$packagesLoaded <- TRUE
  invisible(TRUE)
}

#' Verificar se a biblioteca nativa do pacote esta carregada
#' @noRd
OverUnderNativeAvailable <- function(){
  is.loaded("OU_GenerateSyntheticAdasynC", PACKAGE = "instanceengineering")
}

#' Verificar interrupcao solicitada pelo usuario
#' @noRd
OverUnderCheckUserInterrupt <- function(){
  if(OverUnderNativeAvailable()){
    .Call("OU_CheckUserInterruptC", PACKAGE = "instanceengineering")
  } else {
    Sys.sleep(0)
  }

  invisible(TRUE)
}

#' Resolver algoritmo KNN automatico quando solicitado
#' @noRd
ResolveKnnAlgorithm <- function(knnAlgorithm, predictorColumnCount){
  if(!identical(knnAlgorithm, "auto")){
    return(knnAlgorithm)
  }

  if(predictorColumnCount > 15L){
    "brute"
  } else {
    "kd_tree"
  }
}

#' Resolver backend KNN automatico quando solicitado
#' @noRd
ResolveKnnBackend <- function(knnBackend, knnWorkers){
  if(!identical(knnBackend, "auto")){
    return(knnBackend)
  }

  if(knnWorkers > 1L){
    if(requireNamespace("BiocNeighbors", quietly = TRUE) && requireNamespace("BiocParallel", quietly = TRUE)){
      return("BiocNeighbors")
    }

    OverUnderWarn("'knnWorkers > 1' foi solicitado, mas BiocNeighbors/BiocParallel nao estao instalados; usando FNN sem paralelismo.")
  }

  "FNN"
}

#' Validar numero de workers KNN
#' @noRd
ValidateKnnWorkers <- function(knnWorkers){
  if(!is.numeric(knnWorkers) || length(knnWorkers) != 1L || is.na(knnWorkers) || !is.finite(knnWorkers) || knnWorkers < 1L){
    OverUnderAbort("'knnWorkers' deve ser inteiro positivo")
  }

  as.integer(knnWorkers)
}

#' Validar parametros do backend RcppHNSW
#' @noRd
ValidateHnswParams <- function(hnswM, hnswEf){
  if(!is.numeric(hnswM) || length(hnswM) != 1L || is.na(hnswM) || !is.finite(hnswM) || hnswM < 2L){
    OverUnderAbort("'hnswM' deve ser inteiro >= 2")
  }
  if(!is.numeric(hnswEf) || length(hnswEf) != 1L || is.na(hnswEf) || !is.finite(hnswEf) || hnswEf < 1L){
    OverUnderAbort("'hnswEf' deve ser inteiro positivo")
  }

  list(hnswM = as.integer(hnswM), hnswEf = as.integer(hnswEf))
}

#' Criar parametro BiocParallel seguro para consultas KNN
#' @noRd
CreateKnnBiocParallelParam <- function(knnWorkers){
  if(knnWorkers <= 1L){
    return(BiocParallel::SerialParam())
  }

  if(.Platform$OS.type == "windows"){
    BiocParallel::SnowParam(workers = knnWorkers, type = "SOCK")
  } else {
    BiocParallel::MulticoreParam(workers = knnWorkers)
  }
}

#' Criar parametro BiocNeighbors para busca exata por padrao
#' @noRd
CreateBiocNeighborParam <- function(biocNeighborAlgorithm, predictorColumnCount){
  if(identical(biocNeighborAlgorithm, "auto")){
    biocNeighborAlgorithm <- if(predictorColumnCount > 15L) "Exhaustive" else "Kmknn"
  }

  switch(
    biocNeighborAlgorithm,
    Kmknn = BiocNeighbors::KmknnParam(),
    Vptree = BiocNeighbors::VptreeParam(),
    Exhaustive = BiocNeighbors::ExhaustiveParam(),
    Annoy = BiocNeighbors::AnnoyParam(),
    Hnsw = BiocNeighbors::HnswParam(),
    OverUnderAbort("'biocNeighborAlgorithm' invalido")
  )
}

#' Executar consulta KNN usando FNN ou BiocNeighbors/BiocParallel
#' @noRd
GetKnnx <- function(
  data,
  query,
  k,
  knnAlgorithm,
  knnBackend,
  knnWorkers,
  biocNeighborAlgorithm,
  hnswM,
  hnswEf
){
  OverUnderCheckUserInterrupt()

  if(identical(knnBackend, "FNN")){
    if(!requireNamespace("FNN", quietly = TRUE)){
      OverUnderAbort("'knnBackend = FNN' requer o pacote FNN")
    }

    knnResult <- FNN::get.knnx(
      data = data,
      query = query,
      k = k,
      algorithm = knnAlgorithm
    )
    OverUnderCheckUserInterrupt()

    return(knnResult)
  }

  if(identical(knnBackend, "RcppHNSW")){
    if(!requireNamespace("RcppHNSW", quietly = TRUE)){
      OverUnderAbort("'knnBackend = RcppHNSW' requer o pacote RcppHNSW. Instale-o com install.packages('RcppHNSW').")
    }

    effectiveEf <- min(max(as.integer(hnswEf), as.integer(k)), nrow(data))
    hnswIndex <- RcppHNSW::hnsw_build(
      X = data,
      distance = "euclidean",
      M = as.integer(hnswM),
      ef = effectiveEf,
      verbose = FALSE,
      progress = "bar",
      n_threads = knnWorkers,
      byrow = TRUE
    )
    knnResult <- RcppHNSW::hnsw_search(
      X = query,
      ann = hnswIndex,
      k = k,
      ef = effectiveEf,
      verbose = FALSE,
      progress = "bar",
      n_threads = knnWorkers,
      byrow = TRUE
    )

    OverUnderCheckUserInterrupt()

    return(list(nn.index = knnResult$idx, nn.dist = knnResult$dist))
  }

  if(!requireNamespace("BiocNeighbors", quietly = TRUE) || !requireNamespace("BiocParallel", quietly = TRUE)){
    OverUnderAbort("'knnBackend = BiocNeighbors' requer os pacotes BiocNeighbors e BiocParallel. Instale-os com BiocManager::install(c('BiocNeighbors', 'BiocParallel')).")
  }

  neighborParam <- CreateBiocNeighborParam(biocNeighborAlgorithm, NCOL(data))
  parallelParam <- CreateKnnBiocParallelParam(knnWorkers)
  knnResult <- BiocNeighbors::queryKNN(
    X = data,
    query = query,
    k = k,
    BNPARAM = neighborParam,
    BPPARAM = parallelParam
  )

  OverUnderCheckUserInterrupt()

  list(nn.index = knnResult$index, nn.dist = knnResult$distance)
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
    OverUnderAbort("'predictorData' deve ser data.frame ou matrix")
  }

  if(NROW(predictorData) == 0L){
    OverUnderAbort("'predictorData' deve conter ao menos uma linha")
  }

  if(length(targetVector) != NROW(predictorData)){
    OverUnderAbort("'targetVector' deve ter o mesmo numero de linhas de 'predictorData'")
  }

  if(anyNA(predictorData)){
    OverUnderAbort("'predictorData' nao pode conter NA")
  }

  if(anyNA(targetVector)){
    OverUnderAbort("'targetVector' nao pode conter NA")
  }

  xCheck <- if(is.matrix(predictorData)) predictorData else predictorData
  isNumericColumn <- if(is.matrix(xCheck)) {
    is.numeric(xCheck)
  } else {
    vapply(xCheck, is.numeric, logical(1L))
  }

  if(!all(isNumericColumn)){
    OverUnderAbort("Todos os preditores devem ser numericos")
  }

  targetFactor <- as.factor(targetVector)
  if(nlevels(targetFactor) != 2L){
    OverUnderAbort("'targetVector' deve ser binario")
  }

  classCounts <- table(targetFactor)
  if(any(classCounts < 2L)){
    OverUnderAbort("Cada classe deve ter ao menos 2 observacoes")
  }

  if(!is.numeric(seed) || length(seed) != 1L || is.na(seed) || !is.finite(seed)){
    OverUnderAbort("'seed' deve ser escalar numerico")
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
    OverUnderAbort("'targetVector' deve ser binario")
  }
  if(classCounts[[1L]] == classCounts[[2L]]){
    OverUnderAbort("As rotinas de sampling requerem classes desbalanceadas")
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
    OverUnderAbort("'precomputedScaling' deve conter 'centers' e 'scales'")
  }
  if(length(scalingInfo$centers) != predictorColumnCount || length(scalingInfo$scales) != predictorColumnCount){
    OverUnderAbort("'precomputedScaling' deve ter um centro e uma escala por coluna")
  }
  if(anyNA(scalingInfo$centers) || anyNA(scalingInfo$scales) || any(!is.finite(scalingInfo$centers)) || any(!is.finite(scalingInfo$scales))){
    OverUnderAbort("'precomputedScaling' contem valores ausentes ou infinitos")
  }
  if(any(scalingInfo$scales <= 0)){
    OverUnderAbort("'precomputedScaling$scales' deve conter apenas valores positivos")
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

  if(OverUnderNativeAvailable()){
    params <- .Call("OU_ComputeZScoreParamsC", xMatrix, PACKAGE = "instanceengineering")
  } else {
    params <- list(
      centers = Rfast::colmeans(xMatrix),
      scales = Rfast::colVars(xMatrix, std = TRUE)
    )
  }

  invalid <- is.na(params$scales) | !is.finite(params$scales) | params$scales <= 0
  if(any(invalid)){
    columnNames <- colnames(xMatrix)
    if(is.null(columnNames)){
      columnNames <- paste0("V", seq_len(NCOL(xMatrix)))
    }
    OverUnderAbort(paste0(
      "Colunas com desvio padrao zero ou indefinido: ",
      paste(columnNames[invalid], collapse = ", ")
    ))
  }

  list(centers = as.numeric(params$centers), scales = as.numeric(params$scales))
}

#' Aplicar z-score em matrix double
#' @noRd
ApplyZScoreScalingMatrix <- function(xMatrix, scalingInfo){
  xMatrix <- OverUnderAsNumericMatrix(xMatrix)
  ValidateScalingInfo(scalingInfo, NCOL(xMatrix))

  if(OverUnderNativeAvailable()){
    scaled <- .Call("OU_ApplyZScoreC", xMatrix, as.numeric(scalingInfo$centers), as.numeric(scalingInfo$scales), FALSE, PACKAGE = "instanceengineering")
  } else {
    centered <- Rfast::eachrow(xMatrix, scalingInfo$centers, oper = "-")
    scaled <- Rfast::eachrow(centered, scalingInfo$scales, oper = "/")
  }

  storage.mode(scaled) <- "double"
  colnames(scaled) <- colnames(xMatrix)
  scaled
}

#' Reverter z-score em matrix double
#' @noRd
RevertZScoreScalingMatrix <- function(xMatrix, scalingInfo){
  xMatrix <- OverUnderAsNumericMatrix(xMatrix)
  ValidateScalingInfo(scalingInfo, NCOL(xMatrix))

  if(OverUnderNativeAvailable()){
    restored <- .Call("OU_ApplyZScoreC", xMatrix, as.numeric(scalingInfo$centers), as.numeric(scalingInfo$scales), TRUE, PACKAGE = "instanceengineering")
  } else {
    unscaled <- Rfast::eachrow(xMatrix, scalingInfo$scales, oper = "*")
    restored <- Rfast::eachrow(unscaled, scalingInfo$centers, oper = "+")
  }

  storage.mode(restored) <- "double"
  colnames(restored) <- colnames(xMatrix)
  restored
}

#' Restaurar tipos numericos inferidos
#' @noRd
RestoreNumericColumnTypes <- function(xMatrix, typeInfo, asDataFrame = TRUE){
  xMatrix <- OverUnderAsNumericMatrix(xMatrix)
  if(NCOL(xMatrix) != nrow(typeInfo)){
    OverUnderAbort("Inconsistencia entre numero de colunas e typeInfo")
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
    OverUnderAbort("'overRatio' deve ser escalar numerico nao negativo")
  }

  classRoles <- GetBinaryClassRoles(targetFactor)
  minorityCount <- as.integer(classRoles$classCounts[classRoles$minorityLabel])
  syntheticCount <- floor(minorityCount * overRatio)

  if(syntheticCount < 1L){
    OverUnderAbort("'overRatio' gerou zero linhas sinteticas")
  }

  syntheticCount
}

#' Calcular quantidade retida da maioria
#' @noRd
ComputeMajorityRetentionCount <- function(targetFactor, underRatio){
  if(!is.numeric(underRatio) || length(underRatio) != 1L || is.na(underRatio) || underRatio <= 0 || underRatio > 1){
    OverUnderAbort("'underRatio' deve estar no intervalo (0, 1]")
  }

  classRoles <- GetBinaryClassRoles(targetFactor)
  majorityCount <- as.integer(classRoles$classCounts[classRoles$majorityLabel])
  retainedCount <- floor(majorityCount * underRatio)

  if(retainedCount < 1L){
    OverUnderAbort("'underRatio' reteve zero linhas")
  }

  retainedCount
}

#' Remover o proprio ponto de uma matriz de vizinhos de forma robusta
#' @noRd
DropSelfNeighborIndex <- function(neighborIndex, selfIndex, desiredK){
  out <- matrix(NA_integer_, nrow = nrow(neighborIndex), ncol = desiredK)
  for(i in seq_len(nrow(neighborIndex))){
    candidates <- neighborIndex[i, ]
    candidates <- candidates[!is.na(candidates) & candidates != selfIndex[[i]]]
    if(length(candidates) < desiredK){
      OverUnderAbort("Nao foi possivel remover o proprio ponto mantendo vizinhos suficientes")
    }
    out[i, ] <- candidates[seq_len(desiredK)]
  }
  out
}

#' Gerar amostras sinteticas ADASYN em matriz ja escalada
#' @noRd
GenerateAdasynSamples <- function(xScaled, targetFactor, syntheticCount, kOver, knnAlgorithm, knnBackend, knnWorkers, biocNeighborAlgorithm, hnswM, hnswEf){
  classRoles <- GetBinaryClassRoles(targetFactor)
  minorityIndex <- which(targetFactor == classRoles$minorityLabel)
  minorityMatrix <- xScaled[minorityIndex, , drop = FALSE]

  effectiveAllK <- min(as.integer(kOver) + 1L, nrow(xScaled))
  allNeighborResult <- GetKnnx(
    data = xScaled,
    query = minorityMatrix,
    k = effectiveAllK,
    knnAlgorithm = knnAlgorithm,
    knnBackend = knnBackend,
    knnWorkers = knnWorkers,
    biocNeighborAlgorithm = biocNeighborAlgorithm,
    hnswM = hnswM,
    hnswEf = hnswEf
  )
  OverUnderCheckUserInterrupt()

  neighborIndex <- allNeighborResult$nn.index
  desiredAllK <- min(as.integer(kOver), nrow(xScaled) - 1L)
  if(effectiveAllK > 1L){
    neighborIndex <- DropSelfNeighborIndex(neighborIndex, minorityIndex, desiredAllK)
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
  minorityNeighborResult <- GetKnnx(
    data = minorityMatrix,
    query = minorityMatrix,
    k = effectiveMinorityK,
    knnAlgorithm = knnAlgorithm,
    knnBackend = knnBackend,
    knnWorkers = knnWorkers,
    biocNeighborAlgorithm = biocNeighborAlgorithm,
    hnswM = hnswM,
    hnswEf = hnswEf
  )
  OverUnderCheckUserInterrupt()

  minorityNeighborIndex <- minorityNeighborResult$nn.index
  desiredMinorityK <- min(as.integer(kOver), nrow(minorityMatrix) - 1L)
  if(effectiveMinorityK > 1L){
    minorityNeighborIndex <- DropSelfNeighborIndex(minorityNeighborIndex, seq_len(nrow(minorityMatrix)), desiredMinorityK)
  }

  if(OverUnderNativeAvailable()){
    storage.mode(minorityMatrix) <- "double"
    storage.mode(minorityNeighborIndex) <- "integer"
    syntheticMatrix <- .Call(
      "OU_GenerateSyntheticAdasynC",
      minorityMatrix,
      minorityNeighborIndex,
      as.integer(syntheticPerRow),
      PACKAGE = "instanceengineering"
    )
    OverUnderCheckUserInterrupt()
  } else {
    syntheticMatrix <- matrix(0, nrow = syntheticCount, ncol = NCOL(xScaled))
    writeStart <- 1L
    positiveRows <- which(syntheticPerRow > 0L)
    for(i in positiveRows){
      OverUnderCheckUserInterrupt()
      rowCount <- syntheticPerRow[[i]]
      writeEnd <- writeStart + rowCount - 1L
      baseRows <- matrix(minorityMatrix[i, ], nrow = rowCount, ncol = NCOL(xScaled), byrow = TRUE)
      selectedNeighborRows <- minorityNeighborIndex[i, sample.int(ncol(minorityNeighborIndex), rowCount, replace = TRUE)]
      neighborRows <- minorityMatrix[selectedNeighborRows, , drop = FALSE]
      syntheticMatrix[writeStart:writeEnd, ] <- baseRows + stats::runif(rowCount) * (neighborRows - baseRows)
      writeStart <- writeEnd + 1L
    }
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
#' @param knnAlgorithm algoritmo para FNN::get.knnx quando knnBackend = "FNN"
#' @param knnBackend backend KNN: "FNN", "BiocNeighbors", "RcppHNSW" ou "auto"
#' @param knnWorkers numero de workers para BiocNeighbors/BiocParallel ou RcppHNSW
#' @param biocNeighborAlgorithm algoritmo BiocNeighbors
#' @param hnswM conectividade do indice RcppHNSW
#' @param hnswEf tamanho da lista dinamica de construcao/busca RcppHNSW
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
  knnAlgorithm = c("auto", "cover_tree", "kd_tree", "brute"),
  knnBackend = c("auto", "FNN", "BiocNeighbors", "RcppHNSW"),
  knnWorkers = 1L,
  biocNeighborAlgorithm = c("auto", "Kmknn", "Vptree", "Exhaustive", "Annoy", "Hnsw"),
  hnswM = 16L,
  hnswEf = 200L
){
  OverUnderCheckUserInterrupt()

  output <- match.arg(output)
  knnAlgorithm <- match.arg(knnAlgorithm)
  knnBackend <- match.arg(knnBackend)
  biocNeighborAlgorithm <- match.arg(biocNeighborAlgorithm)
  knnWorkers <- ValidateKnnWorkers(knnWorkers)
  hnswParams <- ValidateHnswParams(hnswM, hnswEf)
  hnswM <- hnswParams$hnswM
  hnswEf <- hnswParams$hnswEf
  ValidateSamplingInputs(predictorData, targetVector, seed)

  if(!is.numeric(kOver) || length(kOver) != 1L || is.na(kOver) || kOver < 1L){
    OverUnderAbort("'kOver' deve ser inteiro positivo")
  }

  xMatrix <- OverUnderAsNumericMatrix(predictorData)
  colnames(xMatrix) <- OverUnderGetColumnNames(predictorData)
  knnAlgorithm <- ResolveKnnAlgorithm(knnAlgorithm, NCOL(xMatrix))
  knnBackend <- ResolveKnnBackend(knnBackend, knnWorkers)
  targetFactor <- as.factor(targetVector)
  typeInfo <- InferNumericColumnTypes(predictorData)
  scalingInfo <- ComputeZScoreParams(xMatrix)
  xScaled <- ApplyZScoreScalingMatrix(xMatrix, scalingInfo)
  OverUnderCheckUserInterrupt()

  syntheticCount <- ComputeMinorityExpansionCount(targetFactor, overRatio)

  set.seed(seed)
  adasynResult <- GenerateAdasynSamples(
    xScaled = xScaled,
    targetFactor = targetFactor,
    syntheticCount = syntheticCount,
    kOver = kOver,
    knnAlgorithm = knnAlgorithm,
    knnBackend = knnBackend,
    knnWorkers = knnWorkers,
    biocNeighborAlgorithm = biocNeighborAlgorithm,
    hnswM = hnswM,
    hnswEf = hnswEf
  )
  OverUnderCheckUserInterrupt()

  colnames(adasynResult$x) <- colnames(xMatrix)

  xRestored <- RevertZScoreScalingMatrix(adasynResult$x, scalingInfo)
  OverUnderCheckUserInterrupt()

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
    knnBackend = knnBackend,
    knnWorkers = knnWorkers,
    biocNeighborAlgorithm = if(identical(knnBackend, "BiocNeighbors")) biocNeighborAlgorithm else NA_character_,
    hnswM = if(identical(knnBackend, "RcppHNSW")) hnswM else NA_integer_,
    hnswEf = if(identical(knnBackend, "RcppHNSW")) hnswEf else NA_integer_,
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
#' @param knnAlgorithm algoritmo para FNN::get.knnx quando knnBackend = "FNN"
#' @param knnBackend backend KNN: "FNN", "BiocNeighbors", "RcppHNSW" ou "auto"
#' @param knnWorkers numero de workers para BiocNeighbors/BiocParallel ou RcppHNSW
#' @param biocNeighborAlgorithm algoritmo BiocNeighbors
#' @param hnswM conectividade do indice RcppHNSW
#' @param hnswEf tamanho da lista dinamica de construcao/busca RcppHNSW
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
  knnAlgorithm = c("auto", "cover_tree", "kd_tree", "brute"),
  knnBackend = c("auto", "FNN", "BiocNeighbors", "RcppHNSW"),
  knnWorkers = 1L,
  biocNeighborAlgorithm = c("auto", "Kmknn", "Vptree", "Exhaustive", "Annoy", "Hnsw"),
  hnswM = 16L,
  hnswEf = 200L
){
  OverUnderCheckUserInterrupt()

  output <- match.arg(output)
  knnAlgorithm <- match.arg(knnAlgorithm)
  knnBackend <- match.arg(knnBackend)
  biocNeighborAlgorithm <- match.arg(biocNeighborAlgorithm)
  knnWorkers <- ValidateKnnWorkers(knnWorkers)
  hnswParams <- ValidateHnswParams(hnswM, hnswEf)
  hnswM <- hnswParams$hnswM
  hnswEf <- hnswParams$hnswEf
  ValidateSamplingInputs(predictorData, targetVector, seed)

  if(!is.numeric(kUnder) || length(kUnder) != 1L || is.na(kUnder) || kUnder < 1L){
    OverUnderAbort("'kUnder' deve ser inteiro positivo")
  }

  xMatrix <- OverUnderAsNumericMatrix(predictorData)
  colnames(xMatrix) <- OverUnderGetColumnNames(predictorData)
  knnAlgorithm <- ResolveKnnAlgorithm(knnAlgorithm, NCOL(xMatrix))
  knnBackend <- ResolveKnnBackend(knnBackend, knnWorkers)
  targetFactor <- as.factor(targetVector)
  typeInfo <- if(is.null(typeInfo)) InferNumericColumnTypes(predictorData) else typeInfo
  if(NCOL(xMatrix) != nrow(typeInfo)){
    OverUnderAbort("'typeInfo' deve ter uma linha por coluna de 'predictorData'")
  }

  scalingInfo <- if(isTRUE(inputAlreadyScaled)) {
    if(is.null(precomputedScaling)){
      OverUnderAbort("'precomputedScaling' e obrigatorio quando 'inputAlreadyScaled = TRUE'")
    }
    precomputedScaling
  } else if(is.null(precomputedScaling)) {
    ComputeZScoreParams(xMatrix)
  } else {
    ValidateScalingInfo(precomputedScaling, NCOL(xMatrix))
    precomputedScaling
  }

  xScaled <- if(isTRUE(inputAlreadyScaled)) xMatrix else ApplyZScoreScalingMatrix(xMatrix, scalingInfo)
  OverUnderCheckUserInterrupt()

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
      OverUnderAbort("Sem linhas minoritarias suficientes para NearMiss")
    }

    set.seed(seed)
    knnResult <- GetKnnx(
      data = minorityMatrix,
      query = majorityMatrix,
      k = effectiveK,
      knnAlgorithm = knnAlgorithm,
      knnBackend = knnBackend,
      knnWorkers = knnWorkers,
      biocNeighborAlgorithm = biocNeighborAlgorithm,
      hnswM = hnswM,
      hnswEf = hnswEf
    )
    OverUnderCheckUserInterrupt()

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
    knnBackend = knnBackend,
    knnWorkers = knnWorkers,
    biocNeighborAlgorithm = if(identical(knnBackend, "BiocNeighbors")) biocNeighborAlgorithm else NA_character_,
    hnswM = if(identical(knnBackend, "RcppHNSW")) hnswM else NA_integer_,
    hnswEf = if(identical(knnBackend, "RcppHNSW")) hnswEf else NA_integer_,
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
#' @param knnAlgorithm algoritmo para FNN::get.knnx quando knnBackend = "FNN"
#' @param knnBackend backend KNN: "FNN", "BiocNeighbors", "RcppHNSW" ou "auto"
#' @param knnWorkers numero de workers para BiocNeighbors/BiocParallel ou RcppHNSW
#' @param biocNeighborAlgorithm algoritmo BiocNeighbors
#' @param hnswM conectividade do indice RcppHNSW
#' @param hnswEf tamanho da lista dinamica de construcao/busca RcppHNSW
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
  knnAlgorithm = c("auto", "cover_tree", "kd_tree", "brute"),
  knnBackend = c("auto", "FNN", "BiocNeighbors", "RcppHNSW"),
  knnWorkers = 1L,
  biocNeighborAlgorithm = c("auto", "Kmknn", "Vptree", "Exhaustive", "Annoy", "Hnsw"),
  hnswM = 16L,
  hnswEf = 200L
){
  OverUnderCheckUserInterrupt()

  output <- match.arg(output)
  knnAlgorithm <- match.arg(knnAlgorithm)
  knnBackend <- match.arg(knnBackend)
  biocNeighborAlgorithm <- match.arg(biocNeighborAlgorithm)
  knnWorkers <- ValidateKnnWorkers(knnWorkers)
  hnswParams <- ValidateHnswParams(hnswM, hnswEf)
  hnswM <- hnswParams$hnswM
  hnswEf <- hnswParams$hnswEf
  knnAlgorithm <- ResolveKnnAlgorithm(knnAlgorithm, NCOL(predictorData))
  knnBackend <- ResolveKnnBackend(knnBackend, knnWorkers)

  overResult <- ApplyAdasynOversampling(
    predictorData = predictorData,
    targetVector = targetVector,
    overRatio = overRatio,
    kOver = kOver,
    seed = seed,
    returnScaled = TRUE,
    restoreTypes = FALSE,
    output = "matrix",
    knnAlgorithm = knnAlgorithm,
    knnBackend = knnBackend,
    knnWorkers = knnWorkers,
    biocNeighborAlgorithm = biocNeighborAlgorithm,
    hnswM = hnswM,
    hnswEf = hnswEf
  )
  OverUnderCheckUserInterrupt()

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
    knnAlgorithm = knnAlgorithm,
    knnBackend = knnBackend,
    knnWorkers = knnWorkers,
    biocNeighborAlgorithm = biocNeighborAlgorithm,
    hnswM = hnswM,
    hnswEf = hnswEf
  )
  OverUnderCheckUserInterrupt()

  diagnostics <- list(
    originalRows = NROW(predictorData),
    afterOversamplingRows = nrow(overResult$balancedScaled$x),
    finalRows = if(identical(output, "data.frame")) nrow(underResult$balancedData) else nrow(underResult$balancedData$x),
    knnBackend = knnBackend,
    knnWorkers = knnWorkers,
    biocNeighborAlgorithm = if(identical(knnBackend, "BiocNeighbors")) biocNeighborAlgorithm else NA_character_,
    hnswM = if(identical(knnBackend, "RcppHNSW")) hnswM else NA_integer_,
    hnswEf = if(identical(knnBackend, "RcppHNSW")) hnswEf else NA_integer_,
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
