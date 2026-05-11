#' Executar consulta KNN usando FNN ou BiocNeighbors/BiocParallel
#'
#' @details
#' A funcao implementa uma unidade interna do fluxo de balanceamento com contrato de entrada explicito e retorno controlado
#' A documentacao descreve a intencao operacional para apoiar manutencao, auditoria e revisao tecnica do pacote
#'
#' @param sby_data Matriz de referencia para busca KNN
#' @param sby_query Matriz de consulta para busca KNN
#' @param sby_k Numero de vizinhos solicitados
#' @param sby_knn_algorithm Algoritmo KNN configurado
#' @param sby_knn_engine Engine KNN configurado
#' @param sby_knn_workers Numero de workers KNN configurado
#' @param sby_knn_hnsw_m Conectividade HNSW configurada
#' @param sby_knn_hnsw_ef Lista dinamica HNSW configurada
#' @param sby_knn_query_chunk_size Tamanho de bloco para consultas KNN
#'
#' @return Lista com matrizes `nn.index` e `nn.dist`
#' @noRd
sby_get_knnx <- function(
  sby_data,
  sby_query,
  sby_k,
  sby_knn_algorithm,
  sby_knn_engine,
  sby_knn_distance_metric,
  sby_knn_workers,
  sby_knn_hnsw_m,
  sby_knn_hnsw_ef,
  sby_knn_query_chunk_size = getOption("instenginer.sby_knn_query_chunk_size", 1000L)
){
  
  # Verifica se ha solicitacao de interrupcao antes da consulta KNN
  sby_adanear_check_user_interrupt()

  # Valida tamanho de bloco para consultas KNN interrompiveis
  sby_knn_query_chunk_size <- sby_validate_knn_query_chunk_size(
    sby_knn_query_chunk_size = sby_knn_query_chunk_size
  )

  # Aplica normalizacao L2 obrigatoria para metricas angulares ou produto interno
  if(!identical(
    x = sby_knn_distance_metric,
    y = "euclidean"
  )){

    # Normaliza referencia e consulta depois do z-score e antes do engine KNN
    sby_data  <- SbyNormalizeL2(
      sby_x_matrix = sby_data
    )
    sby_query <- SbyNormalizeL2(
      sby_x_matrix = sby_query
    )
  }

  # Executa consulta pelo engine FNN quando selecionado
  if(identical(
    x = sby_knn_engine,
    y = "FNN"
  )){

    # Bloqueia metricas nao euclidianas porque FNN implementa apenas distancia euclidiana neste pacote
    if(!identical(
      x = sby_knn_distance_metric,
      y = "euclidean"
    )){

      # Aborta combinacao incompatvel para evitar regressao silenciosa de metrica
      sby_adanear_abort(
        sby_message = "'sby_knn_engine = FNN' suporta apenas 'sby_knn_distance_metric = euclidean'"
      )
    }

    # Verifica disponibilidade do pacote FNN
    if(!requireNamespace(
      package = "FNN",
      quietly = TRUE
    )){

      # Aborta quando o engine FNN nao esta instalado
      sby_adanear_abort(
        sby_message = "'sby_knn_engine = FNN' requer o pacote FNN"
      )
    }

    # Bloqueia algoritmos exclusivos dos outros engines no FNN
    if(!(sby_knn_algorithm %in% c("auto", "kd_tree", "cover_tree", "brute"))){

      # Aborta algoritmo incompatvel com FNN
      sby_adanear_abort(
        sby_message = "'sby_knn_algorithm' deve ser um de 'auto', 'kd_tree', 'cover_tree' ou 'brute' quando 'sby_knn_engine = FNN'"
      )
    }

    # Consulta vizinhos FNN em blocos interrompiveis
    sby_knn_result <- sby_query_knn_in_chunks(
      sby_query = sby_query,
      sby_k = sby_k,
      sby_knn_query_chunk_size = sby_knn_query_chunk_size,
      sby_query_fun = function(sby_query_chunk){
        # Executa consulta FNN para o bloco corrente
        return(FNN::get.knnx(
          data = sby_data,
          query = sby_query_chunk,
          k = sby_k,
          algorithm = sby_knn_algorithm
        ))
      }
    )

    # Verifica se ha solicitacao de interrupcao apos consulta FNN
    sby_adanear_check_user_interrupt()

    # Retorna resultado KNN produzido pelo engine FNN
    return(sby_knn_result)
  }

  # Executa consulta pelo engine RcppHNSW quando selecionado
  if(identical(
    x = sby_knn_engine,
    y = "RcppHNSW"
  )){

    # Verifica disponibilidade do pacote RcppHNSW
    if(!requireNamespace(
      package = "RcppHNSW",
      quietly = TRUE
    )){

      # Aborta quando o engine RcppHNSW nao esta instalado
      sby_adanear_abort(
        sby_message = "'sby_knn_engine = RcppHNSW' requer o pacote RcppHNSW. Instale-o com install.packages('RcppHNSW')."
      )
    }

    # Define parametro efetivo de busca HNSW limitado pelo tamanho dos dados
    sby_effective_ef <- min(
      max(
        as.integer(sby_knn_hnsw_ef),
        as.integer(sby_k)
      ),
      nrow(sby_data)
    )

    # Constroi indice HNSW para a matriz de referencia
    sby_hnsw_index <- RcppHNSW::hnsw_build(
      X = sby_data,
      distance = sby_knn_distance_metric,
      M = as.integer(sby_knn_hnsw_m),
      ef = sby_effective_ef,
      verbose = FALSE,
      progress = "bar",
      n_threads = sby_knn_workers,
      byrow = TRUE
    )

    # Verifica se ha solicitacao de interrupcao apos construcao do indice HNSW
    sby_adanear_check_user_interrupt()

    # Valida tamanho de bloco especifico para consultas HNSW
    sby_hnsw_query_chunk_size <- sby_validate_knn_query_chunk_size(
      sby_knn_query_chunk_size = getOption(
        "instenginer.sby_hnsw_query_chunk_size",
        100L
      )
    )

    # Consulta vizinhos HNSW em blocos interrompiveis
    sby_knn_result <- sby_query_knn_in_chunks(
      sby_query = sby_query,
      sby_k = sby_k,
      sby_knn_query_chunk_size = sby_hnsw_query_chunk_size,
      sby_query_fun = function(sby_query_chunk){
        # Executa busca HNSW para o bloco corrente
        sby_hnsw_result <- RcppHNSW::hnsw_search(
          X = sby_query_chunk,
          ann = sby_hnsw_index,
          k = sby_k,
          ef = sby_effective_ef,
          verbose = FALSE,
          progress = "bar",
          n_threads = sby_knn_workers,
          byrow = TRUE
        )

        # Retorna indices e distancias no contrato comum de KNN
        return(list(
          nn.index = sby_hnsw_result$idx,
          nn.dist  = sby_hnsw_result$dist
        ))
      }
    )

    # Verifica se ha solicitacao de interrupcao apos consulta HNSW
    sby_adanear_check_user_interrupt()

    # Retorna resultado KNN produzido pelo engine HNSW
    return(sby_knn_result)
  }

  # Verifica disponibilidade dos pacotes BiocNeighbors e BiocParallel
  if(!requireNamespace(
    package = "BiocNeighbors",
    quietly = TRUE
  ) || !requireNamespace(
    package = "BiocParallel",
    quietly = TRUE
  )){

    # Aborta quando dependencias do engine BiocNeighbors estao ausentes
    sby_adanear_abort(
      sby_message = "'sby_knn_engine = BiocNeighbors' requer os pacotes BiocNeighbors e BiocParallel. Instale-os com BiocManager::install(c('BiocNeighbors', 'BiocParallel'))."
    )
  }

  # Cria parametros de vizinhanca e paralelismo para BiocNeighbors
  sby_neighbor_param <- sby_create_bioc_neighbor_param(
    sby_knn_algorithm          = sby_knn_algorithm,
    sby_predictor_column_count = NCOL(sby_data),
    sby_knn_distance_metric        = sby_knn_distance_metric
  )
  sby_parallel_param <- sby_create_knn_bioc_parallel_param(
    sby_knn_workers = sby_knn_workers
  )

  # Executa consulta KNN pelo engine BiocNeighbors
  sby_knn_result <- BiocNeighbors::queryKNN(
    X = sby_data,
    query = sby_query,
    k = sby_k,
    BNPARAM = sby_neighbor_param,
    BPPARAM = sby_parallel_param
  )

  # Verifica se ha solicitacao de interrupcao apos consulta BiocNeighbors
  sby_adanear_check_user_interrupt()

  # Retorna indices e distancias no contrato comum de KNN
  return(list(
    nn.index = sby_knn_result$index,
    nn.dist  = sby_knn_result$distance
  ))
}

#' Executar consultas KNN em blocos interrompiveis
#'
#' @details
#' A funcao implementa uma unidade interna do fluxo de balanceamento com contrato de entrada explicito e retorno controlado
#' A documentacao descreve a intencao operacional para apoiar manutencao, auditoria e revisao tecnica do pacote
#'
#' @param sby_query Matriz de consulta para busca KNN
#' @param sby_k Numero de vizinhos solicitados
#' @param sby_knn_query_chunk_size Tamanho de bloco para consultas KNN
#' @param sby_query_fun Funcao que executa consulta KNN em um bloco
#'
#' @return Lista com matrizes `nn.index` e `nn.dist`
#' @noRd
sby_query_knn_in_chunks <- function(sby_query, sby_k, sby_knn_query_chunk_size, sby_query_fun){
  # Calcula numero de linhas da matriz de consulta
  sby_query_rows <- nrow(sby_query)

  # Executa consulta diretamente quando os dados cabem em um unico bloco
  if(sby_query_rows <= sby_knn_query_chunk_size){

    # Retorna resultado da funcao de consulta sem particionamento
    return(sby_query_fun(
      sby_query
    ))
  }

  # Inicializa matrizes de saida e indices de inicio dos blocos
  sby_nn_index     <- matrix(
    data = NA_integer_,
    nrow = sby_query_rows,
    ncol = sby_k
  )
  sby_nn_dist      <- matrix(
    data = NA_real_,
    nrow = sby_query_rows,
    ncol = sby_k
  )
  sby_chunk_starts <- seq.int(
    from = 1L,
    to = sby_query_rows,
    by = sby_knn_query_chunk_size
  )

  # Processa consultas KNN em blocos sequenciais
  for(sby_chunk_start in sby_chunk_starts){
    # Verifica interrupcao antes de cada bloco de consulta
    sby_adanear_check_user_interrupt()

    # Define intervalo de linhas do bloco corrente
    sby_chunk_end   <- min(
      sby_chunk_start + sby_knn_query_chunk_size - 1L,
      sby_query_rows
    )
    sby_chunk_index <- seq.int(
      from = sby_chunk_start,
      to = sby_chunk_end
    )

    # Executa consulta KNN para o bloco corrente
    sby_chunk_result <- sby_query_fun(
      sby_query[sby_chunk_index, , drop = FALSE]
    )

    # Copia indices e distancias do bloco para as matrizes completas
    sby_nn_index[sby_chunk_index, ] <- sby_chunk_result$nn.index
    sby_nn_dist[sby_chunk_index, ]  <- sby_chunk_result$nn.dist

    # Verifica interrupcao apos cada bloco de consulta
    sby_adanear_check_user_interrupt()
  }

  # Retorna matrizes completas de indices e distancias
  return(list(
    nn.index = sby_nn_index,
    nn.dist  = sby_nn_dist
  ))
}
####
## Fim
#
