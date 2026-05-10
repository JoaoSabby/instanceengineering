#' Aplicar subamostragem NearMiss em dados binarios
#'
#' @details
#' A funcao compoe a interface publica do pacote e valida os contratos de entrada antes de executar a etapa principal
#' O processamento preserva a semantica dos dados, registra pontos de diagnostico quando aplicavel e mantem retornos explicitos para facilitar auditoria em ambientes de producao
#' As chamadas auxiliares usam argumentos nomeados para reduzir ambiguidades durante manutencao e revisao de codigo
#'
#' @title Aplicar subamostragem NearMiss em dados binarios
#' @name sby_nearmiss
#' @param sby_predictor_data Data frame ou matriz com variaveis preditoras numericas
#' @param sby_target_vector Vetor de classe binaria associado as linhas de entrada
#' @param sby_under_ratio Fracao da classe majoritaria a ser retida apos a subamostragem
#' @param sby_k_under Numero de vizinhos minoritarios usados no criterio NearMiss
#' @param sby_seed Semente usada para controlar componentes estocasticos do processamento
#' @param sby_audit Indicador logico para retornar metadados completos de auditoria
#' @param sby_precomputed_scaling Lista opcional com parametros de centralizacao e escala previamente calculados
#' @param sby_input_already_scaled Indicador logico de que os preditores ja estao padronizados
#' @param sby_restore_types Indicador logico para restaurar tipos numericos originais ao final
#' @param sby_type_info Informacao opcional sobre os tipos numericos originais dos preditores
#' @param sby_knn_algorithm Algoritmo usado pelo backend FNN para busca de vizinhos proximos
#' @param sby_knn_backend Backend usado para calcular vizinhos proximos
#' @param sby_knn_workers Numero de workers usado por backends paralelizaveis
#' @param sby_bioc_neighbor_algorithm Algoritmo usado pelo backend BiocNeighbors
#' @param sby_hnsw_m Parametro de conectividade do indice HNSW no backend RcppHNSW
#' @param sby_hnsw_ef Tamanho da lista dinamica de construcao e busca HNSW no backend RcppHNSW
#'
#' @return Tibble balanceado quando `sby_audit = FALSE`; lista de auditoria quando `sby_audit = TRUE`
#' @export
sby_nearmiss <- function(
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
){
  
  # Verifica se ha solicitacao de interrupcao pelo usuario
  sby_over_under_check_user_interrupt()

  # Valida parametros logicos escalares de controle operacional
  sby_audit <- sby_validate_logical_scalar(
    sby_value = sby_audit,
    sby_name  = "sby_audit"
  )

  # Valida indicador de entrada previamente escalada
  sby_input_already_scaled <- sby_validate_logical_scalar(
    sby_value = sby_input_already_scaled,
    sby_name  = "sby_input_already_scaled"
  )

  # Valida indicador de restauracao de tipos numericos
  sby_restore_types <- sby_validate_logical_scalar(
    sby_value = sby_restore_types,
    sby_name  = "sby_restore_types"
  )

  # Resolve o algoritmo KNN declarado para uma opcao suportada
  sby_knn_algorithm <- match.arg(
    arg = sby_knn_algorithm
  )

  # Resolve o backend KNN declarado para uma opcao suportada
  sby_knn_backend <- match.arg(
    arg = sby_knn_backend
  )

  # Resolve o algoritmo BiocNeighbors declarado para uma opcao suportada
  sby_bioc_neighbor_algorithm <- match.arg(
    arg = sby_bioc_neighbor_algorithm
  )

  # Valida a quantidade de workers para calculo KNN
  sby_knn_workers <- sby_validate_knn_workers(
    sby_knn_workers = sby_knn_workers
  )

  # Valida parametros HNSW usados por backend aproximado
  sby_hnsw_params <- sby_validate_hnsw_params(
    sby_hnsw_m  = sby_hnsw_m,
    sby_hnsw_ef = sby_hnsw_ef
  )

  # Extrai parametros HNSW normalizados para uso posterior
  sby_hnsw_m  <- sby_hnsw_params$sby_hnsw_m
  sby_hnsw_ef <- sby_hnsw_params$sby_hnsw_ef

  # Valida consistencia basica entre preditores, alvo e semente
  sby_validate_sampling_inputs(
    sby_predictor_data = sby_predictor_data,
    sby_target_vector  = sby_target_vector,
    sby_seed           = sby_seed
  )

  # Verifica se o numero de vizinhos de subamostragem e valido
  if(!is.numeric(sby_k_under) || length(sby_k_under) != 1L || is.na(sby_k_under) || sby_k_under < 1L){

    # Aborta quando o parametro NearMiss nao representa inteiro positivo
    sby_over_under_abort(
      sby_message = "'sby_k_under' deve ser inteiro positivo"
    )
  }

  # Converte os preditores para matriz numerica com nomes preservados
  sby_x_matrix <- sby_over_under_as_numeric_matrix(
    sby_predictor_data = sby_predictor_data
  )
  
  colnames(sby_x_matrix) <- sby_over_under_get_column_names(
    sby_predictor_data = sby_predictor_data
  )

  # Resolve o algoritmo KNN automatico conforme a dimensionalidade dos dados
  sby_knn_algorithm <- sby_resolve_knn_algorithm(
    sby_knn_algorithm = sby_knn_algorithm,
    sby_predictor_column_count = NCOL(sby_x_matrix)
  )

  # Resolve o backend KNN automatico conforme a configuracao de workers
  sby_knn_backend <- sby_resolve_knn_backend(
    sby_knn_backend = sby_knn_backend,
    sby_knn_workers = sby_knn_workers
  )

  # Normaliza o vetor alvo para fator binario
  sby_target_factor <- as.factor(
    x = sby_target_vector
  )

  # Infere informacoes de tipo quando nao foram precomputadas
  if(is.null(sby_type_info)){

    # Calcula metadados de tipos numericos a partir dos preditores originais
    sby_type_info <- sby_infer_numeric_column_types(
      sby_data_frame = sby_predictor_data
    )
  }

  # Verifica compatibilidade entre metadados de tipos e colunas preditoras
  if(NCOL(sby_x_matrix) != nrow(sby_type_info)){

    # Aborta quando os metadados nao cobrem todas as colunas preditoras
    sby_over_under_abort(
      sby_message = "'sby_type_info' deve ter uma linha por coluna de 'sby_predictor_data'"
    )
  }

  # Define parametros de escala a partir do estado informado da entrada
  sby_scaling_info <- if(isTRUE(sby_input_already_scaled)){

    # Verifica se parametros de escala foram fornecidos para entrada ja escalada
    if(is.null(sby_precomputed_scaling)){

      # Aborta quando nao ha referencia para restauracao da escala original
      sby_over_under_abort(
        sby_message = "'sby_precomputed_scaling' e obrigatorio quando 'sby_input_already_scaled = TRUE'"
      )
    }

    # Reutiliza parametros de escala ja informados pelo chamador
    sby_precomputed_scaling
  }else if(is.null(sby_precomputed_scaling)){

    # Calcula parametros de escala a partir dos preditores convertidos
    sby_compute_z_score_params(
      sby_x_matrix = sby_x_matrix
    )
  }else{

    # Valida parametros de escala precomputados contra a largura dos preditores
    sby_validate_scaling_info(
      sby_scaling_info = sby_precomputed_scaling,
      sby_predictor_column_count = NCOL(sby_x_matrix)
    )

    # Reutiliza parametros de escala validados pelo fluxo atual
    sby_precomputed_scaling
  }

  # Define matriz de trabalho conforme o estado de escalonamento da entrada
  if(isTRUE(sby_input_already_scaled)){

    # Reutiliza matriz numerica porque a entrada ja esta padronizada
    sby_x_scaled <- sby_x_matrix
  }else{

    # Aplica padronizacao z-score usando parametros validados
    sby_x_scaled <- sby_apply_z_score_scaling_matrix(
      sby_x_matrix      = sby_x_matrix,
      sby_scaling_info = sby_scaling_info
    )
  }

  # Verifica se ha solicitacao de interrupcao antes do calculo principal
  sby_over_under_check_user_interrupt()

  # Calcula distribuicao de classes para decidir se ha desbalanceamento
  sby_class_counts <- table(
    sby_target_factor
  )

  # Mantem todos os registros quando as classes ja estao balanceadas
  if(sby_class_counts[[1L]] == sby_class_counts[[2L]]){

    # Define indices e objetos reduzidos sem remocao de linhas
    sby_retained_index <- seq_len(
      length.out = nrow(sby_x_scaled)
    )
    
    sby_reduced_scaled <- sby_x_scaled
    sby_reduced_target <- sby_target_factor
    
  }else{

    # Identifica rotulos e indices das classes minoritaria e majoritaria
    sby_class_roles    <- sby_get_binary_class_roles(
      sby_target_factor = sby_target_factor
    )
    
    sby_minority_index <- which(
      x = sby_target_factor == sby_class_roles$sby_minority_label
    )
    sby_majority_index <- which(
      x = sby_target_factor == sby_class_roles$sby_majority_label
    )

    # Separa matrizes escaladas por papel de classe
    sby_minority_matrix <- sby_x_scaled[sby_minority_index, , drop = FALSE]
    sby_majority_matrix <- sby_x_scaled[sby_majority_index, , drop = FALSE]

    # Calcula quantidade de exemplos majoritarios e vizinhos efetivos a reter
    sby_retained_majority_count <- sby_compute_majority_retention_count(
      sby_target_factor = sby_target_factor,
      sby_under_ratio   = sby_under_ratio
    )
    sby_effective_k <- min(
      as.integer(sby_k_under),
      nrow(sby_minority_matrix)
    )

    # Verifica se existem vizinhos minoritarios suficientes para o criterio
    if(sby_effective_k < 1L){

      # Aborta quando o conjunto minoritario nao possui linhas elegiveis
      sby_over_under_abort(
        sby_message = "Sem linhas minoritarias suficientes para NearMiss"
      )
    }

    # Define semente para manter reprodutibilidade dos backends KNN
    set.seed(
      seed = sby_seed
    )

    # Calcula vizinhos minoritarios mais proximos para cada linha majoritaria
    sby_knn_result <- sby_get_knnx(
      sby_data                    = sby_minority_matrix,
      sby_query                   = sby_majority_matrix,
      sby_k                       = sby_effective_k,
      sby_knn_algorithm           = sby_knn_algorithm,
      sby_knn_backend             = sby_knn_backend,
      sby_knn_workers             = sby_knn_workers,
      sby_bioc_neighbor_algorithm = sby_bioc_neighbor_algorithm,
      sby_hnsw_m                  = sby_hnsw_m,
      sby_hnsw_ef                 = sby_hnsw_ef
    )

    # Verifica se ha solicitacao de interrupcao apos calculo KNN
    sby_over_under_check_user_interrupt()

    # Ordena exemplos majoritarios pelo criterio NearMiss de distancia media
    sby_mean_distances <- rowMeans(
      x = sby_knn_result$nn.dist
    )

    # Verifica se ha solicitacao de interrupcao antes da selecao final
    sby_over_under_check_user_interrupt()

    # Seleciona indices majoritarios mais proximos e compoe conjunto retido
    sby_selected_order <- order(
      sby_mean_distances,
      decreasing = FALSE
    )
    sby_selected_majority_index <- sby_majority_index[sby_selected_order[seq_len(sby_retained_majority_count)]]
    sby_retained_index          <- sort(
      x = c(
        sby_minority_index,
        sby_selected_majority_index
      )
    )

    # Reduz matriz escalada e alvo aos indices retidos pelo criterio NearMiss
    sby_reduced_scaled <- sby_x_scaled[sby_retained_index, , drop = FALSE]
    sby_reduced_target <- sby_target_factor[sby_retained_index]
  }

  # Reverte padronizacao z-score para a escala original dos preditores
  sby_x_restored <- sby_revert_z_score_scaling_matrix(
    sby_x_matrix      = sby_reduced_scaled,
    sby_scaling_info = sby_scaling_info
  )

  # Define preditores finais com ou sem restauracao dos tipos originais
  sby_final_predictors <- if(sby_restore_types){

    # Restaura classes numericas originais e retorna estrutura tabular
    sby_restore_numeric_column_types(
      sby_x_matrix       = sby_x_restored,
      sby_type_info     = sby_type_info,
      sby_as_data_frame = TRUE
    )
  }else{

    # Converte matriz restaurada em data frame mantendo nomes originais
    sby_out <- as.data.frame(
      x = sby_x_restored,
      stringsAsFactors = FALSE
    )
    names(sby_out) <- sby_type_info$sby_column_name
    sby_out
  }

  # Combina preditores finais e alvo reduzido em tibble balanceado
  sby_balanced_data <- sby_build_balanced_tibble(
    sby_predictor_data = sby_final_predictors,
    sby_target_vector  = sby_reduced_target
  )

  # Define metadado do algoritmo BiocNeighbors usado no diagnostico
  sby_diagnostic_bioc_neighbor_algorithm <- ifelse(
    test = identical(
      x = sby_knn_backend,
      y = "BiocNeighbors"
    ),
    yes = sby_bioc_neighbor_algorithm,
    no  = NA_character_
  )

  # Define metadado de conectividade HNSW usado no diagnostico
  sby_diagnostic_hnsw_m <- ifelse(
    test = identical(
      x = sby_knn_backend,
      y = "RcppHNSW"
    ),
    yes = sby_hnsw_m,
    no  = NA_integer_
  )

  # Define metadado de busca HNSW usado no diagnostico
  sby_diagnostic_hnsw_ef <- ifelse(
    test = identical(
      x = sby_knn_backend,
      y = "RcppHNSW"
    ),
    yes = sby_hnsw_ef,
    no  = NA_integer_
  )

  # Consolida diagnosticos de entrada, saida e configuracao KNN
  sby_diagnostics <- list(
    sby_input_rows                  = nrow(sby_x_matrix),
    sby_output_rows                 = nrow(sby_balanced_data),
    sby_removed_rows                = nrow(sby_x_matrix) - nrow(sby_balanced_data),
    sby_knn_backend                 = sby_knn_backend,
    sby_knn_workers                 = sby_knn_workers,
    sby_bioc_neighbor_algorithm     = sby_diagnostic_bioc_neighbor_algorithm,
    sby_hnsw_m                      = sby_diagnostic_hnsw_m,
    sby_hnsw_ef                     = sby_diagnostic_hnsw_ef,
    sby_input_class_distribution    = table(sby_target_factor),
    sby_output_class_distribution   = table(as.factor(sby_reduced_target))
  )

  # Consolida resultado completo para retorno auditavel
  sby_result <- list(
    sby_balanced_data   = sby_balanced_data,
    sby_type_info       = sby_type_info,
    sby_scaling_info    = sby_scaling_info,
    sby_diagnostics     = sby_diagnostics,
    sby_balanced_scaled = list(
      x = sby_reduced_scaled,
      y = as.factor(sby_reduced_target)
    )
  )

  # Retorna estrutura completa quando auditoria foi solicitada
  if(isTRUE(sby_audit)){

    # Entrega dados balanceados, metadados e diagnosticos ao chamador
    return(sby_result)
  }

  # Retorna apenas os dados balanceados no fluxo operacional padrao
  return(sby_balanced_data)
}
####
## Fim
#
