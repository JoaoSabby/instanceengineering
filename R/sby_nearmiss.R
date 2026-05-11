#' Aplicar subamostragem NearMiss-1 em dados binários
#'
#' @description
#' `sby_nearmiss()` executa subamostragem da classe majoritária pelo critério
#' NearMiss-1, retendo observações majoritárias que apresentam menor distância
#' média aos vizinhos da classe minoritária. A função é indicada para reduzir
#' predominância majoritária preservando exemplos próximos à fronteira de decisão.
#'
#' @usage
#' sby_nearmiss(
#'   sby_predictor_data,
#'   sby_target_vector,
#'   sby_under_ratio = 0.5,
#'   sby_k_under = 5L,
#'   sby_seed = 42L,
#'   sby_audit = FALSE,
#'   sby_precomputed_scaling = NULL,
#'   sby_input_already_scaled = FALSE,
#'   sby_restore_types = TRUE,
#'   sby_type_info = NULL,
#'   sby_knn_algorithm = c(
#'     "auto", "kd_tree", "cover_tree", "brute", "Kmknn", "Vptree", "Exhaustive", "Annoy", "Hnsw"
#'   ),
#'   sby_knn_engine = c(
#'     "auto", "FNN", "BiocNeighbors", "RcppHNSW"
#'   ),
#'   sby_distance_metric = c(
#'     "euclidean", "ip", "cosine"
#'   ),
#'   sby_knn_workers = 1L,
#'   sby_hnsw_m = 16L,
#'   sby_hnsw_ef = 200L
#' )
#'
#' @param sby_predictor_data Data frame, tibble ou matriz contendo variáveis preditoras numéricas. Não possui valor padrão e deve ter o mesmo número de linhas de `sby_target_vector`. Esses dados definem o espaço no qual as observações majoritárias serão ranqueadas por proximidade à classe minoritária.
#' @param sby_target_vector Vetor atômico ou fator binário associado às linhas de `sby_predictor_data`. Não possui valor padrão. Ele determina os papéis de classe minoritária e majoritária; inversões ou desalinhamentos modificam completamente quais linhas podem ser descartadas.
#' @param sby_under_ratio Valor numérico escalar que representa a fração da classe majoritária a ser retida após a subamostragem. O padrão é `0.5`. Valores menores removem mais observações majoritárias e aumentam o balanceamento, enquanto valores maiores preservam mais cobertura da distribuição original.
#' @param sby_k_under Número inteiro positivo de vizinhos minoritários usados para calcular a distância média do critério NearMiss-1. O padrão é `5L`. Valores maiores reduzem variância do ranqueamento; valores menores focalizam a fronteira local e podem selecionar exemplos muito próximos de ruído minoritário.
#' @param sby_seed Valor numérico inteiro utilizado para inicializar o gerador de números pseudoaleatórios. O padrão é `42L`. A semente torna reprodutíveis desempates, amostragens complementares e a ordem final das observações preservadas.
#' @param sby_audit Indicador lógico escalar que controla o retorno de metadados de auditoria. O padrão é `FALSE`, retornando apenas o tibble final. Quando `TRUE`, a função retorna lista com índices retidos, distribuições de classe, parâmetros resolvidos e informações de escala.
#' @param sby_precomputed_scaling Lista opcional com parâmetros de centralização e escala previamente calculados. O padrão é `NULL`, fazendo com que a função estime a padronização a partir de `sby_predictor_data`. Fornecer essa lista permite reutilizar uma escala externa ou herdada de etapa anterior, evitando inconsistência geométrica em pipelines encadeados.
#' @param sby_input_already_scaled Indicador lógico escalar que informa se `sby_predictor_data` já está em escala Z-score compatível com `sby_precomputed_scaling`. O padrão é `FALSE`. Quando `TRUE`, a função evita reaplicar padronização e interpreta as coordenadas de entrada como prontas para busca KNN.
#' @param sby_restore_types Indicador lógico escalar que define se tipos numéricos originais devem ser restaurados no retorno final. O padrão é `TRUE`. Essa escolha preserva compatibilidade com o esquema de dados de entrada; desativá-la reduz pós-processamento e mantém valores no formato numérico resultante das operações matriciais.
#' @param sby_type_info Lista opcional com metadados dos tipos numéricos originais dos preditores. O padrão é `NULL`, fazendo a função inferir os tipos quando necessário. Fornecer esse objeto é útil em pipelines encadeados, pois garante que a restauração de tipos use exatamente o mesmo diagnóstico da etapa anterior.
#' @param sby_knn_algorithm String escalar escolhida entre `"auto"`, `"kd_tree"`, `"cover_tree"`, `"brute"`, `"Kmknn"`, `"Vptree"`, `"Exhaustive"`, `"Annoy"` e `"Hnsw"`. O padrão é `"auto"` após resolução por `match.arg()`. A escolha define o método de consulta dos vizinhos minoritários e altera desempenho, determinismo e compatibilidade com métricas.
#' @param sby_knn_engine String escalar escolhida entre `"auto"`, `"FNN"`, `"BiocNeighbors"` e `"RcppHNSW"`. O padrão é `"auto"` após resolução. O engine controla a biblioteca usada na busca KNN e, consequentemente, o suporte a busca exata, aproximada, paralelização e métricas não euclidianas.
#' @param sby_distance_metric String escalar escolhida entre `"euclidean"`, `"ip"` e `"cosine"`. O padrão é `"euclidean"`. A métrica define a distância média usada para decidir quais observações majoritárias são mais próximas da classe minoritária e devem ser retidas.
#' @param sby_knn_workers Número inteiro positivo de workers usados por engines que aceitam paralelização. O padrão é `1L`. Mais workers podem reduzir latência em bases grandes, mas aumentam consumo de CPU e podem exigir engines compatíveis com execução paralela.
#' @param sby_hnsw_m Número inteiro positivo que define a conectividade do grafo HNSW quando `RcppHNSW` é usado. O padrão é `16L`. Valores maiores podem melhorar a fidelidade da vizinhança aproximada para o ranqueamento NearMiss, com maior custo de memória.
#' @param sby_hnsw_ef Número inteiro positivo que define a quantidade dinâmica de candidatos avaliados na busca HNSW. O padrão é `200L`. Aumentar esse valor tende a produzir vizinhos aproximados mais confiáveis e reduz risco de retenção baseada em vizinhos subótimos, mas torna a execução mais lenta.
#'
#' @details
#' A função utiliza uma arquitetura de busca espacial configurável para calcular
#' vizinhos próximos sobre preditores numéricos previamente padronizados por
#' Z-score. A seleção de `sby_knn_engine`, `sby_knn_algorithm` e
#' `sby_distance_metric` controla simultaneamente o provedor computacional, a
#' estratégia de indexação e a geometria usada para comparar observações. Quando
#' `sby_knn_engine = "auto"`, o pacote resolve automaticamente um engine
#' compatível com os demais argumentos e com o número de workers solicitado.
#'
#' Combinações válidas entre engine, tipo de busca e métrica:
#'
#' | Engine | Tipo de busca | Métricas suportadas |
#' |---|---|---|
#' | `FNN` | Exata | `euclidean` |
#' | `BiocNeighbors` | Exata/Aproximada | `euclidean`, `cosine` |
#' | `RcppHNSW` | Aproximada | `euclidean`, `ip`, `cosine` |
#'
#' A distância euclidiana corresponde à geometria padrão em espaços contínuos e
#' é definida por \eqn{d(x, y) = \sqrt{\sum_i (x_i - y_i)^2}}. O produto interno
#' é expresso como distância por \eqn{d(x, y) = 1 - \sum_i x_i y_i}. Atenção:
#' para `sby_distance_metric = "ip"`, o pacote realiza normalização L2 prévia
#' automática das matrizes envolvidas na busca para garantir consistência
#' espacial da métrica e evitar que diferenças de norma dominem a vizinhança. A
#' distância de cosseno é definida por \eqn{d(x, y) = 1 - \frac{\sum_i x_i y_i}{\sqrt{\sum_i x_i^2} \sqrt{\sum_i y_i^2}}}
#' e privilegia a orientação angular dos vetores em vez da magnitude absoluta.
#'
#' Em `FNN`, os algoritmos `kd_tree`, `cover_tree`, `brute` e `auto` operam de
#' forma exata e aceitam somente `euclidean`. Em `BiocNeighbors`, `Kmknn`,
#' `Vptree` e `Exhaustive` representam alternativas exatas, enquanto `Annoy` e
#' `Hnsw` representam alternativas aproximadas; nesse engine, `euclidean` e
#' `cosine` são aceitas. Em `RcppHNSW`, a busca é aproximada por grafo HNSW e os
#' parâmetros `sby_hnsw_m` e `sby_hnsw_ef` controlam, respectivamente, a
#' conectividade estrutural do grafo e a largura dinâmica da exploração.
#'
#' @references
#' He, H., Bai, Y., Garcia, E. A., & Li, S. (2008). ADASYN: Adaptive synthetic
#' sampling approach for imbalanced learning. In *2008 IEEE International Joint
#' Conference on Neural Networks* (pp. 1322-1328). IEEE.
#'
#' Malkov, Y. A., & Yashunin, D. A. (2018). Efficient and robust approximate
#' nearest neighbor search using Hierarchical Navigable Small World graphs.
#' *IEEE Transactions on Pattern Analysis and Machine Intelligence*, 42(4),
#' 824-836.
#'
#' @return Tibble balanceado quando `sby_audit = FALSE`; lista de auditoria com dados balanceados, índices, diagnósticos e escala quando `sby_audit = TRUE`.
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
  sby_knn_algorithm = c("auto", "kd_tree", "cover_tree", "brute", "Kmknn", "Vptree", "Exhaustive", "Annoy", "Hnsw"),
  sby_knn_engine = c("auto", "FNN", "BiocNeighbors", "RcppHNSW"),
  sby_distance_metric = c("euclidean", "ip", "cosine"),
  sby_knn_workers = 1L,
  sby_hnsw_m = 16L,
  sby_hnsw_ef = 200L
){
  
  # Verifica se ha solicitacao de interrupcao pelo usuario
  sby_adanear_check_user_interrupt()

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

  # Resolve o engine KNN declarado para uma opcao suportada
  sby_knn_engine <- match.arg(
    arg = sby_knn_engine
  )
  sby_distance_metric <- match.arg(
    arg = sby_distance_metric
  )

  # Valida a quantidade de workers para calculo KNN
  sby_knn_workers <- sby_validate_knn_workers(
    sby_knn_workers = sby_knn_workers
  )

  # Valida parametros HNSW usados por engine aproximado
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
    sby_adanear_abort(
      sby_message = "'sby_k_under' deve ser inteiro positivo"
    )
  }

  # Converte os preditores para matriz numerica com nomes preservados
  sby_x_matrix <- sby_adanear_as_numeric_matrix(
    sby_predictor_data = sby_predictor_data
  )
  
  colnames(sby_x_matrix) <- sby_adanear_get_column_names(
    sby_predictor_data = sby_predictor_data
  )

  # Resolve o engine KNN automatico conforme a configuracao de workers
  sby_knn_engine <- sby_resolve_knn_engine(
    sby_knn_engine = sby_knn_engine,
    sby_knn_workers = sby_knn_workers
  )

  # Resolve o algoritmo KNN automatico conforme a dimensionalidade dos dados
  sby_knn_algorithm <- sby_resolve_knn_algorithm(
    sby_knn_algorithm          = sby_knn_algorithm,
    sby_predictor_column_count = NCOL(sby_x_matrix),
    sby_knn_engine             = sby_knn_engine
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
    sby_adanear_abort(
      sby_message = "'sby_type_info' deve ter uma linha por coluna de 'sby_predictor_data'"
    )
  }

  # Define parametros de escala a partir do estado informado da entrada
  sby_scaling_info <- if(isTRUE(sby_input_already_scaled)){

    # Verifica se parametros de escala foram fornecidos para entrada ja escalada
    if(is.null(sby_precomputed_scaling)){

      # Aborta quando nao ha referencia para restauracao da escala original
      sby_adanear_abort(
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
  sby_adanear_check_user_interrupt()

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
      sby_adanear_abort(
        sby_message = "Sem linhas minoritarias suficientes para NearMiss"
      )
    }

    # Define semente para manter reprodutibilidade dos engines KNN
    set.seed(
      seed = sby_seed
    )

    # Calcula vizinhos minoritarios mais proximos para cada linha majoritaria
    sby_knn_result <- sby_get_knnx(
      sby_data                    = sby_minority_matrix,
      sby_query                   = sby_majority_matrix,
      sby_k                       = sby_effective_k,
      sby_knn_algorithm           = sby_knn_algorithm,
      sby_knn_engine             = sby_knn_engine,
      sby_distance_metric         = sby_distance_metric,
      sby_knn_workers             = sby_knn_workers,
      sby_hnsw_m                  = sby_hnsw_m,
      sby_hnsw_ef                 = sby_hnsw_ef
    )

    # Verifica se ha solicitacao de interrupcao apos calculo KNN
    sby_adanear_check_user_interrupt()

    # Ordena exemplos majoritarios pelo criterio NearMiss de distancia media
    sby_mean_distances <- rowMeans(
      x = sby_knn_result$nn.dist
    )

    # Verifica se ha solicitacao de interrupcao antes da selecao final
    sby_adanear_check_user_interrupt()

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

  # Define metadado de conectividade HNSW usado no diagnostico
  sby_diagnostic_hnsw_m <- ifelse(
    test = identical(
      x = sby_knn_engine,
      y = "RcppHNSW"
    ),
    yes = sby_hnsw_m,
    no  = NA_integer_
  )

  # Define metadado de busca HNSW usado no diagnostico
  sby_diagnostic_hnsw_ef <- ifelse(
    test = identical(
      x = sby_knn_engine,
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
    sby_knn_engine                 = sby_knn_engine,
    sby_distance_metric             = sby_distance_metric,
    sby_knn_workers                 = sby_knn_workers,
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
