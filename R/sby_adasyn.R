#' Aplicar sobreamostragem ADASYN em dados binários
#'
#' @description
#' `sby_adasyn()` executa sobreamostragem adaptativa da classe minoritária em um
#' problema binário, gerando observações sintéticas em regiões nas quais a classe
#' minoritária está localmente mais cercada por exemplos majoritários. A função
#' retorna um tibble balanceado ou, quando solicitado, uma estrutura de auditoria
#' com dados intermediários, escala, classes e diagnósticos.
#'
#' @usage
#' sby_adasyn(
#'   sby_formula,
#'   sby_data,
#'   sby_over_ratio = 0.2,
#'   sby_knn_over_k = 5L,
#'   sby_seed = sample.int(10L^5L, 1L),
#'   sby_audit = FALSE,
#'   sby_return_scaled = FALSE,
#'   sby_restore_types = TRUE,
#'   sby_knn_algorithm = c(
#'     "auto", "kd_tree", "cover_tree", "brute", "Kmknn", "Vptree", "Exhaustive", "Annoy", "Hnsw"
#'   ),
#'   sby_knn_engine = c(
#'     "auto", "FNN", "BiocNeighbors", "RcppHNSW"
#'   ),
#'   sby_knn_distance_metric = c(
#'     "euclidean", "ip", "cosine"
#'   ),
#'   sby_knn_workers = 1L,
#'   sby_knn_hnsw_m = 16L,
#'   sby_knn_hnsw_ef = 200L
#' )
#'
#' @param sby_formula Fórmula no formato `alvo ~ preditores` usada para identificar uma única coluna de desfecho binário e as colunas preditoras numéricas em `sby_data`. O lado direito deve referenciar apenas colunas ja existentes; transformacoes, interacoes e offsets precisam ser materializados antes da chamada. Não possui valor padrão; use `alvo ~ .` para selecionar todos os demais campos como preditores.
#' @param sby_data Data frame, tibble ou matriz com a coluna de desfecho e as variáveis preditoras numéricas referenciadas em `sby_formula`. Não possui valor padrão. A escala e a distribuição das colunas preditoras influenciam diretamente a geração sintética, embora o pacote aplique padronização Z-score antes da busca.
#' @param sby_over_ratio Valor numérico escalar que controla a expansão relativa da classe minoritária. O padrão é `0.2`, indicando uma geração sintética moderada; em bases pequenas, qualquer valor positivo gera ao menos uma linha sintética para evitar abortos por arredondamento. Valores maiores aumentam a expansão da minoria, mas também elevam o risco de criar amostras sintéticas em regiões ruidosas.
#' @param sby_knn_over_k Número inteiro positivo de vizinhos usados para estimar a dificuldade local de cada observação minoritária no critério ADASYN. O padrão é `5L`. Valores maiores tornam a estimativa de dificuldade mais estável e global; valores menores enfatizam estruturas locais e podem reagir fortemente a outliers.
#' @param sby_seed Valor numérico inteiro utilizado para inicializar o gerador de números pseudoaleatórios. O padrão é `sample.int(10L^5L, 1L)`, gerando uma semente inteira aleatória quando o usuário não informa valor. Informe uma semente fixa para reproduzir exatamente os vizinhos empatados, as escolhas de interpolação e as matrizes sintéticas geradas.
#' @param sby_audit Indicador lógico escalar que controla o formato do retorno. O padrão é `FALSE`, retornando apenas o conjunto balanceado final. Quando `TRUE`, a função retorna uma lista com diagnósticos, parâmetros resolvidos e artefatos intermediários úteis para validação metodológica e depuração.
#' @param sby_return_scaled Indicador lógico escalar que define se a matriz balanceada ainda padronizada deve ser incluída no retorno de auditoria. O padrão é `FALSE`, evitando armazenamento adicional. Quando `TRUE`, permite encadear a saída com rotinas que reutilizam a mesma escala, como o pipeline combinado `sby_adanear()`.
#' @param sby_restore_types Indicador lógico escalar que define se tipos numéricos originais devem ser restaurados no tibble final. O padrão é `TRUE`. Essa restauração melhora compatibilidade com dados que entraram como inteiros ou classes numéricas específicas; quando `FALSE`, a saída tende a permanecer em representação numérica de ponto flutuante.
#' @param sby_knn_algorithm String escalar escolhida entre `"auto"`, `"kd_tree"`, `"cover_tree"`, `"brute"`, `"Kmknn"`, `"Vptree"`, `"Exhaustive"`, `"Annoy"` e `"Hnsw"`. O padrão é resolvido como `"auto"`. A seleção determina como os vizinhos minoritários e mistos serão consultados, afetando tempo de execução, exatidão e disponibilidade de métricas.
#' @param sby_knn_engine String escalar escolhida entre `"auto"`, `"FNN"`, `"BiocNeighbors"` e `"RcppHNSW"`. O padrão é resolvido como `"auto"`. Esse engine define a implementação concreta da busca espacial; a escolha pode favorecer exatidão determinística, integração Bioconductor ou busca aproximada de alta escala.
#' @param sby_knn_distance_metric String escalar escolhida entre `"euclidean"`, `"ip"` e `"cosine"`. O padrão é `"euclidean"`. A métrica altera a noção de proximidade usada para estimar dificuldade local e, portanto, pode mudar quais regiões recebem mais observações sintéticas.
#' @param sby_knn_workers Número inteiro positivo de workers disponibilizados para engines paralelizáveis. O padrão é `1L`. Aumentar o valor pode acelerar consultas KNN em matrizes grandes, mas eleva uso de CPU e pode modificar a escolha automática de engine.
#' @param sby_knn_hnsw_m Número inteiro positivo que controla a conectividade estrutural do grafo HNSW quando o engine efetivo é `"RcppHNSW"`. O padrão é `16L`. Valores maiores geralmente melhoram a recuperação de vizinhos aproximados e aumentam memória; valores menores reduzem custo com possível queda de qualidade.
#' @param sby_knn_hnsw_ef Número inteiro positivo que controla a largura dinâmica da busca HNSW. O padrão é `200L`. Elevar esse valor tende a aproximar o resultado da busca exata e torna a sobreamostragem mais estável, ao custo de consultas mais lentas.
#'
#' @details
#' A função utiliza uma arquitetura de busca espacial configurável para calcular
#' vizinhos próximos sobre preditores numéricos previamente padronizados por
#' Z-score. A seleção de `sby_knn_engine`, `sby_knn_algorithm` e
#' `sby_knn_distance_metric` controla simultaneamente o provedor computacional, a
#' estratégia de indexação e a geometria usada para comparar observações. Quando
#' `sby_knn_engine = "auto"`, o pacote resolve automaticamente um engine
#' compatível com os demais argumentos e com o número de workers solicitado.
#'
#' ## Opções de `sby_knn_distance_metric`
#'
#' A métrica escolhida deve ser compatível com `sby_knn_engine` e
#' `sby_knn_algorithm`. As opções são:
#'
#' ### `euclidean`
#'
#' Distância euclidiana, definida por
#' \eqn{d(x, y) = \sqrt{\sum_i (x_i - y_i)^2}}. É a opção padrão e a mais ampla
#' em termos de compatibilidade. Com `sby_knn_engine = "FNN"`, opera somente de
#' forma exata com `sby_knn_algorithm` em `"auto"`, `"kd_tree"`, `"cover_tree"`
#' ou `"brute"`. Com `sby_knn_engine = "BiocNeighbors"`, pode ser exata quando
#' `sby_knn_algorithm` é `"Kmknn"`, `"Vptree"` ou `"Exhaustive"`, ou aproximada
#' quando o algoritmo é `"Annoy"` ou `"Hnsw"`. Com
#' `sby_knn_engine = "RcppHNSW"`, a busca é sempre aproximada por grafo HNSW e os
#' parâmetros `sby_knn_hnsw_m` e `sby_knn_hnsw_ef` controlam conectividade e
#' exploração.
#'
#' ### `cosine`
#'
#' Distância de cosseno, definida por
#' \eqn{d(x, y) = 1 - \frac{\sum_i x_i y_i}{\sqrt{\sum_i x_i^2} \sqrt{\sum_i y_i^2}}}.
#' Privilegia a orientação angular dos vetores em vez da magnitude absoluta. Não
#' é aceita por `sby_knn_engine = "FNN"`. Com
#' `sby_knn_engine = "BiocNeighbors"`, pode ser exata com `"Kmknn"`, `"Vptree"`
#' ou `"Exhaustive"`, ou aproximada com `"Annoy"` ou `"Hnsw"`. Com
#' `sby_knn_engine = "RcppHNSW"`, a busca é aproximada por HNSW. Para manter a
#' consistência angular, o pacote aplica normalização L2 antes da consulta KNN.
#'
#' ### `ip`
#'
#' Produto interno expresso como distância por
#' \eqn{d(x, y) = 1 - \sum_i x_i y_i}. Essa opção é suportada apenas por
#' `sby_knn_engine = "RcppHNSW"`; portanto, a busca é sempre aproximada e depende
#' dos parâmetros HNSW `sby_knn_hnsw_m` e `sby_knn_hnsw_ef`. Ela não é compatível
#' com `sby_knn_engine = "FNN"` nem com `sby_knn_engine = "BiocNeighbors"`,
#' independentemente de `sby_knn_algorithm`. O pacote aplica normalização L2
#' prévia para evitar que diferenças de norma dominem a vizinhança.
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
#' @return Tibble balanceado quando `sby_audit = FALSE`; lista de auditoria com dados balanceados, diagnósticos e artefatos intermediários quando `sby_audit = TRUE`.
#' @export
sby_adasyn <- function(
  sby_formula,
  sby_data,
  sby_over_ratio = 0.2,
  sby_knn_over_k = 5L,
  sby_seed = sample.int(10L^5L, 1L),
  sby_audit = FALSE,
  sby_return_scaled = FALSE,
  sby_restore_types = TRUE,
  sby_knn_algorithm = c("auto", "kd_tree", "cover_tree", "brute", "Kmknn", "Vptree", "Exhaustive", "Annoy", "Hnsw"),
  sby_knn_engine = c("auto", "FNN", "BiocNeighbors", "RcppHNSW"),
  sby_knn_distance_metric = c("euclidean", "ip", "cosine"),
  sby_knn_workers = 1L,
  sby_knn_hnsw_m = 16L,
  sby_knn_hnsw_ef = 200L
){
  
  # Verifica se ha solicitacao de interrupcao pelo usuario
  sby_adanear_check_user_interrupt()

  # Resolve formula e dados em matriz de preditores e vetor alvo
  sby_formula_data <- sby_extract_formula_data(
    sby_formula = sby_formula,
    sby_data    = sby_data
  )
  sby_predictor_data <- sby_formula_data$sby_predictor_data
  sby_target_vector  <- sby_formula_data$sby_target_vector

  # Valida parametros logicos escalares de controle operacional
  sby_audit <- sby_validate_logical_scalar(
    sby_value = sby_audit,
    sby_name  = "sby_audit"
  )
  sby_return_scaled <- sby_validate_logical_scalar(
    sby_value = sby_return_scaled,
    sby_name  = "sby_return_scaled"
  )
  sby_restore_types <- sby_validate_logical_scalar(
    sby_value = sby_restore_types,
    sby_name  = "sby_restore_types"
  )

  # Resolve opcoes declaradas de algoritmo e engine KNN
  sby_knn_algorithm <- match.arg(
    arg = sby_knn_algorithm
  )
  sby_knn_engine <- match.arg(
    arg = sby_knn_engine
  )
  sby_knn_distance_metric <- match.arg(
    arg = sby_knn_distance_metric
  )

  # Valida recursos paralelos e parametros HNSW
  sby_knn_workers <- sby_validate_knn_workers(
    sby_knn_workers = sby_knn_workers
  )
  sby_hnsw_params <- sby_validate_hnsw_params(
    sby_knn_hnsw_m  = sby_knn_hnsw_m,
    sby_knn_hnsw_ef = sby_knn_hnsw_ef
  )

  # Extrai parametros HNSW normalizados para uso posterior
  sby_knn_hnsw_m  <- sby_hnsw_params$sby_knn_hnsw_m
  sby_knn_hnsw_ef <- sby_hnsw_params$sby_knn_hnsw_ef

  # Valida consistencia basica entre preditores, alvo e semente
  sby_validate_sampling_inputs(
    sby_predictor_data = sby_predictor_data,
    sby_target_vector  = sby_target_vector,
    sby_seed           = sby_seed
  )

  # Verifica se o numero de vizinhos de sobreamostragem e inteiro positivo
  sby_knn_over_k <- sby_validate_positive_integer_scalar(
    sby_value = sby_knn_over_k,
    sby_name  = "sby_knn_over_k"
  )

  # Converte preditores e preserva nomes de colunas para o processamento matricial
  sby_x_matrix <- sby_adanear_as_numeric_matrix(
    sby_predictor_data = sby_predictor_data
  )
  colnames(sby_x_matrix) <- sby_adanear_get_column_names(
    sby_predictor_data = sby_predictor_data
  )

  # Resolve engine e algoritmo KNN automaticos conforme dados e workers
  sby_knn_engine <- sby_resolve_knn_engine(
    sby_knn_engine = sby_knn_engine,
    sby_knn_workers = sby_knn_workers
  )
  sby_knn_algorithm <- sby_resolve_knn_algorithm(
    sby_knn_algorithm          = sby_knn_algorithm,
    sby_predictor_column_count = NCOL(sby_x_matrix),
    sby_knn_engine             = sby_knn_engine
  )

  # Prepara alvo, metadados de tipos e matriz padronizada para ADASYN
  sby_target_factor <- as.factor(
    x = sby_target_vector
  )
  sby_type_info <- sby_infer_numeric_column_types(
    sby_data_frame = sby_predictor_data
  )
  sby_scaling_info <- sby_compute_z_score_params(
    sby_x_matrix = sby_x_matrix
  )
  sby_x_scaled <- sby_apply_z_score_scaling_matrix(
    sby_x_matrix      = sby_x_matrix,
    sby_scaling_info = sby_scaling_info
  )

  # Verifica se ha solicitacao de interrupcao antes da geracao sintetica
  sby_adanear_check_user_interrupt()

  # Calcula quantidade de amostras sinteticas a gerar
  sby_synthetic_count <- sby_compute_minority_expansion_count(
    sby_target_factor = sby_target_factor,
    sby_over_ratio    = sby_over_ratio
  )

  # Define semente para manter reprodutibilidade da geracao ADASYN
  set.seed(
    seed = sby_seed
  )

  # Gera amostras sinteticas em escala padronizada
  sby_adasyn_result <- sby_generate_adasyn_samples(
    sby_x_scaled                 = sby_x_scaled,
    sby_target_factor            = sby_target_factor,
    sby_synthetic_count          = sby_synthetic_count,
    sby_knn_over_k                   = sby_knn_over_k,
    sby_knn_algorithm            = sby_knn_algorithm,
    sby_knn_engine              = sby_knn_engine,
    sby_knn_distance_metric          = sby_knn_distance_metric,
    sby_knn_workers              = sby_knn_workers,
    sby_knn_hnsw_m                   = sby_knn_hnsw_m,
    sby_knn_hnsw_ef                  = sby_knn_hnsw_ef
  )

  # Verifica se ha solicitacao de interrupcao apos a geracao sintetica
  sby_adanear_check_user_interrupt()

  # Restaura nomes de colunas e escala original dos preditores expandidos
  colnames(sby_adasyn_result$x) <- colnames(sby_x_matrix)
  sby_x_restored <- sby_revert_z_score_scaling_matrix(
    sby_x_matrix      = sby_adasyn_result$x,
    sby_scaling_info = sby_scaling_info
  )

  # Verifica se ha solicitacao de interrupcao apos reversao de escala
  sby_adanear_check_user_interrupt()

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

  # Combina preditores finais e alvo expandido em tibble balanceado
  sby_balanced_data <- sby_build_balanced_tibble(
    sby_predictor_data = sby_final_predictors,
    sby_target_vector  = sby_adasyn_result$y
  )

  # Define metadado de conectividade HNSW usado no diagnostico
  sby_diagnostic_hnsw_m <- ifelse(
    test = identical(
      x = sby_knn_engine,
      y = "RcppHNSW"
    ),
    yes = sby_knn_hnsw_m,
    no  = NA_integer_
  )

  # Define metadado de busca HNSW usado no diagnostico
  sby_diagnostic_hnsw_ef <- ifelse(
    test = identical(
      x = sby_knn_engine,
      y = "RcppHNSW"
    ),
    yes = sby_knn_hnsw_ef,
    no  = NA_integer_
  )

  # Consolida diagnosticos de entrada, saida e configuracao KNN
  sby_diagnostics <- list(
    sby_input_rows                = NROW(sby_x_matrix),
    sby_output_rows               = nrow(sby_balanced_data),
    sby_generated_rows            = nrow(sby_balanced_data) - nrow(sby_x_matrix),
    sby_knn_engine               = sby_knn_engine,
    sby_knn_distance_metric           = sby_knn_distance_metric,
    sby_knn_workers               = sby_knn_workers,
    sby_knn_hnsw_m                    = sby_diagnostic_hnsw_m,
    sby_knn_hnsw_ef                   = sby_diagnostic_hnsw_ef,
    sby_input_class_distribution  = table(sby_target_factor),
    sby_output_class_distribution = table(as.factor(sby_adasyn_result$y))
  )

  # Consolida resultado completo para retorno auditavel
  sby_result <- list(
    sby_balanced_data = sby_balanced_data,
    sby_type_info     = sby_type_info,
    sby_scaling_info  = sby_scaling_info,
    sby_diagnostics   = sby_diagnostics
  )

  # Inclui matriz escalada quando solicitada para encadeamento interno
  if(isTRUE(sby_return_scaled)){

    # Anexa representacao escalada ao resultado de auditoria
    sby_result$sby_balanced_scaled <- list(
      x = sby_adasyn_result$x,
      y = as.factor(sby_adasyn_result$y)
    )
  }

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
