#' Executar balanceamento combinado ADASYN e NearMiss-1
#'
#' @description
#' `sby_adanear()` executa um pipeline híbrido de balanceamento para problemas
#' binários: primeiro aplica ADASYN para expandir adaptativamente a classe
#' minoritária e, em seguida, aplica NearMiss-1 para reduzir a classe majoritária.
#' O objetivo é combinar geração sintética em regiões difíceis com retenção
#' seletiva de exemplos majoritários próximos à fronteira de decisão.
#'
#' @usage
#' sby_adanear(
#'   sby_formula,
#'   sby_data,
#'   sby_over_ratio = 0.2,
#'   sby_under_ratio = 0.5,
#'   sby_knn_over_k = 5L,
#'   sby_knn_under_k = 5L,
#'   sby_seed = sample.int(10L^5L, 1L),
#'   sby_audit = FALSE,
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
#' @param sby_formula Fórmula no formato `alvo ~ preditores` usada para identificar uma única coluna de desfecho binário e as colunas preditoras numéricas em `sby_data`. Não possui valor padrão; use `alvo ~ .` para selecionar todos os demais campos como preditores.
#' @param sby_data Data frame, tibble ou matriz com a coluna de desfecho e as variáveis preditoras numéricas referenciadas em `sby_formula`. Não possui valor padrão. Esse objeto define o espaço comum no qual serão calculadas tanto a dificuldade adaptativa do ADASYN quanto a proximidade NearMiss-1.
#' @param sby_over_ratio Valor numérico escalar que controla a intensidade da sobreamostragem ADASYN antes da etapa NearMiss. O padrão é `0.2`. Valores maiores inserem mais exemplos sintéticos antes da subamostragem, podendo melhorar cobertura da minoria, mas também propagando ruído em regiões ambíguas.
#' @param sby_under_ratio Valor numérico escalar no intervalo `(0, 1]` que controla a razão mínima desejada entre minoria e maioria após o NearMiss-1. O padrão `0.5` permite reter até duas vezes a quantidade minoritária; `1` reduz a maioria até igualar a minoria quando houver registros suficientes.
#' @param sby_knn_over_k Número inteiro positivo de vizinhos usados pela etapa ADASYN para estimar dificuldade local e gerar amostras sintéticas. O padrão é `5L`. Essa escolha influencia onde a expansão minoritária será concentrada e o grau de suavização da avaliação local.
#' @param sby_knn_under_k Número inteiro positivo de vizinhos minoritários usados pela etapa NearMiss-1 para ranquear observações majoritárias. O padrão é `5L`. Essa escolha controla a sensibilidade da retenção majoritária à fronteira local criada após a sobreamostragem.
#' @param sby_seed Valor numérico inteiro utilizado para inicializar o gerador de números pseudoaleatórios nas duas etapas do pipeline. O padrão é `sample.int(10L^5L, 1L)`, gerando uma semente inteira aleatória quando o usuário não informa valor. Informe uma semente fixa para garantir reprodutibilidade conjunta das amostras sintéticas, desempates, índices retidos e diagnósticos finais.
#' @param sby_audit Indicador lógico escalar que controla se o retorno deve incluir resultados intermediários das etapas ADASYN e NearMiss. O padrão é `FALSE`, retornando somente os dados balanceados finais. Quando `TRUE`, o retorno passa a ser uma lista com auditoria completa, útil para inspeção de contagens e validação experimental.
#' @param sby_restore_types Indicador lógico escalar que define se tipos numéricos originais devem ser restaurados ao final do pipeline. O padrão é `TRUE`. Mantê-lo ativado facilita reintegração com bases tabulares originais; desativá-lo mantém a saída no formato numérico produzido pelas transformações internas.
#' @param sby_knn_algorithm String escalar escolhida entre `"auto"`, `"kd_tree"`, `"cover_tree"`, `"brute"`, `"Kmknn"`, `"Vptree"`, `"Exhaustive"`, `"Annoy"` e `"Hnsw"`. O padrão é resolvido como `"auto"`. O mesmo algoritmo resolvido é usado nas etapas de sobreamostragem e subamostragem, garantindo consistência geométrica ao longo do pipeline.
#' @param sby_knn_engine String escalar escolhida entre `"auto"`, `"FNN"`, `"BiocNeighbors"` e `"RcppHNSW"`. O padrão é resolvido como `"auto"`. O engine selecionado determina a implementação KNN compartilhada pelas duas etapas e afeta velocidade, memória, paralelismo e suporte a métricas.
#' @param sby_knn_distance_metric String escalar escolhida entre `"euclidean"`, `"ip"` e `"cosine"`. O padrão é `"euclidean"`. A métrica define simultaneamente a geometria da geração sintética e da retenção majoritária, podendo alterar tanto a localização dos pontos sintéticos quanto os exemplos majoritários preservados.
#' @param sby_knn_workers Número inteiro positivo de workers usados nas consultas KNN quando o engine efetivo oferece suporte. O padrão é `1L`. Aumentar esse número pode acelerar o pipeline completo, mas eleva uso de recursos e deve ser compatível com o backend escolhido.
#' @param sby_knn_hnsw_m Número inteiro positivo que controla a conectividade do grafo HNSW no engine `RcppHNSW`. O padrão é `16L`. Valores maiores tendem a melhorar recall nas duas etapas do pipeline, com maior consumo de memória e tempo de construção do índice.
#' @param sby_knn_hnsw_ef Número inteiro positivo que controla a largura dinâmica de exploração do HNSW. O padrão é `200L`. Valores maiores tornam a busca aproximada mais precisa e estável para ADASYN e NearMiss, mas aumentam a latência das consultas.
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
#' @return Tibble balanceado quando `sby_audit = FALSE`; lista com resultados de sobreamostragem, subamostragem, dados finais e diagnósticos quando `sby_audit = TRUE`.
#' @export
sby_adanear <- function(
  sby_formula,
  sby_data,
  sby_over_ratio = 0.2,
  sby_under_ratio = 0.5,
  sby_knn_over_k = 5L,
  sby_knn_under_k = 5L,
  sby_seed = sample.int(10L^5L, 1L),
  sby_audit = FALSE,
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

  # Resolve engine e algoritmo KNN automaticos para ambas as etapas
  sby_knn_engine <- sby_resolve_knn_engine(
    sby_knn_engine = sby_knn_engine,
    sby_knn_workers = sby_knn_workers
  )
  sby_knn_algorithm <- sby_resolve_knn_algorithm(
    sby_knn_algorithm          = sby_knn_algorithm,
    sby_predictor_column_count = NCOL(sby_predictor_data),
    sby_knn_engine             = sby_knn_engine
  )

  # Congela os papeis originais para que NearMiss nao remova a classe rara
  # quando o ADASYN fizer a minoria ultrapassar temporariamente a maioria
  sby_original_class_roles <- sby_get_binary_class_roles(
    sby_target_factor = as.factor(sby_target_vector)
  )

  # Executa sobreamostragem ADASYN mantendo matriz escalada para encadeamento
  sby_over_result <- sby_adasyn(
    sby_formula                 = TARGET ~ .,
    sby_data                    = sby_build_balanced_tibble(
      sby_predictor_data = sby_predictor_data,
      sby_target_vector  = sby_target_vector
    ),
    sby_over_ratio              = sby_over_ratio,
    sby_knn_over_k                  = sby_knn_over_k,
    sby_seed                    = sby_seed,
    sby_audit                   = TRUE,
    sby_return_scaled           = TRUE,
    sby_restore_types           = FALSE,
    sby_knn_algorithm           = sby_knn_algorithm,
    sby_knn_engine             = sby_knn_engine,
    sby_knn_distance_metric         = sby_knn_distance_metric,
    sby_knn_workers             = sby_knn_workers,
    sby_knn_hnsw_m                  = sby_knn_hnsw_m,
    sby_knn_hnsw_ef                 = sby_knn_hnsw_ef
  )

  # Verifica se ha solicitacao de interrupcao apos sobreamostragem
  sby_adanear_check_user_interrupt()

  # Executa subamostragem NearMiss reutilizando escala e tipos inferidos
  sby_under_result <- sby_nearmiss(
    sby_formula                 = TARGET ~ .,
    sby_data                    = sby_build_balanced_tibble(
      sby_predictor_data = sby_over_result$sby_balanced_scaled$x,
      sby_target_vector  = sby_over_result$sby_balanced_scaled$y
    ),
    sby_under_ratio             = sby_under_ratio,
    sby_knn_under_k                 = sby_knn_under_k,
    sby_seed                    = sby_seed,
    sby_audit                   = TRUE,
    sby_precomputed_scaling     = sby_over_result$sby_scaling_info,
    sby_input_already_scaled    = TRUE,
    sby_restore_types           = sby_restore_types,
    sby_type_info               = sby_over_result$sby_type_info,
    sby_fixed_minority_label    = sby_original_class_roles$sby_minority_label,
    sby_fixed_majority_label    = sby_original_class_roles$sby_majority_label,
    sby_knn_algorithm           = sby_knn_algorithm,
    sby_knn_engine             = sby_knn_engine,
    sby_knn_distance_metric         = sby_knn_distance_metric,
    sby_knn_workers             = sby_knn_workers,
    sby_knn_hnsw_m                  = sby_knn_hnsw_m,
    sby_knn_hnsw_ef                 = sby_knn_hnsw_ef
  )

  # Verifica se ha solicitacao de interrupcao apos subamostragem
  sby_adanear_check_user_interrupt()

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

  # Consolida diagnosticos das etapas de sobreamostragem e subamostragem
  sby_diagnostics <- list(
    sby_original_rows                         = NROW(sby_predictor_data),
    sby_after_oversampling_rows               = nrow(sby_over_result$sby_balanced_scaled$x),
    sby_final_rows                            = nrow(sby_under_result$sby_balanced_data),
    sby_knn_engine                           = sby_knn_engine,
    sby_knn_distance_metric                       = sby_knn_distance_metric,
    sby_knn_workers                           = sby_knn_workers,
    sby_knn_hnsw_m                                = sby_diagnostic_hnsw_m,
    sby_knn_hnsw_ef                               = sby_diagnostic_hnsw_ef,
    sby_original_class_distribution           = table(as.factor(sby_target_vector)),
    sby_after_oversampling_class_distribution = table(sby_over_result$sby_balanced_scaled$y),
    sby_final_class_distribution              = table(sby_under_result$sby_balanced_data$TARGET)
  )

  # Consolida resultado completo para retorno auditavel
  sby_result <- list(
    sby_oversampling_result  = sby_over_result,
    sby_undersampling_result = sby_under_result,
    sby_balanced_data        = sby_under_result$sby_balanced_data,
    sby_diagnostics          = sby_diagnostics
  )

  # Retorna estrutura completa quando auditoria foi solicitada
  if(isTRUE(sby_audit)){

    # Entrega resultados intermediarios, dados finais e diagnosticos ao chamador
    return(sby_result)
  }

  # Retorna apenas os dados balanceados no fluxo operacional padrao
  return(sby_under_result$sby_balanced_data)
}
####
## Fim
#
