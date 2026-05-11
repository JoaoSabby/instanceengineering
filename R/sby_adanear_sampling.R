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
#'   sby_predictor_data,
#'   sby_target_vector,
#'   sby_over_ratio = 0.2,
#'   sby_under_ratio = 0.5,
#'   sby_k_over = 5L,
#'   sby_k_under = 5L,
#'   sby_seed = 42L,
#'   sby_audit = FALSE,
#'   sby_restore_types = TRUE,
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
#' @param sby_predictor_data Data frame, tibble ou matriz contendo variáveis preditoras numéricas. Não possui valor padrão e deve estar alinhado ao vetor alvo. Esse objeto define o espaço comum no qual serão calculadas tanto a dificuldade adaptativa do ADASYN quanto a proximidade NearMiss-1.
#' @param sby_target_vector Vetor atômico ou fator com exatamente duas classes e comprimento igual ao número de linhas de `sby_predictor_data`. Não possui valor padrão. A distribuição desse vetor determina a classe minoritária a ser expandida e a classe majoritária a ser reduzida.
#' @param sby_over_ratio Valor numérico escalar que controla a intensidade da sobreamostragem ADASYN antes da etapa NearMiss. O padrão é `0.2`. Valores maiores inserem mais exemplos sintéticos antes da subamostragem, podendo melhorar cobertura da minoria, mas também propagando ruído em regiões ambíguas.
#' @param sby_under_ratio Valor numérico escalar que controla a fração da classe majoritária retida depois da sobreamostragem. O padrão é `0.5`. Valores menores tornam o pipeline mais agressivo na redução majoritária; valores maiores preservam mais observações originais e tendem a manter maior diversidade majoritária.
#' @param sby_k_over Número inteiro positivo de vizinhos usados pela etapa ADASYN para estimar dificuldade local e gerar amostras sintéticas. O padrão é `5L`. Essa escolha influencia onde a expansão minoritária será concentrada e o grau de suavização da avaliação local.
#' @param sby_k_under Número inteiro positivo de vizinhos minoritários usados pela etapa NearMiss-1 para ranquear observações majoritárias. O padrão é `5L`. Essa escolha controla a sensibilidade da retenção majoritária à fronteira local criada após a sobreamostragem.
#' @param sby_seed Valor numérico inteiro utilizado para inicializar o gerador de números pseudoaleatórios nas duas etapas do pipeline. O padrão é `42L`. Uma semente fixa garante reprodutibilidade conjunta das amostras sintéticas, desempates, índices retidos e diagnósticos finais.
#' @param sby_audit Indicador lógico escalar que controla se o retorno deve incluir resultados intermediários das etapas ADASYN e NearMiss. O padrão é `FALSE`, retornando somente os dados balanceados finais. Quando `TRUE`, o retorno passa a ser uma lista com auditoria completa, útil para inspeção de contagens e validação experimental.
#' @param sby_restore_types Indicador lógico escalar que define se tipos numéricos originais devem ser restaurados ao final do pipeline. O padrão é `TRUE`. Mantê-lo ativado facilita reintegração com bases tabulares originais; desativá-lo mantém a saída no formato numérico produzido pelas transformações internas.
#' @param sby_knn_algorithm String escalar escolhida entre `"auto"`, `"kd_tree"`, `"cover_tree"`, `"brute"`, `"Kmknn"`, `"Vptree"`, `"Exhaustive"`, `"Annoy"` e `"Hnsw"`. O padrão é resolvido como `"auto"`. O mesmo algoritmo resolvido é usado nas etapas de sobreamostragem e subamostragem, garantindo consistência geométrica ao longo do pipeline.
#' @param sby_knn_engine String escalar escolhida entre `"auto"`, `"FNN"`, `"BiocNeighbors"` e `"RcppHNSW"`. O padrão é resolvido como `"auto"`. O engine selecionado determina a implementação KNN compartilhada pelas duas etapas e afeta velocidade, memória, paralelismo e suporte a métricas.
#' @param sby_distance_metric String escalar escolhida entre `"euclidean"`, `"ip"` e `"cosine"`. O padrão é `"euclidean"`. A métrica define simultaneamente a geometria da geração sintética e da retenção majoritária, podendo alterar tanto a localização dos pontos sintéticos quanto os exemplos majoritários preservados.
#' @param sby_knn_workers Número inteiro positivo de workers usados nas consultas KNN quando o engine efetivo oferece suporte. O padrão é `1L`. Aumentar esse número pode acelerar o pipeline completo, mas eleva uso de recursos e deve ser compatível com o backend escolhido.
#' @param sby_hnsw_m Número inteiro positivo que controla a conectividade do grafo HNSW no engine `RcppHNSW`. O padrão é `16L`. Valores maiores tendem a melhorar recall nas duas etapas do pipeline, com maior consumo de memória e tempo de construção do índice.
#' @param sby_hnsw_ef Número inteiro positivo que controla a largura dinâmica de exploração do HNSW. O padrão é `200L`. Valores maiores tornam a busca aproximada mais precisa e estável para ADASYN e NearMiss, mas aumentam a latência das consultas.
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
#' @return Tibble balanceado quando `sby_audit = FALSE`; lista com resultados de sobreamostragem, subamostragem, dados finais e diagnósticos quando `sby_audit = TRUE`.
#' @export
sby_adanear <- function(
  sby_predictor_data,
  sby_target_vector,
  sby_over_ratio = 0.2,
  sby_under_ratio = 0.5,
  sby_k_over = 5L,
  sby_k_under = 5L,
  sby_seed = 42L,
  sby_audit = FALSE,
  sby_restore_types = TRUE,
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
  sby_distance_metric <- match.arg(
    arg = sby_distance_metric
  )

  # Valida recursos paralelos e parametros HNSW
  sby_knn_workers <- sby_validate_knn_workers(
    sby_knn_workers = sby_knn_workers
  )
  sby_hnsw_params <- sby_validate_hnsw_params(
    sby_hnsw_m  = sby_hnsw_m,
    sby_hnsw_ef = sby_hnsw_ef
  )

  # Extrai parametros HNSW normalizados para uso posterior
  sby_hnsw_m  <- sby_hnsw_params$sby_hnsw_m
  sby_hnsw_ef <- sby_hnsw_params$sby_hnsw_ef

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

  # Executa sobreamostragem ADASYN mantendo matriz escalada para encadeamento
  sby_over_result <- sby_adasyn(
    sby_predictor_data          = sby_predictor_data,
    sby_target_vector           = sby_target_vector,
    sby_over_ratio              = sby_over_ratio,
    sby_k_over                  = sby_k_over,
    sby_seed                    = sby_seed,
    sby_audit                   = TRUE,
    sby_return_scaled           = TRUE,
    sby_restore_types           = FALSE,
    sby_knn_algorithm           = sby_knn_algorithm,
    sby_knn_engine             = sby_knn_engine,
    sby_distance_metric         = sby_distance_metric,
    sby_knn_workers             = sby_knn_workers,
    sby_hnsw_m                  = sby_hnsw_m,
    sby_hnsw_ef                 = sby_hnsw_ef
  )

  # Verifica se ha solicitacao de interrupcao apos sobreamostragem
  sby_adanear_check_user_interrupt()

  # Executa subamostragem NearMiss reutilizando escala e tipos inferidos
  sby_under_result <- sby_nearmiss(
    sby_predictor_data          = sby_over_result$sby_balanced_scaled$x,
    sby_target_vector           = sby_over_result$sby_balanced_scaled$y,
    sby_under_ratio             = sby_under_ratio,
    sby_k_under                 = sby_k_under,
    sby_seed                    = sby_seed,
    sby_audit                   = TRUE,
    sby_precomputed_scaling     = sby_over_result$sby_scaling_info,
    sby_input_already_scaled    = TRUE,
    sby_restore_types           = sby_restore_types,
    sby_type_info               = sby_over_result$sby_type_info,
    sby_knn_algorithm           = sby_knn_algorithm,
    sby_knn_engine             = sby_knn_engine,
    sby_distance_metric         = sby_distance_metric,
    sby_knn_workers             = sby_knn_workers,
    sby_hnsw_m                  = sby_hnsw_m,
    sby_hnsw_ef                 = sby_hnsw_ef
  )

  # Verifica se ha solicitacao de interrupcao apos subamostragem
  sby_adanear_check_user_interrupt()

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

  # Consolida diagnosticos das etapas de sobreamostragem e subamostragem
  sby_diagnostics <- list(
    sby_original_rows                         = NROW(sby_predictor_data),
    sby_after_oversampling_rows               = nrow(sby_over_result$sby_balanced_scaled$x),
    sby_final_rows                            = nrow(sby_under_result$sby_balanced_data),
    sby_knn_engine                           = sby_knn_engine,
    sby_distance_metric                       = sby_distance_metric,
    sby_knn_workers                           = sby_knn_workers,
    sby_hnsw_m                                = sby_diagnostic_hnsw_m,
    sby_hnsw_ef                               = sby_diagnostic_hnsw_ef,
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
