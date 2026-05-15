#' Adicionar uma etapa recipes de balanceamento ADASYN e NearMiss-1
#'
#' @description
#' `sby_step_adanear()` adiciona a uma `recipe` uma etapa supervisionada de
#' balanceamento híbrido ADASYN e NearMiss-1 para problemas de classificação binária. A etapa é
#' ajustada durante `prep()` a partir de uma única coluna de desfecho selecionada
#' e, por padrão, é ignorada em novos dados durante `bake()` porque modifica o
#' número de linhas do conjunto processado.
#'
#' @usage
#' sby_step_adanear(
#'   recipe,
#'   ...,
#'   role = NA,
#'   trained = FALSE,
#'   columns = NULL,
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
#'   sby_knn_hnsw_ef = 200L,
#'   skip = TRUE,
#'   id = recipes::rand_id("adanear")
#' )
#'
#' @param recipe Objeto `recipe` que receberá a etapa de balanceamento. Deve ser uma instância válida de `recipes::recipe()` contendo os preditores numéricos e a variável de desfecho selecionável por `...`. Não possui valor padrão, pois a etapa precisa ser anexada explicitamente a uma receita existente; a escolha determina em qual pipeline de pré-processamento a subamostragem será executada.
#' @param ... Seletores `recipes` ou `tidyselect` usados para identificar exatamente uma coluna de desfecho binário. O formato esperado é uma expressão de seleção não avaliada, como um nome de coluna, `all_outcomes()` ou outro seletor compatível. Não há valor padrão; selecionar uma coluna incorreta altera diretamente quais classes serão tratadas como minoritária e majoritária no balanceamento.
#' @param role Valor de papel (`role`) armazenado na etapa para compatibilidade com a infraestrutura de `recipes`. Espera-se `NA` ou uma string escalar. O padrão é `NA`, indicando que a etapa não introduz um novo papel operacional; alterar esse valor afeta apenas metadados da etapa, não o cálculo dos vizinhos.
#' @param trained Indicador lógico escalar que informa se a etapa já passou por `prep()`. O padrão é `FALSE`, estado adequado para a criação inicial da etapa. Esse valor é controlado internamente por `recipes`; defini-lo manualmente como `TRUE` sem os metadados correspondentes pode tornar a etapa inconsistente.
#' @param columns Vetor de caracteres ou `NULL` com o nome da coluna de desfecho resolvida durante `prep()`. O padrão é `NULL`, indicando que a seleção ainda não foi treinada. Esse metadado define qual variável será removida dos preditores e usada como alvo no momento do balanceamento.
#' @param sby_over_ratio Valor numérico escalar que controla a intensidade da sobreamostragem ADASYN antes da etapa NearMiss. O padrão é `0.2`. Valores maiores inserem mais exemplos sintéticos antes da subamostragem, podendo melhorar cobertura da minoria, mas também propagando ruído em regiões ambíguas.
#' @param sby_under_ratio Valor numérico escalar no intervalo `(0, 1]` que representa a razão mínima desejada entre minoria e maioria após o NearMiss-1. O padrão `0.5` permite reter até duas observações majoritárias para cada minoritária; `1` reduz a maioria até igualar a minoria.
#' @param sby_knn_over_k Número inteiro positivo de vizinhos usados pela etapa ADASYN para estimar dificuldade local e gerar amostras sintéticas. O padrão é `5L`. Essa escolha influencia onde a expansão minoritária será concentrada e o grau de suavização da avaliação local.
#' @param sby_knn_under_k Número inteiro positivo de vizinhos minoritários considerados pelo critério NearMiss-1 para ranquear observações majoritárias. O padrão é `5L`. Valores maiores suavizam o critério de proximidade por considerar uma vizinhança mais ampla; valores menores tornam a seleção mais sensível a fronteiras locais e possíveis ruídos.
#' @param sby_seed Valor numérico inteiro utilizado para inicializar o gerador de números pseudoaleatórios. O padrão é `sample.int(10L^5L, 1L)`, gerando uma semente inteira aleatória quando o usuário não informa valor. Informe uma semente fixa para garantir reprodutibilidade dos empates, amostragens internas e índices retidos durante o balanceamento.
#' @param sby_audit Indicador lógico escalar que controla se metadados de auditoria devem ser preservados no resultado interno da etapa. O padrão é `FALSE`, priorizando uma saída operacional simples. Quando `TRUE`, facilita rastrear contagens, parâmetros resolvidos e diagnósticos, com maior custo de memória.
#' @param sby_restore_types Indicador lógico escalar que define se tipos numéricos inferidos dos preditores originais devem ser restaurados após o processamento matricial. O padrão é `TRUE`. Mantê-lo ativado favorece integração com pipelines que dependem de classes numéricas originais; desativá-lo preserva a representação matricial convertida com menor pós-processamento.
#' @param sby_knn_algorithm String escalar escolhida entre `"auto"`, `"kd_tree"`, `"cover_tree"`, `"brute"`, `"Kmknn"`, `"Vptree"`, `"Exhaustive"`, `"Annoy"` e `"Hnsw"`. O padrão é o vetor de escolhas com resolução por `match.arg()`, efetivamente selecionando `"auto"` quando o usuário não informa valor. Esse parâmetro determina a estrutura de busca KNN e afeta o compromisso entre exatidão, velocidade e compatibilidade com o engine.
#' @param sby_knn_engine String escalar escolhida entre `"auto"`, `"FNN"`, `"BiocNeighbors"` e `"RcppHNSW"`. O padrão é resolvido como `"auto"`, permitindo que o pacote escolha um provedor compatível. A escolha impacta dependências usadas, suporte a paralelismo, métricas disponíveis e possibilidade de busca aproximada.
#' @param sby_knn_distance_metric String escalar escolhida entre `"euclidean"`, `"ip"` e `"cosine"`. O padrão é resolvido como `"euclidean"`. A métrica define a geometria da vizinhança; escolhas não euclidianas podem alterar substancialmente quais observações majoritárias são consideradas próximas à classe minoritária.
#' @param sby_knn_workers Número inteiro positivo de workers usados por engines com suporte a paralelização. O padrão é `1L`, executando em modo sequencial. Aumentar esse valor pode reduzir tempo de busca em bases maiores, mas também aumenta consumo de recursos e pode restringir a resolução automática do engine.
#' @param sby_knn_hnsw_m Número inteiro positivo que controla a conectividade máxima por camada do índice HNSW quando `sby_knn_engine = "RcppHNSW"`. O padrão é `16L`. Valores maiores tendem a aumentar recall e custo de memória; valores menores reduzem o índice, mas podem degradar a qualidade dos vizinhos aproximados.
#' @param sby_knn_hnsw_ef Número inteiro positivo que define o tamanho da lista dinâmica de candidatos explorados pelo HNSW. O padrão é `200L`. Valores maiores tornam a busca aproximada mais fiel à vizinhança exata, ao custo de maior latência; valores menores aceleram a consulta com possível perda de precisão.
#' @param skip Indicador lógico escalar que define se a etapa deve ser ignorada ao aplicar `bake()` em novos dados. O padrão é `TRUE`, configuração recomendada para etapas que alteram o número de linhas durante treinamento. Definir `FALSE` força a aplicação em novos dados e pode ser inadequado para fluxos de predição.
#' @param id String escalar que identifica unicamente a etapa dentro da `recipe`. O padrão é `recipes::rand_id("adanear")`, gerando um identificador reprodutível apenas no contexto da construção da etapa. Esse valor é usado por `recipes` para rastreamento, impressão e manutenção do pipeline.
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
#' @return Objeto `recipe` com uma etapa `sby_step_adanear` adicionada ao pipeline.
#' @export
sby_step_adanear <- function(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  columns = NULL,
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
  sby_knn_hnsw_ef = 200L,
  skip = TRUE,
  id = recipes::rand_id("adanear")
){
  
  # Verifica se ha solicitacao de interrupcao antes de configurar a etapa
  sby_adanear_check_user_interrupt()

  # Valida dependencias declaradas para a etapa recipes
  recipes::recipes_pkg_check(
    required_pkgs.step_sby_step_adanear()
  )

  # Captura seletores de desfecho informados pelo chamador
  sby_terms <- rlang::enquos(...)

  # Valida indicadores logicos de controle da etapa
  sby_audit <- sby_validate_logical_scalar(
    sby_value = sby_audit,
    sby_name  = "sby_audit"
  )
  sby_restore_types <- sby_validate_logical_scalar(
    sby_value = sby_restore_types,
    sby_name  = "sby_restore_types"
  )
  skip <- sby_validate_logical_scalar(
    sby_value = skip,
    sby_name  = "skip"
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

  # Adiciona etapa configurada ao objeto recipe
  return(recipes::add_step(
    rec = recipe,
    object = sby_step_sampling_new(
      sby_subclass                = "sby_step_adanear",
      sby_sampling_method         = "adanear",
      sby_terms                   = sby_terms,
      sby_role                    = role,
      sby_trained                 = trained,
      sby_columns                 = columns,
      sby_over_ratio              = sby_over_ratio,
      sby_under_ratio             = sby_under_ratio,
      sby_knn_over_k              = sby_knn_over_k,
      sby_knn_under_k             = sby_knn_under_k,
      sby_seed                    = sby_seed,
      sby_audit                   = sby_audit,
      sby_restore_types           = sby_restore_types,
      sby_knn_algorithm           = sby_knn_algorithm,
      sby_knn_engine              = sby_knn_engine,
      sby_knn_distance_metric     = sby_knn_distance_metric,
      sby_knn_workers             = sby_knn_workers,
      sby_knn_hnsw_m              = sby_hnsw_params$sby_knn_hnsw_m,
      sby_knn_hnsw_ef             = sby_hnsw_params$sby_knn_hnsw_ef,
      sby_skip                    = skip,
      sby_id                      = id
    )
  ))
}
####
## Fim
#
