#' Adicionar uma etapa recipes de balanceamento NearMiss-1
#'
#' @description
#' `sby_step_adanear()` adiciona a uma `recipe` uma etapa supervisionada de
#' subamostragem NearMiss-1 para problemas de classificação binária. A etapa é
#' ajustada durante `prep()` a partir de uma única coluna de desfecho selecionada
#' e, por padrão, é ignorada em novos dados durante `bake()` porque modifica o
#' número de linhas do conjunto processado.
#'
#' @usage
#' sby_step_adanear(
#'   sby_recipe,
#'   ...,
#'   sby_role = NA,
#'   sby_trained = FALSE,
#'   sby_columns = NULL,
#'   sby_under_ratio = 0.5,
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
#'   sby_hnsw_ef = 200L,
#'   sby_skip = TRUE,
#'   sby_id = recipes::rand_id("balance")
#' )
#'
#' @param sby_recipe Objeto `recipe` que receberá a etapa de balanceamento. Deve ser uma instância válida de `recipes::recipe()` contendo os preditores numéricos e a variável de desfecho selecionável por `...`. Não possui valor padrão, pois a etapa precisa ser anexada explicitamente a uma receita existente; a escolha determina em qual pipeline de pré-processamento a subamostragem será executada.
#' @param ... Seletores `recipes` ou `tidyselect` usados para identificar exatamente uma coluna de desfecho binário. O formato esperado é uma expressão de seleção não avaliada, como um nome de coluna, `all_outcomes()` ou outro seletor compatível. Não há valor padrão; selecionar uma coluna incorreta altera diretamente quais classes serão tratadas como minoritária e majoritária no balanceamento.
#' @param sby_role Valor de papel (`role`) armazenado na etapa para compatibilidade com a infraestrutura de `recipes`. Espera-se `NA` ou uma string escalar. O padrão é `NA`, indicando que a etapa não introduz um novo papel operacional; alterar esse valor afeta apenas metadados da etapa, não o cálculo dos vizinhos.
#' @param sby_trained Indicador lógico escalar que informa se a etapa já passou por `prep()`. O padrão é `FALSE`, estado adequado para a criação inicial da etapa. Esse valor é controlado internamente por `recipes`; defini-lo manualmente como `TRUE` sem os metadados correspondentes pode tornar a etapa inconsistente.
#' @param sby_columns Vetor de caracteres ou `NULL` com o nome da coluna de desfecho resolvida durante `prep()`. O padrão é `NULL`, indicando que a seleção ainda não foi treinada. Esse metadado define qual variável será removida dos preditores e usada como alvo no momento do balanceamento.
#' @param sby_under_ratio Valor numérico escalar que representa a fração da classe majoritária a ser retida pela subamostragem NearMiss-1. O padrão é `0.5`, preservando aproximadamente metade das observações majoritárias elegíveis. Valores menores tornam o conjunto final mais agressivamente balanceado, enquanto valores maiores preservam mais informação da classe majoritária.
#' @param sby_k_under Número inteiro positivo de vizinhos minoritários considerados pelo critério NearMiss-1 para ranquear observações majoritárias. O padrão é `5L`. Valores maiores suavizam o critério de proximidade por considerar uma vizinhança mais ampla; valores menores tornam a seleção mais sensível a fronteiras locais e possíveis ruídos.
#' @param sby_seed Valor numérico inteiro utilizado para inicializar o gerador de números pseudoaleatórios. O padrão é `42L`. Uma semente fixa garante reprodutibilidade dos empates, amostragens internas e índices retidos durante a subamostragem.
#' @param sby_audit Indicador lógico escalar que controla se metadados de auditoria devem ser preservados no resultado interno da etapa. O padrão é `FALSE`, priorizando uma saída operacional simples. Quando `TRUE`, facilita rastrear contagens, parâmetros resolvidos e diagnósticos, com maior custo de memória.
#' @param sby_restore_types Indicador lógico escalar que define se tipos numéricos inferidos dos preditores originais devem ser restaurados após o processamento matricial. O padrão é `TRUE`. Mantê-lo ativado favorece integração com pipelines que dependem de classes numéricas originais; desativá-lo preserva a representação matricial convertida com menor pós-processamento.
#' @param sby_knn_algorithm String escalar escolhida entre `"auto"`, `"kd_tree"`, `"cover_tree"`, `"brute"`, `"Kmknn"`, `"Vptree"`, `"Exhaustive"`, `"Annoy"` e `"Hnsw"`. O padrão é o vetor de escolhas com resolução por `match.arg()`, efetivamente selecionando `"auto"` quando o usuário não informa valor. Esse parâmetro determina a estrutura de busca KNN e afeta o compromisso entre exatidão, velocidade e compatibilidade com o engine.
#' @param sby_knn_engine String escalar escolhida entre `"auto"`, `"FNN"`, `"BiocNeighbors"` e `"RcppHNSW"`. O padrão é resolvido como `"auto"`, permitindo que o pacote escolha um provedor compatível. A escolha impacta dependências usadas, suporte a paralelismo, métricas disponíveis e possibilidade de busca aproximada.
#' @param sby_distance_metric String escalar escolhida entre `"euclidean"`, `"ip"` e `"cosine"`. O padrão é resolvido como `"euclidean"`. A métrica define a geometria da vizinhança; escolhas não euclidianas podem alterar substancialmente quais observações majoritárias são consideradas próximas à classe minoritária.
#' @param sby_knn_workers Número inteiro positivo de workers usados por engines com suporte a paralelização. O padrão é `1L`, executando em modo sequencial. Aumentar esse valor pode reduzir tempo de busca em bases maiores, mas também aumenta consumo de recursos e pode restringir a resolução automática do engine.
#' @param sby_hnsw_m Número inteiro positivo que controla a conectividade máxima por camada do índice HNSW quando `sby_knn_engine = "RcppHNSW"`. O padrão é `16L`. Valores maiores tendem a aumentar recall e custo de memória; valores menores reduzem o índice, mas podem degradar a qualidade dos vizinhos aproximados.
#' @param sby_hnsw_ef Número inteiro positivo que define o tamanho da lista dinâmica de candidatos explorados pelo HNSW. O padrão é `200L`. Valores maiores tornam a busca aproximada mais fiel à vizinhança exata, ao custo de maior latência; valores menores aceleram a consulta com possível perda de precisão.
#' @param sby_skip Indicador lógico escalar que define se a etapa deve ser ignorada ao aplicar `bake()` em novos dados. O padrão é `TRUE`, configuração recomendada para etapas que alteram o número de linhas durante treinamento. Definir `FALSE` força a aplicação em novos dados e pode ser inadequado para fluxos de predição.
#' @param sby_id String escalar que identifica unicamente a etapa dentro da `recipe`. O padrão é `recipes::rand_id("balance")`, gerando um identificador reprodutível apenas no contexto da construção da etapa. Esse valor é usado por `recipes` para rastreamento, impressão e manutenção do pipeline.
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
#' @return Objeto `recipe` com uma etapa `sby_step_adanear` adicionada ao pipeline.
#' @export
sby_step_adanear <- function(
  sby_recipe,
  ...,
  sby_role = NA,
  sby_trained = FALSE,
  sby_columns = NULL,
  sby_under_ratio = 0.5,
  sby_k_under = 5L,
  sby_seed = 42L,
  sby_audit = FALSE,
  sby_restore_types = TRUE,
  sby_knn_algorithm = c("auto", "kd_tree", "cover_tree", "brute", "Kmknn", "Vptree", "Exhaustive", "Annoy", "Hnsw"),
  sby_knn_engine = c("auto", "FNN", "BiocNeighbors", "RcppHNSW"),
  sby_distance_metric = c("euclidean", "ip", "cosine"),
  sby_knn_workers = 1L,
  sby_hnsw_m = 16L,
  sby_hnsw_ef = 200L,
  sby_skip = TRUE,
  sby_id = recipes::rand_id("balance")
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
  sby_skip <- sby_validate_logical_scalar(
    sby_value = sby_skip,
    sby_name  = "sby_skip"
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

  # Adiciona etapa configurada ao objeto recipe
  return(recipes::add_step(
    rec = sby_recipe,
    object = sby_step_adanear_new(
      sby_terms                   = sby_terms,
      sby_role                    = sby_role,
      sby_trained                 = sby_trained,
      sby_columns                 = sby_columns,
      sby_under_ratio             = sby_under_ratio,
      sby_k_under                 = sby_k_under,
      sby_seed                    = sby_seed,
      sby_audit                   = sby_audit,
      sby_restore_types           = sby_restore_types,
      sby_knn_algorithm           = sby_knn_algorithm,
      sby_knn_engine             = sby_knn_engine,
      sby_distance_metric         = sby_distance_metric,
      sby_knn_workers             = sby_knn_workers,
      sby_hnsw_m                  = sby_hnsw_params$sby_hnsw_m,
      sby_hnsw_ef                 = sby_hnsw_params$sby_hnsw_ef,
      sby_skip                    = sby_skip,
      sby_id                      = sby_id
    )
  ))
}
####
## Fim
#
