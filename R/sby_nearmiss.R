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
#'   sby_formula,
#'   sby_data,
#'   sby_under_ratio = 0.5,
#'   sby_knn_under_k = 5L,
#'   sby_seed = sample.int(10L^5L, 1L),
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
#'   sby_knn_distance_metric = c(
#'     "euclidean", "ip", "cosine"
#'   ),
#'   sby_knn_workers = 1L,
#'   sby_knn_hnsw_m = 16L,
#'   sby_knn_hnsw_ef = 200L
#' )
#'
#' @param sby_formula Fórmula no formato `alvo ~ preditores` usada para identificar uma única coluna de desfecho binário e as colunas preditoras numéricas em `sby_data`. O lado direito deve referenciar apenas colunas ja existentes; transformacoes, interacoes e offsets precisam ser materializados antes da chamada. Não possui valor padrão; use `alvo ~ .` para selecionar todos os demais campos como preditores.
#' @param sby_data Data frame, tibble ou matriz com a coluna de desfecho e as variáveis preditoras numéricas referenciadas em `sby_formula`. Não possui valor padrão. Esses dados definem o espaço no qual as observações majoritárias serão ranqueadas por proximidade à classe minoritária.
#' @param sby_under_ratio Valor numérico escalar no intervalo `(0, 1]` que representa a razão mínima desejada entre minoria e maioria após a subamostragem. O padrão `0.5` permite reter até duas observações majoritárias para cada minoritária; `1` reduz a maioria até igualar a minoria.
#' @param sby_knn_under_k Número inteiro positivo de vizinhos minoritários usados para calcular a distância média do critério NearMiss-1. O padrão é `5L`. Valores maiores reduzem variância do ranqueamento; valores menores focalizam a fronteira local e podem selecionar exemplos muito próximos de ruído minoritário.
#' @param sby_seed Valor numérico inteiro utilizado para inicializar o gerador de números pseudoaleatórios. O padrão é `sample.int(10L^5L, 1L)`, gerando uma semente inteira aleatória quando o usuário não informa valor. Informe uma semente fixa para tornar reprodutíveis desempates, amostragens complementares e a ordem final das observações preservadas.
#' @param sby_audit Indicador lógico escalar que controla o retorno de metadados de auditoria. O padrão é `FALSE`, retornando apenas o tibble final. Quando `TRUE`, a função retorna lista com índices retidos, distribuições de classe, parâmetros resolvidos e informações de escala.
#' @param sby_precomputed_scaling Lista opcional com parâmetros de centralização e escala previamente calculados. O padrão é `NULL`, fazendo com que a função estime a padronização a partir de `sby_data`. Fornecer essa lista permite reutilizar uma escala externa ou herdada de etapa anterior, evitando inconsistência geométrica em pipelines encadeados.
#' @param sby_input_already_scaled Indicador lógico escalar que informa se `sby_data` já está em escala Z-score compatível com `sby_precomputed_scaling`. O padrão é `FALSE`. Quando `TRUE`, a função evita reaplicar padronização e interpreta as coordenadas de entrada como prontas para busca KNN.
#' @param sby_restore_types Indicador lógico escalar que define se tipos numéricos originais devem ser restaurados no retorno final. O padrão é `TRUE`. Essa escolha preserva compatibilidade com o esquema de dados de entrada; desativá-la reduz pós-processamento e mantém valores no formato numérico resultante das operações matriciais.
#' @param sby_type_info Lista opcional com metadados dos tipos numéricos originais dos preditores. O padrão é `NULL`, fazendo a função inferir os tipos quando necessário. Fornecer esse objeto é útil em pipelines encadeados, pois garante que a restauração de tipos use exatamente o mesmo diagnóstico da etapa anterior.
#' @param sby_fixed_minority_label Rótulo interno opcional usado por `sby_adanear()` para preservar a classe minoritária original após o ADASYN. O padrão `NULL` mantém o comportamento autônomo de inferir os papéis pelas contagens atuais.
#' @param sby_fixed_majority_label Rótulo interno opcional usado por `sby_adanear()` para preservar a classe majoritária original após o ADASYN. O padrão `NULL` mantém o comportamento autônomo de inferir os papéis pelas contagens atuais.
#' @param sby_knn_algorithm String escalar que escolhe a estratégia de busca KNN: `"auto"`, `"kd_tree"`, `"cover_tree"`, `"brute"`, `"Kmknn"`, `"Vptree"`, `"Exhaustive"`, `"Annoy"` ou `"Hnsw"`. Use `"auto"` para deixar o pacote escolher uma opção compatível com o engine e a dimensionalidade; informe uma alternativa explícita quando quiser controlar o compromisso entre exatidão, velocidade, memória e suporte a métricas. Consulte os detalhes para recomendações por algoritmo.
#' @param sby_knn_engine String escalar que escolhe a biblioteca usada para executar a busca KNN: `"auto"`, `"FNN"`, `"BiocNeighbors"` ou `"RcppHNSW"`. Na maioria dos casos, mantenha `"auto"`; informe explicitamente apenas quando precisar de uma implementação específica, paralelismo via `BiocNeighbors`, métricas não euclidianas ou busca aproximada HNSW por `RcppHNSW`. Consulte os detalhes para saber quando o engine precisa ser declarado.
#' @param sby_knn_distance_metric String escalar que define a geometria da vizinhança: `"euclidean"`, `"cosine"` ou `"ip"`. A escolha muda o significado de proximidade e também restringe engines e algoritmos disponíveis; `"euclidean"` é a opção mais geral, `"cosine"` privilegia direção angular e `"ip"` usa produto interno via `RcppHNSW`. Consulte os detalhes para recomendações.
#' @param sby_knn_workers Número inteiro positivo de workers usados por engines que aceitam paralelização. O padrão é `1L`. Mais workers podem reduzir latência em bases grandes, mas aumentam consumo de CPU e podem exigir engines compatíveis com execução paralela.
#' @param sby_knn_hnsw_m Número inteiro positivo usado apenas quando o engine efetivo é `"RcppHNSW"`. Controla a conectividade máxima do grafo (`M`): valores maiores aumentam a chance de recuperar vizinhos melhores e tornam o índice mais robusto, mas consomem mais memória e tempo de construção. O padrão `16L` costuma ser um bom equilíbrio; aumente em bases grandes, ruidosas ou de alta dimensionalidade quando recall for mais importante que memória.
#' @param sby_knn_hnsw_ef Número inteiro positivo usado apenas quando o engine efetivo é `"RcppHNSW"`. Controla a largura da lista dinâmica de candidatos (`ef`) durante construção/consulta: valores maiores aproximam a busca do resultado exato e estabilizam ADASYN/NearMiss, mas deixam as consultas mais lentas. O padrão `200L` prioriza qualidade; reduza para velocidade ou aumente quando a vizinhança aproximada precisar de mais fidelidade.
#'
#' @details
#' A função utiliza uma arquitetura KNN configurável sobre preditores numéricos
#' padronizados por Z-score. Três escolhas trabalham em conjunto:
#' `sby_knn_algorithm` define a estrutura/estratégia de busca,
#' `sby_knn_engine` define a implementação computacional e
#' `sby_knn_distance_metric` define a noção de proximidade.
#'
#' ## Preciso informar `sby_knn_engine`?
#'
#' Em geral, não: para o uso cotidiano, mantenha `sby_knn_engine = "auto"` e
#' `sby_knn_algorithm = "auto"`. Nesse modo, o pacote usa `FNN` em execução
#' sequencial e `BiocNeighbors` quando `sby_knn_workers > 1L`; o algoritmo também
#' é escolhido automaticamente pela dimensionalidade dos preditores. Informe o
#' engine explicitamente quando quiser garantir uma biblioteca específica, usar
#' paralelismo de `BiocNeighbors`, usar `RcppHNSW`, ou quando escolher um
#' algoritmo que pertence a uma família específica. Na API atual, informar apenas
#' `sby_knn_algorithm = "Hnsw"`, `"Annoy"`, `"Kmknn"`, `"Vptree"` ou
#' `"Exhaustive"` não substitui a escolha do engine: declare
#' `sby_knn_engine = "BiocNeighbors"` para esses algoritmos, ou
#' `sby_knn_engine = "RcppHNSW"` quando quiser a implementação HNSW do pacote
#' `RcppHNSW`.
#'
#' ## Engines disponíveis
#'
#' | Engine | Melhor para | Vantagens | Limitações e cuidados |
#' |---|---|---|---|
#' | `"auto"` | Uso recomendado quando você não precisa controlar a biblioteca. | Reduz configuração manual, escolhe um caminho compatível com workers e mantém uma boa opção padrão. | Não expressa intenção metodológica específica; se você precisa de HNSW, Annoy, cosseno, produto interno ou paralelismo controlado, prefira declarar engine/algoritmo. |
#' | `"FNN"` | Bases pequenas a médias, distância euclidiana, execução sequencial e busca exata. | Simples, rápido para baixa/média dimensionalidade, determinístico e sem aproximação. | Aceita apenas `"euclidean"` neste pacote; não usa `sby_knn_workers`; só combina com `"auto"`, `"kd_tree"`, `"cover_tree"` ou `"brute"`. |
#' | `"BiocNeighbors"` | Busca exata ou aproximada com suporte a paralelismo e métrica `"cosine"`. | Oferece vários algoritmos (`"Kmknn"`, `"Vptree"`, `"Exhaustive"`, `"Annoy"`, `"Hnsw"`), integra `BiocParallel` e permite escolher entre exatidão e aproximação. | Requer `BiocNeighbors` e `BiocParallel`; não suporta `"ip"` neste pacote; algoritmos aproximados podem variar em recall. |
#' | `"RcppHNSW"` | Bases grandes, alta dimensionalidade, métricas `"cosine"`/`"ip"` e consultas em que velocidade é mais importante que exatidão perfeita. | Implementa HNSW de alto desempenho, usa `sby_knn_workers`, costuma escalar melhor que busca exata e suporta `"euclidean"`, `"cosine"` e `"ip"`. | A busca é aproximada; exige calibrar `sby_knn_hnsw_m` e `sby_knn_hnsw_ef`; consome memória para o grafo; resultados podem diferir de uma busca exata quando `ef` é baixo. |
#'
#' ## Algoritmos disponíveis
#'
#' | Algoritmo | Engine compatível | Tipo | Quando usar | Evite quando |
#' |---|---|---|---|---|
#' | `"auto"` | Todos | Depende do engine | Melhor ponto de partida; deixa o pacote escolher `"kd_tree"`/`"brute"` em `FNN`, `"Kmknn"`/`"Exhaustive"` em `BiocNeighbors`, ou o HNSW interno em `RcppHNSW`. | Você precisa reproduzir exatamente a mesma estrutura de busca entre ambientes ou documentar uma escolha metodológica específica. |
#' | `"kd_tree"` | `FNN` | Exato | Dados euclidianos com poucas ou médias dimensões; tende a ser eficiente quando as partições espaciais ainda discriminam bem os vizinhos. | Alta dimensionalidade, muitas variáveis ruidosas ou métrica não euclidiana. |
#' | `"cover_tree"` | `FNN` | Exato | Alternativa exata para dados euclidianos quando a estrutura intrínseca pode favorecer árvore de cobertura. | Quando testes rápidos mostram desempenho inferior a `"kd_tree"`/`"brute"`; não serve para cosseno ou produto interno. |
#' | `"brute"` | `FNN` | Exato | Alta dimensionalidade moderada, bases pequenas/médias, auditorias ou cenários em que simplicidade e previsibilidade importam mais que indexação. | Bases muito grandes, pois compara muitos pares e pode ficar lento. |
#' | `"Kmknn"` | `BiocNeighbors` | Exato | Padrão exato de `BiocNeighbors` para menor dimensionalidade; bom equilíbrio entre velocidade e exatidão com `"euclidean"` ou `"cosine"`. | Bases muito grandes ou de alta dimensionalidade em que busca aproximada seria aceitável. |
#' | `"Vptree"` | `BiocNeighbors` | Exato | Busca exata baseada em árvore por pontos de vantagem; útil para comparar alternativas exatas, especialmente com métricas suportadas pelo engine. | Quando a dimensionalidade reduz a eficácia da árvore ou quando `"Exhaustive"` é mais previsível. |
#' | `"Exhaustive"` | `BiocNeighbors` | Exato | Referência robusta para alta dimensionalidade, validação de resultados aproximados e conjuntos em que o custo total ainda é aceitável. | Bases grandes com muitas consultas, pois pode ser computacionalmente caro. |
#' | `"Annoy"` | `BiocNeighbors` | Aproximado | Bases grandes em que uma aproximação rápida é aceitável, especialmente para exploração, pré-processamento e redução de custo com `"euclidean"` ou `"cosine"`. | Auditorias que exigem vizinhos exatos; uso de `"ip"`; casos sensíveis a pequenas mudanças na vizinhança. |
#' | `"Hnsw"` | `BiocNeighbors` | Aproximado | Quando você quer HNSW dentro do ecossistema `BiocNeighbors`/`BiocParallel`, com `"euclidean"` ou `"cosine"`. | Quando precisa de `"ip"` ou dos controles `sby_knn_hnsw_m`/`sby_knn_hnsw_ef` da rota `RcppHNSW`; nesse caso use `sby_knn_engine = "RcppHNSW"`. |
#'
#' ## Métricas de distância
#'
#' | Métrica | Interpretação | Compatibilidade | Recomendação |
#' |---|---|---|---|
#' | `"euclidean"` | Distância geométrica usual após Z-score; pontos são próximos quando têm valores padronizados parecidos em magnitude e direção. | Todos os engines; exata em `FNN`/algoritmos exatos de `BiocNeighbors` e aproximada em Annoy/HNSW/RcppHNSW. | Use como padrão para variáveis tabulares numéricas, especialmente quando diferenças de magnitude padronizada são relevantes. |
#' | `"cosine"` | Distância angular; compara a orientação dos vetores e reduz a influência da norma após normalização L2. | `BiocNeighbors` e `RcppHNSW`; não é aceita por `FNN`. | Use quando o padrão relativo entre variáveis importa mais que o tamanho absoluto, como perfis, composições e vetores esparsos/direcionais. |
#' | `"ip"` | Produto interno convertido em distância; após normalização L2, fica próximo de uma comparação por similaridade angular. | Somente `RcppHNSW` neste pacote. | Use quando o modelo conceitual é similaridade por produto interno ou quando você precisa alinhar a busca a embeddings/vetores normalizados; requer busca aproximada. |
#'
#' ## Parâmetros exclusivos da rota HNSW
#'
#' Os argumentos `sby_knn_hnsw_m` e `sby_knn_hnsw_ef` só afetam a rota
#' `sby_knn_engine = "RcppHNSW"`; eles não configuram o `"Hnsw"` de
#' `BiocNeighbors`. `sby_knn_hnsw_m`
#' representa a conectividade máxima do grafo: valores maiores criam mais arestas,
#' melhoram o recall e tornam a busca mais robusta, mas aumentam memória e tempo
#' de construção. O padrão `16L` é conservador; valores como 24 ou 32 podem ser
#' úteis em bases grandes, ruidosas ou de alta dimensionalidade. `sby_knn_hnsw_ef`
#' representa quantos candidatos são explorados dinamicamente na busca: deve ser
#' pelo menos tão grande quanto o número de vizinhos solicitado e, internamente, é
#' limitado ao número de linhas da base de referência. O padrão `200L` favorece
#' qualidade; reduza para acelerar quando pequenas perdas de recall forem
#' aceitáveis, ou aumente quando NearMiss/ADASYN ficarem sensíveis a vizinhos
#' aproximados subótimos.
#'
#' ## Recomendações práticas
#'
#' - Comece com `sby_knn_engine = "auto"`, `sby_knn_algorithm = "auto"` e
#'   `sby_knn_distance_metric = "euclidean"`.
#' - Para auditoria, bases pequenas ou necessidade de vizinhos exatos, prefira
#'   `FNN`/`"brute"` ou algoritmos exatos de `BiocNeighbors`.
#' - Para paralelismo e métrica cosseno, use `sby_knn_engine = "BiocNeighbors"`.
#' - Para bases grandes, embeddings, `"ip"` ou alta dimensionalidade, use
#'   `sby_knn_engine = "RcppHNSW"` e ajuste `sby_knn_hnsw_m`/`sby_knn_hnsw_ef`.
#' - Em ADASYN, vizinhos aproximados podem mudar quais regiões recebem amostras
#'   sintéticas; em NearMiss, podem mudar quais exemplos majoritários são retidos.
#'   Aumente `sby_knn_hnsw_ef` quando essa estabilidade for importante.
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
  sby_formula,
  sby_data,
  sby_under_ratio = 0.5,
  sby_knn_under_k = 5L,
  sby_seed = sample.int(10L^5L, 1L),
  sby_audit = FALSE,
  sby_precomputed_scaling = NULL,
  sby_input_already_scaled = FALSE,
  sby_restore_types = TRUE,
  sby_type_info = NULL,
  sby_fixed_minority_label = NULL,
  sby_fixed_majority_label = NULL,
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
  sby_knn_distance_metric <- match.arg(
    arg = sby_knn_distance_metric
  )

  # Valida a quantidade de workers para calculo KNN
  sby_knn_workers <- sby_validate_knn_workers(
    sby_knn_workers = sby_knn_workers
  )

  # Valida parametros HNSW usados por engine aproximado
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

  # Verifica se o numero de vizinhos de subamostragem e inteiro positivo
  sby_knn_under_k <- sby_validate_positive_integer_scalar(
    sby_value = sby_knn_under_k,
    sby_name  = "sby_knn_under_k"
  )

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
    sby_class_roles <- sby_get_binary_class_roles(
      sby_target_factor  = sby_target_factor,
      sby_minority_label = sby_fixed_minority_label,
      sby_majority_label = sby_fixed_majority_label
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
      sby_target_factor  = sby_target_factor,
      sby_under_ratio    = sby_under_ratio,
      sby_minority_label = sby_class_roles$sby_minority_label,
      sby_majority_label = sby_class_roles$sby_majority_label
    )
    sby_effective_k <- min(
      as.integer(sby_knn_under_k),
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
      sby_knn_distance_metric         = sby_knn_distance_metric,
      sby_knn_workers             = sby_knn_workers,
      sby_knn_hnsw_m                  = sby_knn_hnsw_m,
      sby_knn_hnsw_ef                 = sby_knn_hnsw_ef
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
    sby_input_rows                  = nrow(sby_x_matrix),
    sby_output_rows                 = nrow(sby_balanced_data),
    sby_removed_rows                = nrow(sby_x_matrix) - nrow(sby_balanced_data),
    sby_knn_engine                 = sby_knn_engine,
    sby_knn_distance_metric             = sby_knn_distance_metric,
    sby_knn_workers                 = sby_knn_workers,
    sby_knn_hnsw_m                      = sby_diagnostic_hnsw_m,
    sby_knn_hnsw_ef                     = sby_diagnostic_hnsw_ef,
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
