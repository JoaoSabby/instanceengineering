\name{sby_nearmiss}
\alias{sby_nearmiss}
\title{Aplicar subamostragem NearMiss-1 em dados binários}
\description{
\code{sby_nearmiss()} executa subamostragem da classe majoritária pelo critério
NearMiss-1, retendo observações majoritárias que apresentam menor distância
média aos vizinhos da classe minoritária. A função é indicada para reduzir
predominância majoritária preservando exemplos próximos à fronteira de decisão.
}
\usage{
sby_nearmiss(
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
  sby_knn_algorithm = c(
    "auto", "kd_tree", "cover_tree", "brute", "Kmknn", "Vptree", "Exhaustive", "Annoy", "Hnsw"
  ),
  sby_knn_engine = c(
    "auto", "FNN", "BiocNeighbors", "RcppHNSW"
  ),
  sby_knn_distance_metric = c(
    "euclidean", "ip", "cosine"
  ),
  sby_knn_workers = 1L,
  sby_knn_hnsw_m = 16L,
  sby_knn_hnsw_ef = 200L
)

}
\arguments{
\item{sby_formula}{Fórmula no formato \code{alvo ~ preditores} usada para identificar uma única coluna de desfecho binário e as colunas preditoras numéricas em \code{sby_data}. O lado direito deve referenciar apenas colunas já existentes; transformações, interações e offsets precisam ser materializados antes da chamada. Use \code{alvo ~ .} para selecionar todos os demais campos como preditores.}
\item{sby_data}{Data frame, tibble ou matriz com a coluna de desfecho e as variáveis preditoras numéricas referenciadas em \code{sby_formula}.}
\item{sby_under_ratio}{Valor numérico escalar no intervalo \code{(0, 1]} que representa a razão mínima desejada entre minoria e maioria após a subamostragem. O padrão \code{0.5} permite reter até duas observações majoritárias para cada minoritária; \code{1} reduz a maioria até igualar a minoria.}
\item{sby_knn_under_k}{Número inteiro positivo de vizinhos minoritários usados para calcular a distância média do critério NearMiss-1. O padrão é \code{5L}. Valores maiores reduzem variância do ranqueamento; valores menores focalizam a fronteira local e podem selecionar exemplos muito próximos de ruído minoritário.}
\item{sby_seed}{Valor numérico inteiro utilizado para inicializar o gerador de números pseudoaleatórios. O padrão é \code{sample.int(10L^5L, 1L)}, gerando uma semente inteira aleatória quando o usuário não informa valor. Informe uma semente fixa para tornar reprodutíveis desempates, amostragens complementares e a ordem final das observações preservadas.}
\item{sby_audit}{Indicador lógico escalar que controla o retorno de metadados de auditoria. O padrão é \code{FALSE}, retornando apenas o tibble final. Quando \code{TRUE}, a função retorna lista com índices retidos, distribuições de classe, parâmetros resolvidos e informações de escala.}
\item{sby_precomputed_scaling}{Lista opcional com parâmetros de centralização e escala previamente calculados. O padrão é \code{NULL}, fazendo com que a função estime a padronização a partir de \code{sby_data}. Fornecer essa lista permite reutilizar uma escala externa ou herdada de etapa anterior, evitando inconsistência geométrica em pipelines encadeados.}
\item{sby_input_already_scaled}{Indicador lógico escalar que informa se \code{sby_data} já está em escala Z-score compatível com \code{sby_precomputed_scaling}. O padrão é \code{FALSE}. Quando \code{TRUE}, a função evita reaplicar padronização e interpreta as coordenadas de entrada como prontas para busca KNN.}
\item{sby_restore_types}{Indicador lógico escalar que define se tipos numéricos originais devem ser restaurados no retorno final. O padrão é \code{TRUE}. Essa escolha preserva compatibilidade com o esquema de dados de entrada; desativá-la reduz pós-processamento e mantém valores no formato numérico resultante das operações matriciais.}
\item{sby_type_info}{Lista opcional com metadados dos tipos numéricos originais dos preditores. O padrão é \code{NULL}, fazendo a função inferir os tipos quando necessário. Fornecer esse objeto é útil em pipelines encadeados, pois garante que a restauração de tipos use exatamente o mesmo diagnóstico da etapa anterior.}
\item{sby_fixed_minority_label}{Rótulo interno opcional usado por \code{sby_adanear()} para preservar a classe minoritária original após o ADASYN. O padrão \code{NULL} mantém o comportamento autônomo de inferir os papéis pelas contagens atuais.}
\item{sby_fixed_majority_label}{Rótulo interno opcional usado por \code{sby_adanear()} para preservar a classe majoritária original após o ADASYN. O padrão \code{NULL} mantém o comportamento autônomo de inferir os papéis pelas contagens atuais.}
\item{sby_knn_algorithm}{String escalar que escolhe a estratégia de busca KNN: \code{"auto"}, \code{"kd_tree"}, \code{"cover_tree"}, \code{"brute"}, \code{"Kmknn"}, \code{"Vptree"}, \code{"Exhaustive"}, \code{"Annoy"} ou \code{"Hnsw"}. Use \code{"auto"} para deixar o pacote escolher uma opção compatível com o engine e a dimensionalidade; informe uma alternativa explícita quando quiser controlar o compromisso entre exatidão, velocidade de execução, consumo de memória e suporte a métricas. Consulte os detalhes para recomendações por algoritmo.}
\item{sby_knn_engine}{String escalar que escolhe a biblioteca usada para executar a busca KNN: \code{"auto"}, \code{"FNN"}, \code{"BiocNeighbors"} ou \code{"RcppHNSW"}. Na maioria dos casos, mantenha \code{"auto"}; informe explicitamente apenas quando precisar de uma implementação específica, paralelismo via \code{BiocNeighbors}, métricas não euclidianas ou busca aproximada HNSW por \code{RcppHNSW}. Consulte os detalhes para saber quando o engine precisa ser declarado.}
\item{sby_knn_distance_metric}{String escalar que define a geometria da vizinhança: \code{"euclidean"}, \code{"cosine"} ou \code{"ip"}. A escolha muda o significado de proximidade e também restringe engines e algoritmos disponíveis; \code{"euclidean"} é a opção mais geral, \code{"cosine"} privilegia direção angular e \code{"ip"} usa produto interno via \code{RcppHNSW}. Consulte os detalhes para recomendações.}
\item{sby_knn_workers}{Número inteiro positivo de workers usados por engines que aceitam paralelização. O padrão é \code{1L}. Mais workers podem reduzir latência em bases grandes, mas aumentam consumo de CPU e podem exigir engines compatíveis com execução paralela.}
\item{sby_knn_hnsw_m}{Número inteiro positivo usado apenas quando o engine efetivo é \code{"RcppHNSW"}. Controla a conectividade máxima do grafo (\code{M}): valores maiores aumentam a chance de recuperar vizinhos melhores e tornam o índice mais robusto, mas consomem mais memória e tempo de construção. O padrão \code{16L} costuma ser um bom equilíbrio; aumente em bases grandes, ruidosas ou de alta dimensionalidade quando recall for mais importante que memória.}
\item{sby_knn_hnsw_ef}{Número inteiro positivo usado apenas quando o engine efetivo é \code{"RcppHNSW"}. Controla a largura da lista dinâmica de candidatos (\code{ef}) durante construção/consulta: valores maiores aproximam a busca do resultado exato e estabilizam ADASYN/NearMiss, mas deixam as consultas mais lentas. O padrão \code{200L} prioriza qualidade; reduza para velocidade ou aumente quando a vizinhança aproximada precisar de mais fidelidade.}
}
\value{Tibble balanceado quando \code{sby_audit = FALSE}; lista de auditoria com dados balanceados, índices, diagnósticos e escala quando \code{sby_audit = TRUE}.}
\details{
A função utiliza uma arquitetura KNN configurável sobre preditores numéricos padronizados por Z-score. Três escolhas trabalham em conjunto: \code{sby_knn_algorithm} define a estrutura/estratégia de busca, \code{sby_knn_engine} define a implementação computacional e \code{sby_knn_distance_metric} define a noção de proximidade.

\subsection{Preciso informar \code{sby_knn_engine}?}{
Em geral, não: para o uso cotidiano, mantenha \code{sby_knn_engine = "auto"} e \code{sby_knn_algorithm = "auto"}. Nesse modo, o pacote usa \code{FNN} em execução sequencial e \code{BiocNeighbors} quando \code{sby_knn_workers > 1L}; o algoritmo também é escolhido automaticamente pela dimensionalidade dos preditores. Informe o engine explicitamente quando quiser garantir uma biblioteca específica, usar paralelismo de \code{BiocNeighbors}, usar \code{RcppHNSW}, ou quando escolher um algoritmo que pertence a uma família específica. Na API atual, informar apenas \code{sby_knn_algorithm = "Hnsw"}, \code{"Annoy"}, \code{"Kmknn"}, \code{"Vptree"} ou \code{"Exhaustive"} não substitui a escolha do engine: declare \code{sby_knn_engine = "BiocNeighbors"} para esses algoritmos, ou \code{sby_knn_engine = "RcppHNSW"} quando quiser a implementação HNSW do pacote \code{RcppHNSW}.
}

\subsection{Engines disponíveis}{
\describe{
\item{\code{"auto"}}{Melhor para uso recomendado quando você não precisa controlar a biblioteca. Reduz configuração manual, escolhe um caminho compatível com workers e mantém uma boa opção padrão. Limitação: não expressa intenção metodológica específica; se você precisa de HNSW, Annoy, cosseno, produto interno ou paralelismo controlado, prefira declarar engine/algoritmo.}
\item{\code{"FNN"}}{Melhor para bases pequenas a médias, distância euclidiana, execução sequencial e busca exata. É simples, rápido para baixa/média dimensionalidade, determinístico e sem aproximação. Limitações: aceita apenas \code{"euclidean"} neste pacote, não usa \code{sby_knn_workers} e só combina com \code{"auto"}, \code{"kd_tree"}, \code{"cover_tree"} ou \code{"brute"}.}
\item{\code{"BiocNeighbors"}}{Melhor para busca exata ou aproximada com suporte a paralelismo e métrica \code{"cosine"}. Oferece vários algoritmos (\code{"Kmknn"}, \code{"Vptree"}, \code{"Exhaustive"}, \code{"Annoy"}, \code{"Hnsw"}), integra \code{BiocParallel} e permite escolher entre exatidão e aproximação. Limitações: requer \code{BiocNeighbors} e \code{BiocParallel}, não suporta \code{"ip"} neste pacote e algoritmos aproximados podem variar em recall.}
\item{\code{"RcppHNSW"}}{Melhor para bases grandes, alta dimensionalidade, métricas \code{"cosine"}/\code{"ip"} e consultas em que velocidade é mais importante que exatidão perfeita. Implementa HNSW de alto desempenho, usa \code{sby_knn_workers}, costuma escalar melhor que busca exata e suporta \code{"euclidean"}, \code{"cosine"} e \code{"ip"}. Limitações: a busca é aproximada, exige calibrar \code{sby_knn_hnsw_m} e \code{sby_knn_hnsw_ef}, consome memória para o grafo e pode diferir de uma busca exata quando \code{ef} é baixo.}
}
}

\subsection{Algoritmos disponíveis}{
\describe{
\item{\code{"auto"}}{Melhor ponto de partida; deixa o pacote escolher \code{"kd_tree"}/\code{"brute"} em \code{FNN}, \code{"Kmknn"}/\code{"Exhaustive"} em \code{BiocNeighbors}, ou o HNSW interno em \code{RcppHNSW}. Evite quando você precisa reproduzir exatamente a mesma estrutura de busca entre ambientes ou documentar uma escolha metodológica específica.}
\item{\code{"kd_tree"}}{Algoritmo exato de \code{FNN}. Use em dados euclidianos com poucas ou médias dimensões, quando partições espaciais ainda discriminam bem os vizinhos. Evite em alta dimensionalidade, muitas variáveis ruidosas ou métrica não euclidiana.}
\item{\code{"cover_tree"}}{Algoritmo exato de \code{FNN}. Use como alternativa exata para dados euclidianos quando a estrutura intrínseca pode favorecer árvore de cobertura. Evite quando testes rápidos mostram desempenho inferior a \code{"kd_tree"}/\code{"brute"}; não serve para cosseno ou produto interno.}
\item{\code{"brute"}}{Algoritmo exato de \code{FNN}. Use em alta dimensionalidade moderada, bases pequenas/médias, auditorias ou cenários em que simplicidade e previsibilidade importam mais que indexação. Evite bases muito grandes, pois compara muitos pares e pode ficar lento.}
\item{\code{"Kmknn"}}{Algoritmo exato de \code{BiocNeighbors}. Use como padrão exato de menor dimensionalidade, com bom equilíbrio entre velocidade e exatidão para \code{"euclidean"} ou \code{"cosine"}. Evite bases muito grandes ou de alta dimensionalidade em que busca aproximada seria aceitável.}
\item{\code{"Vptree"}}{Algoritmo exato de \code{BiocNeighbors}. Use para comparar alternativas exatas, especialmente com métricas suportadas pelo engine. Evite quando a dimensionalidade reduz a eficácia da árvore ou quando \code{"Exhaustive"} é mais previsível.}
\item{\code{"Exhaustive"}}{Algoritmo exato de \code{BiocNeighbors}. Use como referência robusta para alta dimensionalidade, validação de resultados aproximados e conjuntos em que o custo total ainda é aceitável. Evite bases grandes com muitas consultas, pois pode ser caro.}
\item{\code{"Annoy"}}{Algoritmo aproximado de \code{BiocNeighbors}. Use em bases grandes quando aproximação rápida é aceitável, especialmente para exploração, pré-processamento e redução de custo com \code{"euclidean"} ou \code{"cosine"}. Evite auditorias que exigem vizinhos exatos, uso de \code{"ip"} e casos sensíveis a pequenas mudanças na vizinhança.}
\item{\code{"Hnsw"}}{Algoritmo aproximado de \code{BiocNeighbors}. Use quando você quer HNSW dentro do ecossistema \code{BiocNeighbors}/\code{BiocParallel}, com \code{"euclidean"} ou \code{"cosine"}. Evite quando precisa de \code{"ip"} ou dos controles \code{sby_knn_hnsw_m}/\code{sby_knn_hnsw_ef} da rota \code{RcppHNSW}; nesse caso use \code{sby_knn_engine = "RcppHNSW"}.}
}
}


\subsection{Desempenho, memória e matrizes esparsas}{
A velocidade deve ser interpretada como um atributo de qualidade da configuração KNN, junto com fidelidade da vizinhança, consumo de memória e compatibilidade de métrica. Em termos práticos:
\describe{
\item{Árvores exatas (\code{"kd_tree"}, \code{"cover_tree"}, \code{"Kmknn"}, \code{"Vptree"})}{Podem ser rápidas em baixa/média dimensionalidade e mantêm estruturas auxiliares moderadas. A indexação evita comparar todos os pares, mas perde eficiência quando muitas dimensões tornam as partições pouco seletivas.}
\item{Busca exaustiva (\code{"brute"}, \code{"Exhaustive"})}{É mais lenta em bases grandes, porém previsível e geralmente usa menos memória de índice. Calcula distâncias diretamente; não paga custo de construção de índice, mas o tempo cresce com número de linhas, consultas e colunas.}
\item{Aproximados (\code{"Annoy"}, \code{"Hnsw"}, \code{RcppHNSW})}{Normalmente são mais rápidos em bases grandes ou de alta dimensionalidade, mas usam memória adicional para índices; HNSW cresce com \code{sby_knn_hnsw_m}. Essas opções trocam exatidão por velocidade: consultam apenas parte do espaço de busca e recuperam vizinhos prováveis. Aumentar \code{sby_knn_hnsw_ef} melhora recall, mas aumenta latência.}
\item{Paralelismo (\code{sby_knn_workers > 1L})}{Pode reduzir tempo de consulta em matrizes grandes, com maior uso simultâneo de CPU e possível pressão adicional de memória. O trabalho é dividido entre workers quando o engine suporta paralelização, sobretudo em \code{BiocNeighbors} e \code{RcppHNSW}.}
}
As rotinas atuais operam sobre \code{matrix} numérica densa após seleção de preditores e padronização. Matrizes esparsas do pacote \code{Matrix} são rejeitadas antes de densificação implícita para evitar estouro de memória. Portanto, em dados muito esparsos, materialize conscientemente uma matriz densa somente se houver memória suficiente, reduza dimensionalidade/seleção de variáveis antes do balanceamento, ou use outro pré-processamento que produza preditores densos. Em matrizes densas muito largas, prefira testar uma busca aproximada ou uma busca exaustiva paralelizável, pois árvores exatas tendem a perder vantagem.
}

\subsection{Métricas de distância}{
\describe{
\item{\code{"euclidean"}}{Distância geométrica usual após Z-score; pontos são próximos quando têm valores padronizados parecidos em magnitude e direção. Compatível com todos os engines; exata em \code{FNN}/algoritmos exatos de \code{BiocNeighbors} e aproximada em Annoy/HNSW/RcppHNSW. Use como padrão para variáveis tabulares numéricas, especialmente quando diferenças de magnitude padronizada são relevantes.}
\item{\code{"cosine"}}{Distância angular; compara a orientação dos vetores e reduz a influência da norma após normalização L2. Compatível com \code{BiocNeighbors} e \code{RcppHNSW}; não é aceita por \code{FNN}. Use quando o padrão relativo entre variáveis importa mais que o tamanho absoluto, como composições, assinaturas de perfil e vetores de alta dimensionalidade já materializados como matriz densa.}
\item{\code{"ip"}}{Produto interno convertido em distância; após normalização L2, fica próximo de uma comparação por similaridade angular. Compatível somente com \code{RcppHNSW} neste pacote. Use quando o modelo conceitual é similaridade por produto interno ou quando você precisa alinhar a busca a embeddings/vetores normalizados; requer busca aproximada.}
}
}

\subsection{Parâmetros exclusivos da rota HNSW}{
Os argumentos \code{sby_knn_hnsw_m} e \code{sby_knn_hnsw_ef} só afetam a rota \code{sby_knn_engine = "RcppHNSW"}; eles não configuram o \code{"Hnsw"} de \code{BiocNeighbors}. \code{sby_knn_hnsw_m} representa a conectividade máxima do grafo: valores maiores criam mais arestas, melhoram o recall e tornam a busca mais robusta, mas aumentam memória e tempo de construção. O padrão \code{16L} é conservador; valores como 24 ou 32 podem ser úteis em bases grandes, ruidosas ou de alta dimensionalidade. \code{sby_knn_hnsw_ef} representa quantos candidatos são explorados dinamicamente na busca: deve ser pelo menos tão grande quanto o número de vizinhos solicitado e, internamente, é limitado ao número de linhas da base de referência. O padrão \code{200L} favorece qualidade; reduza para acelerar quando pequenas perdas de recall forem aceitáveis, ou aumente quando NearMiss/ADASYN ficarem sensíveis a vizinhos aproximados subótimos.
}

\subsection{Recomendações práticas}{
\itemize{
\item Comece com \code{sby_knn_engine = "auto"}, \code{sby_knn_algorithm = "auto"} e \code{sby_knn_distance_metric = "euclidean"}.
\item Para auditoria, bases pequenas ou necessidade de vizinhos exatos, prefira \code{FNN}/\code{"brute"} ou algoritmos exatos de \code{BiocNeighbors}.
\item Para paralelismo e métrica cosseno, use \code{sby_knn_engine = "BiocNeighbors"}.
\item Para bases grandes, embeddings, \code{"ip"} ou alta dimensionalidade, use \code{sby_knn_engine = "RcppHNSW"} e ajuste \code{sby_knn_hnsw_m}/\code{sby_knn_hnsw_ef}.
\item Em ADASYN, vizinhos aproximados podem mudar quais regiões recebem amostras sintéticas; em NearMiss, podem mudar quais exemplos majoritários são retidos. Aumente \code{sby_knn_hnsw_ef} quando essa estabilidade for importante.
}
}
}
\references{
He, H., Bai, Y., Garcia, E. A., \& Li, S. (2008). ADASYN: Adaptive synthetic
sampling approach for imbalanced learning. In *2008 IEEE International Joint
Conference on Neural Networks* (pp. 1322-1328). IEEE.

Malkov, Y. A., \& Yashunin, D. A. (2018). Efficient and robust approximate
nearest neighbor search using Hierarchical Navigable Small World graphs.
*IEEE Transactions on Pattern Analysis and Machine Intelligence*, 42(4),
824-836.

}
