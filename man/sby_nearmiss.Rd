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
\item{sby_knn_algorithm}{String escalar escolhida entre \code{"auto"}, \code{"kd_tree"}, \code{"cover_tree"}, \code{"brute"}, \code{"Kmknn"}, \code{"Vptree"}, \code{"Exhaustive"}, \code{"Annoy"} e \code{"Hnsw"}. O padrão é \code{"auto"} após resolução por \code{match.arg()}. A escolha define o método de consulta dos vizinhos minoritários e altera desempenho, determinismo e compatibilidade com métricas.}
\item{sby_knn_engine}{String escalar escolhida entre \code{"auto"}, \code{"FNN"}, \code{"BiocNeighbors"} e \code{"RcppHNSW"}. O padrão é \code{"auto"} após resolução. O engine controla a biblioteca usada na busca KNN e, consequentemente, o suporte a busca exata, aproximada, paralelização e métricas não euclidianas.}
\item{sby_knn_distance_metric}{String escalar escolhida entre \code{"euclidean"}, \code{"ip"} e \code{"cosine"}. O padrão é \code{"euclidean"}. A métrica define a distância média usada para decidir quais observações majoritárias são mais próximas da classe minoritária e devem ser retidas.}
\item{sby_knn_workers}{Número inteiro positivo de workers usados por engines que aceitam paralelização. O padrão é \code{1L}. Mais workers podem reduzir latência em bases grandes, mas aumentam consumo de CPU e podem exigir engines compatíveis com execução paralela.}
\item{sby_knn_hnsw_m}{Número inteiro positivo que define a conectividade do grafo HNSW quando \code{RcppHNSW} é usado. O padrão é \code{16L}. Valores maiores podem melhorar a fidelidade da vizinhança aproximada para o ranqueamento NearMiss, com maior custo de memória.}
\item{sby_knn_hnsw_ef}{Número inteiro positivo que define a quantidade dinâmica de candidatos avaliados na busca HNSW. O padrão é \code{200L}. Aumentar esse valor tende a produzir vizinhos aproximados mais confiáveis e reduz risco de retenção baseada em vizinhos subótimos, mas torna a execução mais lenta. }
}
\value{Tibble balanceado quando \code{sby_audit = FALSE}; lista de auditoria com dados balanceados, índices, diagnósticos e escala quando \code{sby_audit = TRUE}.}
\details{
A função utiliza uma arquitetura de busca espacial configurável para calcular
vizinhos próximos sobre preditores numéricos previamente padronizados por
Z-score. A seleção de \code{sby_knn_engine}, \code{sby_knn_algorithm} e
\code{sby_knn_distance_metric} controla simultaneamente o provedor computacional, a
estratégia de indexação e a geometria usada para comparar observações. Quando
\code{sby_knn_engine = "auto"}, o pacote resolve automaticamente um engine
compatível com os demais argumentos e com o número de workers solicitado.

Combinações válidas entre engine, tipo de busca e métrica:

\tabular{lll}{
Engine \tab Tipo de busca \tab Métricas suportadas\cr
\code{FNN} \tab Exata \tab \code{euclidean}\cr
\code{BiocNeighbors} \tab Exata/Aproximada \tab \code{euclidean}, \code{cosine}\cr
\code{RcppHNSW} \tab Aproximada \tab \code{euclidean}, \code{ip}, \code{cosine}\cr
}

A distância euclidiana corresponde à geometria padrão em espaços contínuos e
é definida por \eqn{d(x, y) = \sqrt{\sum_i (x_i - y_i)^2}}. O produto interno
é expresso como distância por \eqn{d(x, y) = 1 - \sum_i x_i y_i}. Atenção:
para \code{sby_knn_distance_metric = "ip"}, o pacote realiza normalização L2 prévia
automática das matrizes envolvidas na busca para garantir consistência
espacial da métrica e evitar que diferenças de norma dominem a vizinhança. A
distância de cosseno é definida por \eqn{d(x, y) = 1 - \frac{\sum_i x_i y_i}{\sqrt{\sum_i x_i^2} \sqrt{\sum_i y_i^2}}}
e privilegia a orientação angular dos vetores em vez da magnitude absoluta.

Em \code{FNN}, os algoritmos \code{kd_tree}, \code{cover_tree}, \code{brute} e \code{auto} operam de
forma exata e aceitam somente \code{euclidean}. Em \code{BiocNeighbors}, \code{Kmknn},
\code{Vptree} e \code{Exhaustive} representam alternativas exatas, enquanto \code{Annoy} e
\code{Hnsw} representam alternativas aproximadas; nesse engine, \code{euclidean} e
\code{cosine} são aceitas. Em \code{RcppHNSW}, a busca é aproximada por grafo HNSW e os
parâmetros \code{sby_knn_hnsw_m} e \code{sby_knn_hnsw_ef} controlam, respectivamente, a
conectividade estrutural do grafo e a largura dinâmica da exploração.

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
