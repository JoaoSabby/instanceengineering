\name{sby_adasyn}
\alias{sby_adasyn}
\title{Aplicar sobreamostragem ADASYN em dados binários}
\description{
\code{sby_adasyn()} executa sobreamostragem adaptativa da classe minoritária em um
problema binário, gerando observações sintéticas em regiões nas quais a classe
minoritária está localmente mais cercada por exemplos majoritários. A função
retorna um tibble balanceado ou, quando solicitado, uma estrutura de auditoria
com dados intermediários, escala, classes e diagnósticos.
}
\usage{
sby_adasyn(
  sby_formula,
  sby_data,
  sby_over_ratio = 0.2,
  sby_knn_over_k = 5L,
  sby_seed = sample.int(10L^5L, 1L),
  sby_audit = FALSE,
  sby_return_scaled = FALSE,
  sby_restore_types = TRUE,
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
\item{sby_over_ratio}{Valor numérico escalar positivo que controla a expansão relativa da classe minoritária. O padrão é \code{0.2}; em bases pequenas, qualquer valor positivo gera ao menos uma linha sintética para evitar abortos por arredondamento. Valores maiores aumentam a expansão da minoria, mas também elevam o risco de criar amostras sintéticas em regiões ruidosas.}
\item{sby_knn_over_k}{Número inteiro positivo de vizinhos usados para estimar a dificuldade local de cada observação minoritária no critério ADASYN. O padrão é \code{5L}. Valores maiores tornam a estimativa de dificuldade mais estável e global; valores menores enfatizam estruturas locais e podem reagir fortemente a outliers.}
\item{sby_seed}{Valor numérico inteiro utilizado para inicializar o gerador de números pseudoaleatórios. O padrão é \code{sample.int(10L^5L, 1L)}, gerando uma semente inteira aleatória quando o usuário não informa valor. Informe uma semente fixa para reproduzir exatamente os vizinhos empatados, as escolhas de interpolação e as matrizes sintéticas geradas.}
\item{sby_audit}{Indicador lógico escalar que controla o formato do retorno. O padrão é \code{FALSE}, retornando apenas o conjunto balanceado final. Quando \code{TRUE}, a função retorna uma lista com diagnósticos, parâmetros resolvidos e artefatos intermediários úteis para validação metodológica e depuração.}
\item{sby_return_scaled}{Indicador lógico escalar que define se a matriz balanceada ainda padronizada deve ser incluída no retorno de auditoria. O padrão é \code{FALSE}, evitando armazenamento adicional. Quando \code{TRUE}, permite encadear a saída com rotinas que reutilizam a mesma escala, como o pipeline combinado \code{sby_adanear()}.}
\item{sby_restore_types}{Indicador lógico escalar que define se tipos numéricos originais devem ser restaurados no tibble final. O padrão é \code{TRUE}. Essa restauração melhora compatibilidade com dados que entraram como inteiros ou classes numéricas específicas; quando \code{FALSE}, a saída tende a permanecer em representação numérica de ponto flutuante.}
\item{sby_knn_algorithm}{String escalar escolhida entre \code{"auto"}, \code{"kd_tree"}, \code{"cover_tree"}, \code{"brute"}, \code{"Kmknn"}, \code{"Vptree"}, \code{"Exhaustive"}, \code{"Annoy"} e \code{"Hnsw"}. O padrão é resolvido como \code{"auto"}. A seleção determina como os vizinhos minoritários e mistos serão consultados, afetando tempo de execução, exatidão e disponibilidade de métricas.}
\item{sby_knn_engine}{String escalar escolhida entre \code{"auto"}, \code{"FNN"}, \code{"BiocNeighbors"} e \code{"RcppHNSW"}. O padrão é resolvido como \code{"auto"}. Esse engine define a implementação concreta da busca espacial; a escolha pode favorecer exatidão determinística, integração Bioconductor ou busca aproximada de alta escala.}
\item{sby_knn_distance_metric}{String escalar escolhida entre \code{"euclidean"}, \code{"ip"} e \code{"cosine"}. O padrão é \code{"euclidean"}. A métrica altera a noção de proximidade usada para estimar dificuldade local e, portanto, pode mudar quais regiões recebem mais observações sintéticas.}
\item{sby_knn_workers}{Número inteiro positivo de workers disponibilizados para engines paralelizáveis. O padrão é \code{1L}. Aumentar o valor pode acelerar consultas KNN em matrizes grandes, mas eleva uso de CPU e pode modificar a escolha automática de engine.}
\item{sby_knn_hnsw_m}{Número inteiro positivo que controla a conectividade estrutural do grafo HNSW quando o engine efetivo é \code{"RcppHNSW"}. O padrão é \code{16L}. Valores maiores geralmente melhoram a recuperação de vizinhos aproximados e aumentam memória; valores menores reduzem custo com possível queda de qualidade.}
\item{sby_knn_hnsw_ef}{Número inteiro positivo que controla a largura dinâmica da busca HNSW. O padrão é \code{200L}. Elevar esse valor tende a aproximar o resultado da busca exata e torna a sobreamostragem mais estável, ao custo de consultas mais lentas. }
}
\value{Tibble balanceado quando \code{sby_audit = FALSE}; lista de auditoria com dados balanceados, diagnósticos e artefatos intermediários quando \code{sby_audit = TRUE}.}
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
