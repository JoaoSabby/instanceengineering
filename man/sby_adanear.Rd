\name{sby_adanear}
\alias{sby_adanear}
\title{Executar balanceamento combinado ADASYN e NearMiss-1}
\description{
\code{sby_adanear()} executa um pipeline híbrido de balanceamento para problemas
binários: primeiro aplica ADASYN para expandir adaptativamente a classe
minoritária e, em seguida, aplica NearMiss-1 para reduzir a classe majoritária.
O objetivo é combinar geração sintética em regiões difíceis com retenção
seletiva de exemplos majoritários próximos à fronteira de decisão.
}
\usage{
sby_adanear(
  sby_formula,
  sby_data,
  sby_over_ratio = 0.2,
  sby_under_ratio = 0.5,
  sby_knn_over_k = 5L,
  sby_knn_under_k = 5L,
  sby_seed = sample.int(10L^5L, 1L),
  sby_audit = FALSE,
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
\item{sby_formula}{Fórmula no formato \code{alvo ~ preditores} usada para identificar uma única coluna de desfecho binário e as colunas preditoras numéricas em \code{sby_data}. Use \code{alvo ~ .} para selecionar todos os demais campos como preditores.}
\item{sby_data}{Data frame, tibble ou matriz com a coluna de desfecho e as variáveis preditoras numéricas referenciadas em \code{sby_formula}.}
\item{sby_over_ratio}{Valor numérico escalar que controla a intensidade da sobreamostragem ADASYN antes da etapa NearMiss. O padrão é \code{0.2}. Valores maiores inserem mais exemplos sintéticos antes da subamostragem, podendo melhorar cobertura da minoria, mas também propagando ruído em regiões ambíguas.}
\item{sby_under_ratio}{Valor numérico escalar que controla a fração da classe majoritária retida depois da sobreamostragem. O padrão é \code{0.5}. Valores menores tornam o pipeline mais agressivo na redução majoritária; valores maiores preservam mais observações originais e tendem a manter maior diversidade majoritária.}
\item{sby_knn_over_k}{Número inteiro positivo de vizinhos usados pela etapa ADASYN para estimar dificuldade local e gerar amostras sintéticas. O padrão é \code{5L}. Essa escolha influencia onde a expansão minoritária será concentrada e o grau de suavização da avaliação local.}
\item{sby_knn_under_k}{Número inteiro positivo de vizinhos minoritários usados pela etapa NearMiss-1 para ranquear observações majoritárias. O padrão é \code{5L}. Essa escolha controla a sensibilidade da retenção majoritária à fronteira local criada após a sobreamostragem.}
\item{sby_seed}{Valor numérico inteiro utilizado para inicializar o gerador de números pseudoaleatórios nas duas etapas do pipeline. O padrão é \code{sample.int(10L^5L, 1L)}, gerando uma semente inteira aleatória quando o usuário não informa valor. Informe uma semente fixa para garantir reprodutibilidade conjunta das amostras sintéticas, desempates, índices retidos e diagnósticos finais.}
\item{sby_audit}{Indicador lógico escalar que controla se o retorno deve incluir resultados intermediários das etapas ADASYN e NearMiss. O padrão é \code{FALSE}, retornando somente os dados balanceados finais. Quando \code{TRUE}, o retorno passa a ser uma lista com auditoria completa, útil para inspeção de contagens e validação experimental.}
\item{sby_restore_types}{Indicador lógico escalar que define se tipos numéricos originais devem ser restaurados ao final do pipeline. O padrão é \code{TRUE}. Mantê-lo ativado facilita reintegração com bases tabulares originais; desativá-lo mantém a saída no formato numérico produzido pelas transformações internas.}
\item{sby_knn_algorithm}{String escalar escolhida entre \code{"auto"}, \code{"kd_tree"}, \code{"cover_tree"}, \code{"brute"}, \code{"Kmknn"}, \code{"Vptree"}, \code{"Exhaustive"}, \code{"Annoy"} e \code{"Hnsw"}. O padrão é resolvido como \code{"auto"}. O mesmo algoritmo resolvido é usado nas etapas de sobreamostragem e subamostragem, garantindo consistência geométrica ao longo do pipeline.}
\item{sby_knn_engine}{String escalar escolhida entre \code{"auto"}, \code{"FNN"}, \code{"BiocNeighbors"} e \code{"RcppHNSW"}. O padrão é resolvido como \code{"auto"}. O engine selecionado determina a implementação KNN compartilhada pelas duas etapas e afeta velocidade, memória, paralelismo e suporte a métricas.}
\item{sby_knn_distance_metric}{String escalar escolhida entre \code{"euclidean"}, \code{"ip"} e \code{"cosine"}. O padrão é \code{"euclidean"}. A métrica define simultaneamente a geometria da geração sintética e da retenção majoritária, podendo alterar tanto a localização dos pontos sintéticos quanto os exemplos majoritários preservados.}
\item{sby_knn_workers}{Número inteiro positivo de workers usados nas consultas KNN quando o engine efetivo oferece suporte. O padrão é \code{1L}. Aumentar esse número pode acelerar o pipeline completo, mas eleva uso de recursos e deve ser compatível com o backend escolhido.}
\item{sby_knn_hnsw_m}{Número inteiro positivo que controla a conectividade do grafo HNSW no engine \code{RcppHNSW}. O padrão é \code{16L}. Valores maiores tendem a melhorar recall nas duas etapas do pipeline, com maior consumo de memória e tempo de construção do índice.}
\item{sby_knn_hnsw_ef}{Número inteiro positivo que controla a largura dinâmica de exploração do HNSW. O padrão é \code{200L}. Valores maiores tornam a busca aproximada mais precisa e estável para ADASYN e NearMiss, mas aumentam a latência das consultas. }
}
\value{Tibble balanceado quando \code{sby_audit = FALSE}; lista com resultados de sobreamostragem, subamostragem, dados finais e diagnósticos quando \code{sby_audit = TRUE}.}
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
