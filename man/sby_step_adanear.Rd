\name{sby_step_adanear}
\alias{sby_step_adanear}
\title{Adicionar uma etapa recipes de balanceamento NearMiss-1}
\description{
\code{sby_step_adanear()} adiciona a uma \code{recipe} uma etapa supervisionada de
subamostragem NearMiss-1 para problemas de classificação binária. A etapa é
ajustada durante \code{prep()} a partir de uma única coluna de desfecho selecionada
e, por padrão, é ignorada em novos dados durante \code{bake()} porque modifica o
número de linhas do conjunto processado.
}
\usage{
sby_step_adanear(
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
  sby_knn_algorithm = c(
    "auto", "kd_tree", "cover_tree", "brute", "Kmknn", "Vptree", "Exhaustive", "Annoy", "Hnsw"
  ),
  sby_knn_engine = c(
    "auto", "FNN", "BiocNeighbors", "RcppHNSW"
  ),
  sby_distance_metric = c(
    "euclidean", "ip", "cosine"
  ),
  sby_knn_workers = 1L,
  sby_hnsw_m = 16L,
  sby_hnsw_ef = 200L,
  sby_skip = TRUE,
  sby_id = recipes::rand_id("balance")
)

}
\arguments{
\item{sby_recipe}{Objeto \code{recipe} que receberá a etapa de balanceamento. Deve ser uma instância válida de \code{recipes::recipe()} contendo os preditores numéricos e a variável de desfecho selecionável por \code{...}. Não possui valor padrão, pois a etapa precisa ser anexada explicitamente a uma receita existente; a escolha determina em qual pipeline de pré-processamento a subamostragem será executada.}
\item{...}{Seletores \code{recipes} ou \code{tidyselect} usados para identificar exatamente uma coluna de desfecho binário. O formato esperado é uma expressão de seleção não avaliada, como um nome de coluna, \code{all_outcomes()} ou outro seletor compatível. Não há valor padrão; selecionar uma coluna incorreta altera diretamente quais classes serão tratadas como minoritária e majoritária no balanceamento.}
\item{sby_role}{Valor de papel (\code{role}) armazenado na etapa para compatibilidade com a infraestrutura de \code{recipes}. Espera-se \code{NA} ou uma string escalar. O padrão é \code{NA}, indicando que a etapa não introduz um novo papel operacional; alterar esse valor afeta apenas metadados da etapa, não o cálculo dos vizinhos.}
\item{sby_trained}{Indicador lógico escalar que informa se a etapa já passou por \code{prep()}. O padrão é \code{FALSE}, estado adequado para a criação inicial da etapa. Esse valor é controlado internamente por \code{recipes}; defini-lo manualmente como \code{TRUE} sem os metadados correspondentes pode tornar a etapa inconsistente.}
\item{sby_columns}{Vetor de caracteres ou \code{NULL} com o nome da coluna de desfecho resolvida durante \code{prep()}. O padrão é \code{NULL}, indicando que a seleção ainda não foi treinada. Esse metadado define qual variável será removida dos preditores e usada como alvo no momento do balanceamento.}
\item{sby_under_ratio}{Valor numérico escalar que representa a fração da classe majoritária a ser retida pela subamostragem NearMiss-1. O padrão é \code{0.5}, preservando aproximadamente metade das observações majoritárias elegíveis. Valores menores tornam o conjunto final mais agressivamente balanceado, enquanto valores maiores preservam mais informação da classe majoritária.}
\item{sby_k_under}{Número inteiro positivo de vizinhos minoritários considerados pelo critério NearMiss-1 para ranquear observações majoritárias. O padrão é \code{5L}. Valores maiores suavizam o critério de proximidade por considerar uma vizinhança mais ampla; valores menores tornam a seleção mais sensível a fronteiras locais e possíveis ruídos.}
\item{sby_seed}{Valor numérico inteiro utilizado para inicializar o gerador de números pseudoaleatórios. O padrão é \code{42L}. Uma semente fixa garante reprodutibilidade dos empates, amostragens internas e índices retidos durante a subamostragem.}
\item{sby_audit}{Indicador lógico escalar que controla se metadados de auditoria devem ser preservados no resultado interno da etapa. O padrão é \code{FALSE}, priorizando uma saída operacional simples. Quando \code{TRUE}, facilita rastrear contagens, parâmetros resolvidos e diagnósticos, com maior custo de memória.}
\item{sby_restore_types}{Indicador lógico escalar que define se tipos numéricos inferidos dos preditores originais devem ser restaurados após o processamento matricial. O padrão é \code{TRUE}. Mantê-lo ativado favorece integração com pipelines que dependem de classes numéricas originais; desativá-lo preserva a representação matricial convertida com menor pós-processamento.}
\item{sby_knn_algorithm}{String escalar escolhida entre \code{"auto"}, \code{"kd_tree"}, \code{"cover_tree"}, \code{"brute"}, \code{"Kmknn"}, \code{"Vptree"}, \code{"Exhaustive"}, \code{"Annoy"} e \code{"Hnsw"}. O padrão é o vetor de escolhas com resolução por \code{match.arg()}, efetivamente selecionando \code{"auto"} quando o usuário não informa valor. Esse parâmetro determina a estrutura de busca KNN e afeta o compromisso entre exatidão, velocidade e compatibilidade com o engine.}
\item{sby_knn_engine}{String escalar escolhida entre \code{"auto"}, \code{"FNN"}, \code{"BiocNeighbors"} e \code{"RcppHNSW"}. O padrão é resolvido como \code{"auto"}, permitindo que o pacote escolha um provedor compatível. A escolha impacta dependências usadas, suporte a paralelismo, métricas disponíveis e possibilidade de busca aproximada.}
\item{sby_distance_metric}{String escalar escolhida entre \code{"euclidean"}, \code{"ip"} e \code{"cosine"}. O padrão é resolvido como \code{"euclidean"}. A métrica define a geometria da vizinhança; escolhas não euclidianas podem alterar substancialmente quais observações majoritárias são consideradas próximas à classe minoritária.}
\item{sby_knn_workers}{Número inteiro positivo de workers usados por engines com suporte a paralelização. O padrão é \code{1L}, executando em modo sequencial. Aumentar esse valor pode reduzir tempo de busca em bases maiores, mas também aumenta consumo de recursos e pode restringir a resolução automática do engine.}
\item{sby_hnsw_m}{Número inteiro positivo que controla a conectividade máxima por camada do índice HNSW quando \code{sby_knn_engine = "RcppHNSW"}. O padrão é \code{16L}. Valores maiores tendem a aumentar recall e custo de memória; valores menores reduzem o índice, mas podem degradar a qualidade dos vizinhos aproximados.}
\item{sby_hnsw_ef}{Número inteiro positivo que define o tamanho da lista dinâmica de candidatos explorados pelo HNSW. O padrão é \code{200L}. Valores maiores tornam a busca aproximada mais fiel à vizinhança exata, ao custo de maior latência; valores menores aceleram a consulta com possível perda de precisão.}
\item{sby_skip}{Indicador lógico escalar que define se a etapa deve ser ignorada ao aplicar \code{bake()} em novos dados. O padrão é \code{TRUE}, configuração recomendada para etapas que alteram o número de linhas durante treinamento. Definir \code{FALSE} força a aplicação em novos dados e pode ser inadequado para fluxos de predição.}
\item{sby_id}{String escalar que identifica unicamente a etapa dentro da \code{recipe}. O padrão é \code{recipes::rand_id("balance")}, gerando um identificador reprodutível apenas no contexto da construção da etapa. Esse valor é usado por \code{recipes} para rastreamento, impressão e manutenção do pipeline. }
}
\value{Objeto \code{recipe} com uma etapa \code{sby_step_adanear} adicionada ao pipeline.}
\details{
A função utiliza uma arquitetura de busca espacial configurável para calcular
vizinhos próximos sobre preditores numéricos previamente padronizados por
Z-score. A seleção de \code{sby_knn_engine}, \code{sby_knn_algorithm} e
\code{sby_distance_metric} controla simultaneamente o provedor computacional, a
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
para \code{sby_distance_metric = "ip"}, o pacote realiza normalização L2 prévia
automática das matrizes envolvidas na busca para garantir consistência
espacial da métrica e evitar que diferenças de norma dominem a vizinhança. A
distância de cosseno é definida por \eqn{d(x, y) = 1 - \frac{\sum_i x_i y_i}{\sqrt{\sum_i x_i^2} \sqrt{\sum_i y_i^2}}}
e privilegia a orientação angular dos vetores em vez da magnitude absoluta.

Em \code{FNN}, os algoritmos \code{kd_tree}, \code{cover_tree}, \code{brute} e \code{auto} operam de
forma exata e aceitam somente \code{euclidean}. Em \code{BiocNeighbors}, \code{Kmknn},
\code{Vptree} e \code{Exhaustive} representam alternativas exatas, enquanto \code{Annoy} e
\code{Hnsw} representam alternativas aproximadas; nesse engine, \code{euclidean} e
\code{cosine} são aceitas. Em \code{RcppHNSW}, a busca é aproximada por grafo HNSW e os
parâmetros \code{sby_hnsw_m} e \code{sby_hnsw_ef} controlam, respectivamente, a
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
