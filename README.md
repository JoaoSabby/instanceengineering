# instenginer — balanceamento binário ADASYN + NearMiss-1

`instenginer` é um pacote R para **engenharia de instâncias em problemas de
classificação binária desbalanceados**. O pacote oferece rotinas de
sobreamostragem **ADASYN**, subamostragem **NearMiss-1** e um pipeline híbrido
**ADASYN + NearMiss-1** chamado `sby_adanear()`.

Internamente, as rotinas trabalham com matrizes numéricas, padronização Z-score,
consultas KNN configuráveis, fallback com `Rfast` e kernels nativos em C para
partes críticas quando disponíveis.

## Estado da API

A versão atual do pacote é **0.2.6**. O pacote está em desenvolvimento e a API
pública foi organizada para usar:

- Funções e parâmetros com prefixo `sby_` e padrão `snake_case`.
- Interface principal por **fórmula + dados**: `sby_formula` e `sby_data`.
- Preditores numéricos e alvo binário com exatamente duas classes.
- Retorno padrão como `tibble` balanceado.
- Coluna de desfecho padronizada como `TARGET` no objeto retornado.
- `sby_audit = FALSE` para retornar apenas os dados balanceados.
- `sby_audit = TRUE` para retornar lista com dados balanceados, diagnósticos,
  informações de escala e metadados de tipos.

> **Importante:** em chamadas como `sby_adanear(sby_y ~ ., sby_data)`, o lado
> esquerdo da fórmula identifica o desfecho e o lado direito identifica os
> preditores. Use `~ .` para usar todas as demais colunas como preditores.

### Contrato de fórmulas e dados

As funções públicas aceitam fórmulas para **selecionar colunas já existentes** em
`sby_data`. Transformações, interações e offsets da sintaxe de fórmulas do R
(por exemplo, `log(x)`, `x1:x2` ou `offset(z)`) devem ser calculados antes da
chamada e armazenados como colunas explícitas. Essa restrição evita divergências
entre a seleção por fórmula, a restauração de tipos e o pipeline matricial usado
pelas rotinas KNN e nativas.

Preditores devem ser numéricos, finitos e densos. Matrizes esparsas do pacote
`Matrix` são rejeitadas com erro explícito para evitar densificação acidental de
bases grandes. Colunas constantes também são rejeitadas porque a padronização
Z-score exige desvio padrão positivo.

Para bases pequenas, `sby_over_ratio` positivo sempre gera ao menos uma linha
sintética; use uma razão positiva explícita e uma `sby_seed` inteira para
reprodutibilidade.

## KNN, métricas e engines

As rotinas usam KNN para estimar vizinhanças locais. Os principais controles são:

- `sby_knn_engine`: engine de busca (`"auto"`, `"FNN"`, `"BiocNeighbors"`,
  `"RcppHNSW"`).
- `sby_knn_algorithm`: algoritmo dentro do engine (`"kd_tree"`, `"cover_tree"`,
  `"brute"`, `"Kmknn"`, `"Vptree"`, `"Exhaustive"`, `"Annoy"`, `"Hnsw"`, etc.).
- `sby_knn_distance_metric`: métrica (`"euclidean"`, `"cosine"`, `"ip"`).
- `sby_knn_workers`: número de workers quando o backend oferece paralelismo.
- `sby_knn_hnsw_m` e `sby_knn_hnsw_ef`: parâmetros do HNSW quando
  `sby_knn_engine = "RcppHNSW"`.

Resumo de compatibilidade:

| Engine | Tipo de busca | Métricas suportadas |
|---|---|---|
| `FNN` | Exata | `euclidean` |
| `BiocNeighbors` | Exata ou aproximada, conforme `sby_knn_algorithm` | `euclidean`, `cosine` |
| `RcppHNSW` | Aproximada por HNSW | `euclidean`, `cosine`, `ip` |

Consultas KNN longas são executadas em blocos para permitir interrupção por
`Ctrl + C`. Ajuste os blocos com:

```r
options(instenginer.sby_knn_query_chunk_size = 1000L)
options(instenginer.sby_hnsw_query_chunk_size = 100L)
```

No Unix, chamadas nativas longas do `RcppHNSW` rodam por padrão em um processo
filho monitorado pelo R, permitindo que `Ctrl + C` encerre também fases
bloqueantes como a construção do índice HNSW. Para voltar ao caminho direto,
use:

```r
options(instenginer.sby_hnsw_interruptible_fork = FALSE)
```

## Funções principais

```r
# Pipeline híbrido: primeiro gera amostras sintéticas com ADASYN e depois
# reduz a classe majoritária com NearMiss-1
sby_adanear(
  sby_formula,
  sby_data,
  sby_over_ratio = 0.2,
  sby_under_ratio = 0.5,
  sby_knn_over_k = 5L,
  sby_knn_under_k = 5L,
  sby_seed = sample.int(10L^5L, 1L),
  sby_audit = FALSE
)

# Somente sobreamostragem ADASYN da classe minoritária
sby_adasyn(
  sby_formula,
  sby_data,
  sby_over_ratio = 0.2,
  sby_knn_over_k = 5L,
  sby_seed = sample.int(10L^5L, 1L),
  sby_audit = FALSE
)

# Somente subamostragem NearMiss-1 da classe majoritária
sby_nearmiss(
  sby_formula,
  sby_data,
  sby_under_ratio = 0.5,
  sby_knn_under_k = 5L,
  sby_seed = sample.int(10L^5L, 1L),
  sby_audit = FALSE
)
```

## Etapas para `recipes`

O pacote também oferece etapas supervisionadas para pipelines `recipes`:

```r
# Etapa ADASYN para recipes.
sby_step_adasyn(
  recipe,
  ...,
  sby_over_ratio = 0.2,
  sby_knn_over_k = 5L,
  sby_seed = sample.int(10L^5L, 1L),
  sby_audit = FALSE
)

# Etapa NearMiss-1 para recipes.
sby_step_nearmiss(
  recipe,
  ...,
  sby_under_ratio = 0.5,
  sby_knn_under_k = 5L,
  sby_seed = sample.int(10L^5L, 1L),
  sby_audit = FALSE
)

# Etapa combinada ADASYN + NearMiss-1 para recipes.
sby_step_adanear(
  recipe,
  ...,
  sby_over_ratio = 0.2,
  sby_under_ratio = 0.5,
  sby_knn_over_k = 5L,
  sby_knn_under_k = 5L,
  sby_seed = sample.int(10L^5L, 1L),
  sby_audit = FALSE
)
```

Por padrão, as etapas usam `skip = TRUE`, pois alteram o número de linhas do
conjunto processado e normalmente devem ser aplicadas apenas no treinamento

## Exemplo rápido com `sby_adanear()`

```r
library(instenginer)

# A semente aqui controla apenas a criação do exemplo reproduzível
set.seed(42)

# Cria dois preditores numéricos. As rotinas de sampling esperam preditores
# numéricos; variáveis categóricas devem ser tratadas antes do balanceamento
sby_x <- tibble::tibble(
  sby_a = rnorm(40),
  sby_b = rnorm(40)
)

# Cria um alvo binário desbalanceado: 10 observações minoritárias e 30
# majoritárias. A coluna do alvo pode ter qualquer nome na entrada
sby_y <- factor(c(rep("minority", 10), rep("majority", 30)))

# Junta preditores e alvo em um único data frame, pois a API pública usa fórmula + dados
sby_data <- tibble::add_column(sby_x, sby_y = sby_y)

# Aplica o pipeline híbrido:
# - sby_formula = sby_y ~ . informa que sby_y é o alvo e as demais colunas são
#   preditores;
# - sby_over_ratio controla a geração sintética ADASYN;
# - sby_under_ratio controla a razão minoria/maioria final no NearMiss-1;
#   use 1 para reduzir a majoritária até igualar a minoritária disponível;
# - sby_seed fixo torna a geração e desempates reproduzíveis.
sby_balanced <- sby_adanear(
  sby_formula = sby_y ~ .,
  sby_data = sby_data,
  sby_over_ratio = 0.5,
  sby_under_ratio = 0.8,
  sby_seed = 123
)

# O retorno padrão é um tibble. A coluna alvo é padronizada como TARGET.
sby_balanced
```

## Auditoria

```r
# Com sby_audit = TRUE, a função retorna uma lista com dados finais,
# resultados intermediários e diagnósticos de contagem/configuração
sby_audit <- sby_adanear(
  sby_formula = sby_y ~ .,
  sby_data = sby_data,
  sby_over_ratio = 0.5,
  sby_under_ratio = 0.8,
  sby_seed = 123,
  sby_audit = TRUE
)

# Diagnósticos incluem contagens de linhas, distribuição de classes e
# parâmetros KNN resolvidos
sby_audit$sby_diagnostics

# Dados balanceados finais.
sby_audit$sby_balanced_data
```

## Exemplos individuais

```r
# Apenas ADASYN: aumenta adaptativamente a classe minoritária e mantém todos os
# exemplos originais
sby_only_over <- sby_adasyn(
  sby_formula = sby_y ~ .,
  sby_data = sby_data,
  sby_over_ratio = 0.5,
  sby_seed = 123
)

# Apenas NearMiss-1: reduz a classe majoritária priorizando exemplos próximos à
# classe minoritária
sby_only_under <- sby_nearmiss(
  sby_formula = sby_y ~ .,
  sby_data = sby_data,
  sby_under_ratio = 0.8,
  sby_seed = 123
)
```

## Exemplo com `recipes`

```r
library(recipes)

# Define uma recipe simples. O desfecho é sby_y e os preditores são sby_a/sby_b.
sby_rec <- recipe(sby_y ~ ., data = sby_data)

# Seleciona explicitamente o desfecho para a etapa supervisionada
sby_rec <- sby_step_adanear(
  recipe = sby_rec,
  all_outcomes(),
  sby_over_ratio = 0.5,
  sby_under_ratio = 0.8,
  sby_seed = 123
)

# prep() treina a etapa e resolve a coluna de desfecho selecionada.
sby_rec_prepped <- prep(sby_rec, training = sby_data)

# bake() aplica a etapa ao conjunto informado. Como a etapa altera linhas, use
# com cuidado fora do treinamento.
sby_rec_balanced <- bake(sby_rec_prepped, new_data = sby_data)
```

## Instalação local

```sh
R CMD INSTALL .
```

## Dependências

Dependências importadas pelo pacote:

```r
install.packages(c(
  "cli", "FNN", "generics", "recipes", "rlang", "RcppHNSW", "Rfast", "tibble"
))
```

Dependências opcionais para o engine `BiocNeighbors` e paralelismo via
Bioconductor:

```r
install.packages("BiocManager")
BiocManager::install(c("BiocNeighbors", "BiocParallel"))
```

## Ambiente de desenvolvimento

O repositório inclui um `Dockerfile` com R, toolchain de compilação e as
dependências de sistema necessárias para desenvolvimento e validação do pacote

```sh
docker build -t instenginer-r .
docker run --rm -it -v "$PWD":/workspace/instenginer instenginer-r
```

Dentro do container, valide o pacote com:

```sh
R CMD build .
R CMD check instenginer_0.2.6.tar.gz
```

## Arquivos principais

- `DESCRIPTION`: metadados, versão e dependências do pacote R.
- `NAMESPACE`: funções exportadas, métodos S3 e carregamento da biblioteca
  nativa.
- `R/`: funções R, helpers internos e métodos S3 das etapas `recipes`.
- `src/instenginer.c`: kernels nativos em C compilados na instalação do pacote.
- `man/`: documentação gerada a partir dos blocos roxygen2.

## Validação recomendada

```sh
R CMD build .
R CMD check instenginer_0.2.6.tar.gz
R CMD INSTALL .
```

Quando o binário do R não estiver disponível no ambiente, valide ao menos a
estrutura textual com `git diff --check` e buscas com `rg`.
