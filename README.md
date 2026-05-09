# instanceengineering — balanceamento binário ADASYN + NearMiss

Este repositório é um **pacote R em desenvolvimento** para balanceamento binário
com **ADASYN** (oversampling) e **NearMiss-1** (undersampling), usando matrizes
numéricas internamente, fallback com **Rfast**, backends configuráveis de KNN e
kernels em **C puro** para partes críticas.

## Estado da API

A versão atual do pacote é **0.2.3**. O pacote ainda não foi liberado para
clientes e está em fase de desenvolvimento, ajustes e testes. Portanto, **não há
necessidade de manter compatibilidade com a estrutura anterior**.

A API foi padronizada com as seguintes regras:

- Todas as funções públicas usam prefixo `sby_` e padrão `snake_case`.
- Todos os parâmetros das funções públicas usam prefixo `sby_` e padrão
  `snake_case`.
- As funções R top-level foram separadas em arquivos próprios em `R/`, com o nome
  do arquivo igual ao nome da função.
- A base balanceada retornada pelas rotinas principais é sempre um `tibble`.
- A coluna de desfecho é sempre `TARGET` e sempre aparece como a primeira coluna
  do tibble.
- O parâmetro `sby_audit = FALSE` retorna somente o tibble balanceado.
- O parâmetro `sby_audit = TRUE` retorna uma lista com dados balanceados,
  diagnósticos, informações de escala e metadados de tipos.

## Funções principais

```r
sby_over_under_sampling(
  sby_predictor_data,
  sby_target_vector,
  sby_over_ratio = 0.2,
  sby_under_ratio = 0.5,
  sby_k_over = 5L,
  sby_k_under = 5L,
  sby_seed = 42L,
  sby_audit = FALSE
)

sby_apply_adasyn_oversampling(
  sby_predictor_data,
  sby_target_vector,
  sby_over_ratio = 0.2,
  sby_k_over = 5L,
  sby_seed = 42L,
  sby_audit = FALSE
)

sby_apply_nearmiss_undersampling(
  sby_predictor_data,
  sby_target_vector,
  sby_under_ratio = 0.5,
  sby_k_under = 5L,
  sby_seed = 42L,
  sby_audit = FALSE
)

sby_step_balance(
  sby_recipe,
  ...,
  sby_under_ratio = 0.5,
  sby_k_under = 5L,
  sby_seed = 42L,
  sby_audit = FALSE
)
```

## Exemplo rápido

```r
library(instanceengineering)

set.seed(1)
sby_x <- data.frame(
  sby_a = rnorm(40),
  sby_b = rnorm(40)
)
sby_y <- factor(c(rep("minority", 10), rep("majority", 30)))

sby_balanced <- sby_over_under_sampling(
  sby_predictor_data = sby_x,
  sby_target_vector = sby_y,
  sby_over_ratio = 0.5,
  sby_under_ratio = 0.8
)

sby_balanced
```

Para obter auditoria completa:

```r
sby_audit <- sby_over_under_sampling(
  sby_predictor_data = sby_x,
  sby_target_vector = sby_y,
  sby_over_ratio = 0.5,
  sby_under_ratio = 0.8,
  sby_audit = TRUE
)

sby_audit$sby_diagnostics
sby_audit$sby_balanced_data
```

## Ambiente de desenvolvimento permanente

O repositório inclui um `Dockerfile` e uma configuração `.devcontainer/` com R,
toolchain de compilação, bibliotecas de sistema, `qpdf` e as dependências R
mínimas do pacote.

Para criar o ambiente manualmente:

```sh
docker build -t instanceengineering-r .
docker run --rm -it -v "$PWD":/workspace/instance_engineering instanceengineering-r
```

Dentro do container, valide o pacote com:

```sh
R CMD build .
R CMD check instanceengineering_0.2.3.tar.gz
```

## Instalação local

```sh
R CMD INSTALL .
```

## Dependências

Dependências mínimas do pacote:

```r
install.packages(c("cli", "FNN", "generics", "recipes", "rlang", "Rfast", "tibble"))
```

Para KNN paralelo exato, instale também os pacotes Bioconductor opcionais:

```r
install.packages("BiocManager")
BiocManager::install(c("BiocNeighbors", "BiocParallel"))
```

Para KNN aproximado HNSW via CRAN:

```r
install.packages("RcppHNSW")
```

## Arquivos principais

- `DESCRIPTION`: metadados, versão e dependências do pacote R.
- `NAMESPACE`: exports das funções públicas `sby_*`, métodos S3 e carregamento da
  biblioteca nativa.
- `R/`: funções R top-level separadas em arquivos próprios.
- `src/over_under_fast.c`: kernels em C puro para z-score e geração sintética
  ADASYN, compilados na instalação do pacote.
- `man/`: documentação manual atualizada para a API `sby_*`.

## Validação recomendada

```sh
R CMD build .
R CMD check instanceengineering_0.2.3.tar.gz
R CMD INSTALL .
```

Quando o binário do R não estiver disponível no ambiente, valide ao menos a
estrutura textual com `rg`, `find` e `git diff --check`.
