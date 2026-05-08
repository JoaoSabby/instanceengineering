# instanceengineering — OverUnderSampling de alto desempenho

Este repositório agora é um **pacote R** para balanceamento binário com **ADASYN** (oversampling) seguido de **NearMiss-1** (undersampling), com caminho crítico otimizado para matriz numérica, uso de **Rfast** e aceleração em **C puro** compilada pelo próprio mecanismo de instalação do R.

## O que foi revisado

A revisão priorizou desempenho sem abrir mão de segurança numérica e reprodutibilidade.

| Ideia | Decisão | Motivo |
|---|---|---|
| Rcpp/RcppArmadillo | Descartada | Boa ergonomia, mas adiciona dependências e overhead de toolchain; para o gargalo de geração sintética, C puro é suficiente e mais direto. |
| C puro via `.Call` | Implementada | Remove loops R internos do ADASYN, usa ponteiros column-major nativos do R e o RNG do próprio R. |
| Fortran puro | Descartada nesta versão | Excelente para kernels numéricos densos, mas o gargalo principal envolve índices, RNG e integração com objetos R; C puro reduz risco de conversões e simplifica validação. |
| `Rfast` para escala | Mantida como fallback rápido | Já funciona no ambiente-alvo e continua sendo o fallback quando a biblioteca C não puder ser compilada/carregada. |
| `t((t(x)-center)/scale)` | Descartada | Pode ser rápida, mas cria transpostas grandes e pressão de memória; C/Rfast evita cópias extras. |
| Inferência de inteiro por amostra de 1000 linhas | Descartada | Pode classificar coluna incorretamente em bases grandes; a implementação mantém varredura completa para evitar erro silencioso. |
| Seleção automática do KNN | Implementada | `knnAlgorithm = "auto"` usa `kd_tree` até 15 colunas e `brute` acima disso quando o backend é `FNN`. |
| Paralelismo no KNN | Implementado como opção | `FNN` foi mantido por compatibilidade, mas não é o backend paralelo; para paralelismo exato use `knnBackend = "BiocNeighbors"` com `knnWorkers > 1`. |
| `RcppHNSW` | Implementado como opt-in aproximado | Pode ser excelente para bases grandes, mas é aproximado; use explicitamente `knnBackend = "RcppHNSW"` quando o ganho de velocidade justificar pequena variação nos vizinhos. |
| Mensagens com `cli` | Implementada | Erros, avisos e mensagens internas passam por `cli`, com fallback base apenas se `cli` ainda não estiver disponível. |

## Arquivos principais

- `DESCRIPTION`: metadados, dependências e configuração do pacote R.
- `NAMESPACE`: exports das funções públicas e carregamento da biblioteca nativa.
- `R/OverUnderSampling.R`: API R do pacote e fallback seguro.
- `src/over_under_fast.c`: kernels em C puro para z-score e geração sintética ADASYN, compilados na instalação do pacote.
- `man/OverUnderSampling.Rd`: documentação das funções exportadas.
- `ideias.txt`: revisão técnica das alternativas de otimização; não é usado em runtime.

## Instalação do pacote R

### Opção 1: instalar direto do GitHub

Use `pak` ou `remotes`:

```r
install.packages("pak")
pak::pak("JoaoSabby/instance_engineering")
```

ou:

```r
install.packages("remotes")
remotes::install_github("JoaoSabby/instance_engineering")
```

Durante a instalação, o R compila automaticamente `src/over_under_fast.c` via `R CMD SHLIB`/toolchain do próprio R e carrega a biblioteca nativa por `NAMESPACE` com `useDynLib(instanceengineering, .registration = TRUE)`.

### Opção 2: instalar de um clone local

```sh
git clone https://github.com/JoaoSabby/instance_engineering.git
cd instance_engineering
R CMD INSTALL .
```

Ou, dentro do R:

```r
install.packages("remotes")
remotes::install_local(".")
```

Depois de instalado:

```r
library(instanceengineering)
```

## Dependências

No R, instale o conjunto mínimo:

```r
install.packages(c("cli", "FNN", "Rfast", "modeldata"))
```

Para KNN paralelo exato, instale também os pacotes Bioconductor opcionais:

```r
install.packages("BiocManager")
BiocManager::install(c("BiocNeighbors", "BiocParallel"))
```

Para KNN aproximado HNSW via CRAN, instale:

```r
install.packages("RcppHNSW")
```

`FNN` foi mantido como backend padrão por compatibilidade e simplicidade de instalação, mas ele não é a opção de paralelismo. Para distribuir consultas KNN exatas entre workers, use `knnBackend = "BiocNeighbors"` e ajuste `knnWorkers`. Para bases grandes em que busca aproximada é aceitável, `knnBackend = "RcppHNSW"` pode ser uma boa ideia porque expõe `n_threads` e índices HNSW rápidos, mas não é o padrão para evitar alterações silenciosas na seleção dos vizinhos.

## Compilação dos códigos em C

Como o projeto agora é um pacote R, a compilação de `src/over_under_fast.c` acontece automaticamente durante `R CMD INSTALL`, `remotes::install_github()` ou `pak::pak()`. O `NAMESPACE` carrega a biblioteca compilada com `useDynLib(instanceengineering, .registration = TRUE)`.

Para validar manualmente a compilação sem instalar:

```sh
cd instance_engineering
R CMD SHLIB src/over_under_fast.c
```

Em Linux, isso tende a gerar `src/over_under_fast.so`; no macOS, `src/over_under_fast.so` ou `.dylib` conforme a toolchain; no Windows, `src/over_under_fast.dll`.

Para validação completa como pacote:

```sh
R CMD build .
R CMD check instanceengineering_0.1.0.tar.gz
R CMD INSTALL .
```

### Pré-requisitos de compilação

- R instalado com headers de desenvolvimento (`R.h`, `Rinternals.h`, `Rmath.h`).
- Toolchain compatível com `R CMD SHLIB`:
  - Linux: `gcc`, `gfortran` não é necessário para este arquivo C, e pacote de desenvolvimento do R da distribuição.
  - macOS: Xcode Command Line Tools e toolchain configurada pelo R.
  - Windows: Rtools compatível com a versão do R.
- Não compile com `gcc` diretamente, a menos que você replique exatamente as flags de `R CMD SHLIB`; usar `R CMD SHLIB` é o caminho recomendado porque inclui headers, ABI e flags corretas do R.

## Processo passo a passo usado na implementação

1. **Validação do estado inicial**: leitura de `OverUnderSampling.R` e `ideias.txt` para mapear gargalos e propostas existentes.
2. **Conversão para pacote R**: criação de `DESCRIPTION`, `NAMESPACE`, `R/`, `man/` e compilação nativa via `src/`.
3. **Preservação da API**: manutenção das funções exportadas `ApplyAdasynOversampling()`, `ApplyNearmissUndersampling()` e `OverUnderSampling()`.
4. **C puro onde agrega mais desempenho**:
   - cálculo de médias/desvios por coluna;
   - aplicação/reversão de z-score;
   - geração das linhas sintéticas ADASYN com RNG nativo do R.
5. **Fallback sem erro**: caso o C não esteja disponível, usa `Rfast::colmeans()`, `Rfast::colVars()` e `Rfast::eachrow()`.
6. **Revisão de ideias inseguras**: a inferência parcial de tipos foi rejeitada para evitar erro silencioso; a versão completa continua sendo usada.
7. **KNN automático**: `knnAlgorithm = "auto"` passa a escolher entre `kd_tree` e `brute` no backend `FNN` conforme a dimensionalidade.
8. **KNN paralelo opcional**: `knnBackend = "BiocNeighbors"` usa `BiocNeighbors::queryKNN()` com `BiocParallel` para dividir consultas entre workers mantendo busca exata por padrão.
9. **KNN aproximado opcional**: `knnBackend = "RcppHNSW"` usa `RcppHNSW::hnsw_build()`/`hnsw_search()` com `hnswM`, `hnswEf` e `knnWorkers` para workloads grandes em que aproximação é aceitável.
10. **Mensagens padronizadas**: erros e avisos usam `cli`, melhorando legibilidade em scripts e pipelines.
11. **Reprodutibilidade**: `set.seed()` continua controlando tanto o caminho R quanto o caminho C, pois o C usa `GetRNGstate()`/`PutRNGstate()`.

## Exemplo com base binária do pacote `modeldata`

O exemplo abaixo usa `modeldata::two_class_dat`, que possui desfecho binário `Class` e preditores numéricos.

```r
library(instanceengineering)
library(modeldata)

data(two_class_dat, package = "modeldata")

predictors <- subset(two_class_dat, select = -Class)
target <- two_class_dat$Class

table(target)

result <- OverUnderSampling(
  predictorData = predictors,
  targetVector = target,
  overRatio = 0.30,
  underRatio = 0.70,
  kOver = 5L,
  kUnder = 5L,
  seed = 42L,
  restoreTypes = TRUE,
  output = "data.frame",
  knnAlgorithm = "auto",
  knnBackend = "auto",
  knnWorkers = 1L
)

result$diagnostics
balanced <- result$balancedData
str(balanced)
table(balanced$TARGET)
```

### Exemplo apenas com oversampling ADASYN

```r
over <- ApplyAdasynOversampling(
  predictorData = predictors,
  targetVector = target,
  overRatio = 0.50,
  kOver = 5L,
  seed = 42L,
  returnScaled = TRUE,
  output = "matrix",
  knnAlgorithm = "auto",
  knnBackend = "auto",
  knnWorkers = 1L
)

over$diagnostics
```

### Exemplo apenas com NearMiss-1

```r
under <- ApplyNearmissUndersampling(
  predictorData = predictors,
  targetVector = target,
  underRatio = 0.50,
  kUnder = 5L,
  seed = 42L,
  output = "data.frame",
  knnAlgorithm = "auto",
  knnBackend = "auto",
  knnWorkers = 1L
)

under$diagnostics
```


### Exemplo com KNN paralelo via BiocNeighbors

```r
parallel_result <- OverUnderSampling(
  predictorData = predictors,
  targetVector = target,
  overRatio = 0.30,
  underRatio = 0.70,
  kOver = 5L,
  kUnder = 5L,
  seed = 42L,
  output = "data.frame",
  knnBackend = "BiocNeighbors",
  knnWorkers = 4L,
  biocNeighborAlgorithm = "auto"
)

parallel_result$diagnostics
```

Use `biocNeighborAlgorithm = "Kmknn"` ou `"Vptree"` para métodos exatos baseados em árvore, `"Exhaustive"` para força bruta exata e `"Annoy"`/`"Hnsw"` apenas quando busca aproximada for aceitável.

### Exemplo com KNN aproximado via RcppHNSW

```r
hnsw_result <- OverUnderSampling(
  predictorData = predictors,
  targetVector = target,
  overRatio = 0.30,
  underRatio = 0.70,
  kOver = 5L,
  kUnder = 5L,
  seed = 42L,
  output = "data.frame",
  knnBackend = "RcppHNSW",
  knnWorkers = 4L,
  hnswM = 16L,
  hnswEf = 200L
)

hnsw_result$diagnostics
```

`RcppHNSW` é uma boa opção quando o custo das buscas KNN domina o pipeline e uma busca aproximada é aceitável. Para máxima fidelidade ao NearMiss/ADASYN exato, prefira `FNN` ou `BiocNeighbors` com algoritmos exatos. Aumentar `hnswEf` tende a melhorar o recall, mas aumenta tempo de busca; aumentar `hnswM` tende a melhorar conectividade/qualidade do índice, mas aumenta memória e tempo de construção.

## Observações de desempenho

- Use `output = "matrix"` quando o próximo passo do pipeline aceitar matriz; isso reduz conversões para `data.frame`.
- Use `restoreTypes = FALSE` quando os preditores permanecerem em formato numérico contínuo no pipeline de modelagem.
- Para sequências ADASYN + NearMiss, prefira `OverUnderSampling()`, pois ela reutiliza a matriz escalada e evita recomputar z-score.
- Em alta dimensionalidade com backend `FNN`, `knnAlgorithm = "auto"` tende a evitar árvores KNN pouco eficientes e seleciona `brute`.
- Para paralelismo real e busca exata no KNN, configure `knnBackend = "BiocNeighbors"` e `knnWorkers` maior que 1; `FNN` permanece disponível como backend simples e compatível, mas não distribui consultas entre workers.
- Para máxima velocidade em bases grandes, avalie `knnBackend = "RcppHNSW"`; ele é aproximado, então valide métricas/recall antes de usá-lo como padrão de produção.

## Segurança e limitações

- A entrada deve ser `data.frame` ou `matrix` com preditores numéricos sem `NA`.
- O alvo deve ser binário e cada classe precisa ter ao menos duas observações.
- Classes já perfeitamente balanceadas não são aceitas nas rotinas que dependem explicitamente de minoria/maioria.
- O caminho C é compilado na instalação do pacote. Se a biblioteca nativa não estiver carregada em um uso não convencional, o código ainda mantém fallback R/Rfast.
- `RcppHNSW` e `biocNeighborAlgorithm = "Annoy"/"Hnsw"` fazem busca aproximada; isso pode alterar os vizinhos escolhidos e, consequentemente, as linhas sintéticas/retidas.
