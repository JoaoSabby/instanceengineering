# Style Guide — instenginer

Este documento define regras de estilo, organização, nomenclatura, documentação e padrões de programação para o pacote `instenginer`.

O projeto é um pacote R com integração nativa em C, documentação via roxygen2 e métodos S3 para integração com o ecossistema `recipes`.

## 1. Idioma do código e da documentação

### 1.1 Documentação

A documentação de usuário, os comentários explicativos e as mensagens funcionais devem ser escritos preferencialmente em português brasileiro.

Exemplo:

```r
# Verifica se ha solicitacao de interrupcao antes de configurar a etapa
sby_adanear_check_user_interrupt()
```

### 1.2 Identificadores

Nomes de funções, variáveis, argumentos e objetos internos devem ser escritos em inglês técnico, com prefixos padronizados.

Exemplos:

```r
sby_compute_z_score_params()
sby_validate_knn_workers()
sby_resolve_knn_engine()
```

## 2. Convenção geral de nomenclatura

### 2.1 Regra principal

Todos os identificadores novos em R devem usar `snake_case`.

Formato recomendado para funções internas:

```text
[prefixo padrão]_[verbo de ação em inglês no infinitivo]_[objeto onde será aplicada a ação]
```

Exemplos:

```r
sby_compute_z_score_params()
sby_validate_sampling_inputs()
sby_resolve_knn_algorithm()
sby_generate_adasyn_samples()
```

### 2.2 Prefixo padrão

Deve-se usar o prefixo `sby_` para funções, argumentos e variáveis específicas do pacote.

Exemplos:

```r
sby_formula
sby_data
sby_knn_engine
sby_knn_algorithm
sby_target_factor
sby_predictor_data
```

### 2.3 Estrutura de nomes de funções

Funções devem seguir uma das estruturas descritas nesta seção.

#### Funções de ação

```text
sby_[verbo]_[objeto]
```

Exemplos:

```r
sby_validate_knn_workers()
sby_compute_z_score_params()
sby_restore_numeric_column_types()
```

#### Funções de criação

```text
sby_create_[objeto]
```

Exemplos:

```r
sby_create_bioc_neighbor_param()
sby_create_knn_bioc_parallel_param()
```

#### Funções de resolução de configuração

```text
sby_resolve_[objeto]
```

Exemplos:

```r
sby_resolve_knn_engine()
sby_resolve_knn_algorithm()
```

#### Funções de validação

```text
sby_validate_[objeto]
```

Exemplos:

```r
sby_validate_hnsw_params()
sby_validate_logical_scalar()
sby_validate_sampling_inputs()
```

#### Funções de cálculo

```text
sby_compute_[objeto]
```

Exemplos:

```r
sby_compute_majority_retention_count()
sby_compute_minority_expansion_count()
sby_compute_z_score_params()
```

#### Funções de transformação

```text
sby_apply_[transformacao]_[objeto]
sby_revert_[transformacao]_[objeto]
sby_restore_[objeto]
```

Exemplos:

```r
sby_apply_z_score_scaling_matrix()
sby_revert_z_score_scaling_matrix()
sby_restore_numeric_column_types()
```

## 3. Funções públicas

### 3.1 Prefixo

Funções públicas exportadas devem iniciar com `sby_`.

Exemplos:

```r
sby_adasyn()
sby_nearmiss()
sby_adanear()
sby_step_adasyn()
sby_step_nearmiss()
sby_step_adanear()
```

### 3.2 Exportação

Funções públicas devem conter `@export` na documentação roxygen2.

Exemplo:

```r
#' @return Objeto `recipe` com uma etapa `sby_step_adasyn` adicionada ao pipeline.
#' @export
sby_step_adasyn <- function(...)
```

### 3.3 Documentação obrigatória para funções públicas

Toda função pública deve conter:

- título curto em português;
- bloco `@description`;
- bloco `@details` quando houver comportamento não trivial;
- bloco `@usage` quando a assinatura for extensa;
- um `@param` para cada argumento;
- um `@return`;
- `@references`, quando houver referência metodológica;
- `@export`.

## 4. Funções internas

### 4.1 Documentação

Funções internas também devem ser documentadas com roxygen2.

Devem conter:

- título curto;
- `@details`, quando necessário;
- `@param` para cada argumento;
- `@return`;
- `@noRd`.

Exemplo:

```r
#' Calcular parametros de z-score
#'
#' @details
#' A funcao implementa uma unidade interna do fluxo de balanceamento com contrato de entrada explicito e retorno controlado
#'
#' @param sby_x_matrix Matriz numerica usada para estimar centro e escala
#'
#' @return Lista com vetores numericos `centers` e `scales`
#' @noRd
sby_compute_z_score_params <- function(sby_x_matrix){
  ...
}
```

### 4.2 Uso de `@noRd`

Deve-se usar `@noRd` em funções internas para manter documentação técnica no código sem gerar página `.Rd`.

## 5. Métodos S3

### 5.1 Nomes

Métodos S3 devem usar o formato base R:

```text
generico.classe
```

Exemplos:

```r
prep.step_sby_step_adasyn()
bake.step_sby_step_adasyn()
print.step_sby_step_adasyn()
tidy.step_sby_step_adasyn()
required_pkgs.step_sby_step_adasyn()
```

### 5.2 Registro

Todos os métodos S3 devem estar registrados no `NAMESPACE`.

Exemplo:

```r
S3method(prep, step_sby_step_adasyn)
S3method(bake, step_sby_step_adasyn)
```

### 5.3 Argumentos S3

Métodos S3 devem preservar os nomes esperados pelo genérico.

Exemplos:

```r
prep.step_sby_step_adasyn <- function(x, training, info = NULL, ...)
bake.step_sby_step_adasyn <- function(object, new_data, ...)
print.step_sby_step_adasyn <- function(x, width = ..., ...)
tidy.step_sby_step_adasyn <- function(x, ...)
```

### 5.4 Normalização interna de nomes

Quando o genérico exigir nomes genéricos como `x`, `object` ou `new_data`, deve-se normalizar internamente para nomes com prefixo `sby_`.

Exemplo:

```r
sby_object <- object
sby_new_data <- new_data
```

## 6. Classes de steps `recipes`

### 6.1 Nomenclatura

Classes de steps devem seguir:

```text
step_sby_step_[algoritmo]
```

Exemplos:

```r
step_sby_step_adasyn
step_sby_step_nearmiss
step_sby_step_adanear
```

### 6.2 Funções públicas de steps

Funções públicas que adicionam steps devem seguir:

```text
sby_step_[algoritmo]
```

Exemplos:

```r
sby_step_adasyn()
sby_step_nearmiss()
sby_step_adanear()
```

### 6.3 Argumentos padrão de steps

Steps devem expor argumentos compatíveis com `recipes`, incluindo:

```r
recipe
...
role
trained
columns
skip
id
```

Argumentos próprios do pacote devem usar prefixo `sby_`.

## 7. Argumentos

### 7.1 Prefixo obrigatório

Argumentos próprios do pacote devem iniciar com `sby_`.

Exemplos:

```r
sby_over_ratio
sby_under_ratio
sby_knn_over_k
sby_knn_under_k
sby_knn_algorithm
sby_knn_engine
sby_knn_workers
```

### 7.2 Argumentos herdados de APIs externas

Argumentos exigidos por APIs externas podem manter seus nomes originais.

Exemplos:

```r
recipe
...
role
trained
columns
skip
id
x
object
new_data
training
info
width
```

### 7.3 Booleanos

Argumentos booleanos devem ter nomes que indiquem controle ou estado.

Exemplos:

```r
sby_audit
sby_return_scaled
sby_restore_types
sby_trained
sby_skip
```

### 7.4 Inteiros

Literais inteiros devem usar sufixo `L`.

Exemplos:

```r
1L
5L
16L
200L
```

### 7.5 Opções enumeradas

Argumentos com conjunto fechado de opções devem usar `match.arg()`.

Exemplo:

```r
sby_knn_algorithm <- match.arg(arg = sby_knn_algorithm)
sby_knn_engine <- match.arg(arg = sby_knn_engine)
sby_knn_distance_metric <- match.arg(arg = sby_knn_distance_metric)
```

## 8. Formatação de código R

### 8.1 Atribuição

Deve-se usar `<-` para atribuição.

```r
sby_terms <- rlang::enquos(...)
```

### 8.2 Chaves

A chave de abertura deve ficar na mesma linha da função, `if`, `else`, `for` ou bloco equivalente.

```r
if(sby_adanear_native_available()){
  ...
}else{
  ...
}
```

### 8.3 Retorno explícito

Deve-se usar `return()` explicitamente para retornos principais.

```r
return(list(
  centers = as.numeric(sby_params$centers),
  scales  = as.numeric(sby_params$scales)
))
```

### 8.4 Chamadas multilinha

Chamadas com muitos argumentos devem ser quebradas em múltiplas linhas, com um argumento por linha.

```r
sby_formula_data <- sby_extract_formula_data(
  sby_formula = sby_formula,
  sby_data    = sby_data
)
```

### 8.5 Alinhamento opcional

Quando houver blocos de argumentos semanticamente relacionados, é permitido alinhar `=` para leitura tabular.

```r
sby_subclass        = "sby_step_adasyn",
sby_sampling_method = "adasyn",
sby_terms           = sby_terms
```

### 8.6 Espaços

O padrão atual do projeto usa:

```r
if(condicao){
  ...
}
```

e não:

```r
if (condicao) {
  ...
}
```

Para consistência com o código existente, deve-se manter o estilo atual em novos arquivos, salvo decisão explícita de migração global do projeto para outro padrão.

## 9. Comentários em R

### 9.1 Comentários explicativos

Comentários devem explicar intenção, decisão ou contexto, não apenas repetir o código.

Bom:

```r
# Verifica se a etapa foi treinada antes de aplicar bake
if(!isTRUE(sby_object$sby_trained)){
  ...
}
```

Evitar:

```r
# If
if(...){
  ...
}
```

### 9.2 Idioma

Comentários devem ser escritos em português brasileiro.

### 9.3 Estilo

Deve-se usar comentários simples com `#` para lógica interna.

```r
# Resolve opcoes declaradas de algoritmo, engine e metrica KNN
```

### 9.4 Comentários finais legados

O projeto possui blocos finais como:

```r
####
## Fim
#
```

Recomenda-se:

- manter em arquivos existentes se a intenção for realizar alteração mínima;
- evitar em novos arquivos, salvo se o projeto decidir tornar esse padrão obrigatório.

## 10. roxygen2

### 10.1 Configuração

O pacote usa roxygen2 com Markdown habilitado:

```text
Roxygen: list(markdown = TRUE)
```

### 10.2 Tags permitidas

As tags observadas e recomendadas são:

- `@description`
- `@details`
- `@usage`
- `@param`
- `@return`
- `@references`
- `@export`
- `@keywords internal`
- `@noRd`

### 10.3 Ordem recomendada das tags

Para funções públicas:

```r
#' Título curto
#'
#' @description
#' Descrição da função.
#'
#' @details
#' Detalhes adicionais.
#'
#' @usage
#' funcao(...)
#'
#' @param arg Descrição.
#'
#' @return Descrição do retorno.
#'
#' @references
#' Referências, quando aplicável.
#'
#' @export
```

Para funções internas:

```r
#' Título curto
#'
#' @details
#' Detalhes técnicos.
#'
#' @param arg Descrição.
#'
#' @return Descrição do retorno.
#' @noRd
```

### 10.4 Markdown na documentação

Como Markdown está habilitado, devem-se usar crases para nomes de funções, argumentos e valores literais.

Exemplos:

```r
#' `sby_adasyn()` executa sobreamostragem adaptativa.
#' O padrão é `FALSE`.
#' Use `sby_knn_engine = "RcppHNSW"`.
```

## 11. Mensagens, erros e avisos

### 11.1 Wrappers internos

Mensagens, erros e avisos devem usar wrappers internos do pacote.

Exemplos esperados:

```r
sby_adanear_abort()
sby_adanear_warn()
sby_adanear_inform()
```

### 11.2 Prefixo dos argumentos

Wrappers de mensagem devem receber texto em argumento prefixado com `sby_`.

Exemplo:

```r
sby_adanear_abort(
  sby_message = "Mensagem de erro"
)
```

### 11.3 Mensagens

Mensagens para usuário devem ser claras, diagnósticas e em português brasileiro.

## 12. Dependências e chamadas externas

### 12.1 Namespace explícito

Chamadas a pacotes importados ou sugeridos devem preferir `pacote::funcao()`.

Exemplos:

```r
recipes::recipes_pkg_check(...)
rlang::enquos(...)
Rfast::colmeans(...)
tibble::as_tibble(...)
```

### 12.2 Imports no NAMESPACE

Apenas genéricos e símbolos necessários devem ser importados diretamente quando isso for exigido pela integração S3.

Exemplos:

```r
importFrom(generics, required_pkgs)
importFrom(generics, tidy)
importFrom(recipes, bake)
importFrom(recipes, prep)
```

### 12.3 Dependências opcionais

Dependências opcionais devem ser tratadas como `Suggests` e validadas em tempo de execução quando necessário.

## 13. Validação de entradas

### 13.1 Funções dedicadas

Validações recorrentes devem ser extraídas para funções específicas.

Exemplos:

```r
sby_validate_logical_scalar()
sby_validate_knn_workers()
sby_validate_hnsw_params()
sby_validate_sampling_inputs()
```

### 13.2 Validação antecipada

Entradas devem ser validadas no início da função, antes de processamento pesado.

### 13.3 Interrupção do usuário

Operações potencialmente longas devem chamar:

```r
sby_adanear_check_user_interrupt()
```

### 13.4 Erros

Erros devem usar o wrapper interno:

```r
sby_adanear_abort(
  sby_message = "..."
)
```

## 14. Retornos

### 14.1 Funções públicas de sampling

Funções públicas de sampling devem retornar:

- `tibble` balanceado quando auditoria estiver desativada;
- lista de auditoria quando `sby_audit = TRUE`.

### 14.2 Funções internas

Funções internas devem retornar estruturas simples, previsíveis e nomeadas.

Exemplo:

```r
return(list(
  centers = as.numeric(sby_params$centers),
  scales  = as.numeric(sby_params$scales)
))
```

### 14.3 Tibble público, matriz interna

A API pública deve priorizar `tibble`, enquanto operações internas de alto desempenho devem usar `matrix double`.

## 15. Organização de arquivos R

### 15.1 Um conceito principal por arquivo

Cada arquivo deve conter uma função principal ou um conjunto coeso de helpers.

Exemplos:

```text
R/sby_compute_z_score_params.R
R/sby_validate_knn_workers.R
R/sby_step_sampling_helpers.R
```

### 15.2 Nome do arquivo

O nome do arquivo deve refletir o nome da função principal.

Exemplo:

```text
R/sby_compute_z_score_params.R
```

para:

```r
sby_compute_z_score_params()
```

### 15.3 Arquivos de métodos S3

Métodos S3 devem seguir:

```text
R/[generico].[classe].R
```

Exemplos:

```text
R/prep.step_sby_step_adasyn.R
R/bake.step_sby_step_adasyn.R
R/tidy.step_sby_step_adasyn.R
```

## 16. Código C

### 16.1 Comentários

Código C deve usar comentários de documentação no formato:

```c
/**
 * @brief Descrição curta.
 *
 * Detalhamento técnico.
 *
 * @param nome Descrição.
 * @return Descrição.
 */
```

### 16.2 Comentários internos

Comentários internos devem usar `/* ... */` e explicar intenção operacional.

Exemplo:

```c
/* Valida coerencia da estrutura de dados de entrada */
```

### 16.3 Nomes de funções C exportadas para R

Funções C chamadas por `.Call` devem usar o prefixo:

```text
OU_
```

e sufixo `C`.

Exemplos:

```c
OU_CheckUserInterruptC
OU_ComputeZScoreParamsC
OU_ApplyZScoreC
OU_GenerateSyntheticAdasynC
```

### 16.4 Funções C estáticas

Funções auxiliares internas devem ser `static`.

Exemplo:

```c
static void require_real_matrix(SEXP x, const char *name)
```

### 16.5 Registro de rotinas

Toda função C exposta para R deve ser registrada em `CallEntries`.

Exemplo:

```c
static const R_CallMethodDef CallEntries[] = {
  {"OU_CheckUserInterruptC", (DL_FUNC) &OU_CheckUserInterruptC, 0},
  {"OU_ComputeZScoreParamsC", (DL_FUNC) &OU_ComputeZScoreParamsC, 1},
  {NULL, NULL, 0}
};
```

### 16.6 Proteção de memória R

Objetos R alocados em C devem usar `PROTECT()` e `UNPROTECT()` corretamente.

### 16.7 Interrupções

Laços longos em C devem chamar periodicamente:

```c
R_CheckUserInterrupt();
```

### 16.8 RNG

Ao usar RNG do R em C, deve-se usar:

```c
GetRNGstate();
...
PutRNGstate();
```

## 17. `.Call` e integração R/C

### 17.1 Chamada R

Chamadas para C devem usar `.Call()` com `PACKAGE`.

Exemplo:

```r
.Call(
  "OU_ComputeZScoreParamsC",
  sby_x_matrix,
  PACKAGE = "instenginer"
)
```

### 17.2 Fallback em R

Quando houver implementação nativa opcional, deve-se fornecer fallback em R.

Exemplo:

```r
if(sby_adanear_native_available()){
  ...
}else{
  ...
}
```

## 18. Padrões de performance

### 18.1 Matrizes internas

Operações numéricas intensivas devem usar `matrix double`.

### 18.2 Evitar recomputação

Quando possível, artefatos intermediários, como escala e dados normalizados, devem ser preservados, especialmente em fluxos com auditoria.

### 18.3 Backends KNN

Configurações de engine, algoritmo, workers e métricas devem ser resolvidas explicitamente antes da execução principal.

## 19. Padrões de compatibilidade com `recipes`

### 19.1 `skip`

Steps que alteram número de linhas devem usar `skip = TRUE` por padrão.

### 19.2 `prep()`

O método `prep()` deve:

- avaliar seletores;
- validar que exatamente uma coluna de desfecho foi selecionada;
- retornar step treinado.

### 19.3 `bake()`

O método `bake()` deve:

- verificar se a etapa foi treinada;
- validar presença da coluna de desfecho;
- construir fórmula interna;
- executar o método de sampling;
- retornar dados no formato esperado pelo pipeline.

### 19.4 `tidy()`

O método `tidy()` deve retornar metadados resumidos da etapa.

### 19.5 `print()`

O método `print()` deve delegar para helper comum quando possível.

## 20. Constantes e literais

### 20.1 Inteiros

Deve-se usar sufixo `L`.

```r
1L
5L
16L
200L
```

### 20.2 Lógicos

Deve-se usar `TRUE` e `FALSE` em maiúsculas.

### 20.3 Ausência

Deve-se usar `NULL`, `NA`, `NA_real_` ou `NA_integer_` conforme o tipo esperado.

### 20.4 Strings de opções

Strings de opções devem ser minúsculas quando forem opções internas simples.

Exemplos:

```r
"auto"
"brute"
"euclidean"
"cosine"
"ip"
```

Deve-se preservar capitalização quando a string refletir nome de pacote, classe ou backend externo.

Exemplos:

```r
"FNN"
"BiocNeighbors"
"RcppHNSW"
"Kmknn"
"Hnsw"
```

## 21. Exceções e legado

### 21.1 Código legado fora do padrão

Se houver função ou variável fora do padrão, esse estilo não deve ser replicado em código novo.

Exemplo legado identificado:

```r
SbyNormalizeL2()
sbyRowNorm
```

### 21.2 Renomeações

Renomeações devem ser feitas apenas quando:

- não quebrarem API pública;
- forem acompanhadas de atualização de chamadas internas;
- forem cobertas por testes ou checagem manual suficiente.

## 22. Checklist para novas contribuições

Antes de finalizar uma alteração, deve-se verificar:

- [ ] Funções novas usam `snake_case`.
- [ ] Funções e argumentos internos usam prefixo `sby_`.
- [ ] Funções públicas possuem `@export`.
- [ ] Funções internas possuem `@noRd`.
- [ ] Todos os argumentos estão documentados com `@param`.
- [ ] O retorno está documentado com `@return`.
- [ ] Comentários explicam intenção, não apenas repetem o código.
- [ ] Mensagens para usuário estão em português brasileiro.
- [ ] Chamadas externas usam `pacote::funcao()`.
- [ ] Validações ocorrem antes de processamento pesado.
- [ ] Laços longos verificam interrupção do usuário.
- [ ] Código C registra rotinas em `CallEntries`.
- [ ] Código C usa `PROTECT()` e `UNPROTECT()` corretamente.
- [ ] Nenhuma nova exceção de nomenclatura foi introduzida sem justificativa.
