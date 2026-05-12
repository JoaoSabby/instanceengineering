# Checklist de revisão profunda do pacote `instenginer`

Este checklist documenta a revisão feita para investigar regressão severa de tempo de execução, capacidade de interrupção por `Ctrl + C`, bugs, desperdícios de memória/processamento e oportunidades de otimização.

## Achados críticos corrigidos

- [x] Validar se a rotina de inicialização nativa segue o nome exigido pelo R para o pacote carregado.
  - Correção: `R_init_adanear` foi renomeada para `R_init_instenginer`, garantindo que o R registre as rotinas nativas ao carregar o pacote.
  - Impacto esperado: volta do caminho nativo em C para z-score, reversão de escala, geração sintética ADASYN e checagem de interrupção.
- [x] Conferir compatibilidade entre `R_forceSymbols(dll, TRUE)` e chamadas `.Call()`.
  - Correção: chamadas `.Call()` passaram a usar os símbolos nativos registrados pelo `useDynLib(..., .registration = TRUE)`, em vez de strings com `PACKAGE`.
  - Impacto esperado: evita falhas ou fallback indireto quando o registro nativo está ativo e símbolos forçados são exigidos.
- [x] Remover barras de progresso em consultas HNSW internas repetidas.
  - Correção: `RcppHNSW::hnsw_build()` e `RcppHNSW::hnsw_search()` usam `progress = "none"`.
  - Impacto esperado: reduz overhead de I/O/console em processamento em blocos, principalmente em bases grandes ou sessões não interativas.
- [x] Reduzir cópia temporária desnecessária no cálculo das proporções ADASYN.
  - Correção: a máscara lógica de vizinhos majoritários é redimensionada por `dim<-`, evitando criar uma nova matriz via `matrix()` apenas para chamar `rowMeans()`.
  - Impacto esperado: menor pressão de memória e menor tempo em bases com muitas linhas minoritárias.

## Checklist de performance

- [x] Verificar se rotinas C são registradas pelo nome correto do pacote.
- [x] Verificar se `sby_adanear_native_available()` pode detectar a DLL nativa após carregamento.
- [x] Verificar se z-score e reversão de z-score usam C quando disponível.
- [x] Verificar se geração sintética ADASYN usa C quando disponível.
- [x] Verificar se loops C longos possuem `R_CheckUserInterrupt()` periódico.
- [x] Verificar se consultas FNN e HNSW são executadas em blocos para permitir pontos de interrupção entre blocos.
- [x] Verificar overhead de progresso/log em engines KNN chamados repetidamente.
- [x] Verificar alocações temporárias evitáveis em cálculo de máscara/razões.
- [ ] Executar benchmark com a base real do usuário comparando antes/depois.
- [ ] Ajustar `options(instenginer.sby_knn_query_chunk_size = ...)` para calibrar latência de `Ctrl + C` versus overhead de chamadas KNN.
- [ ] Avaliar `sby_knn_workers > 1`: paralelismo pode acelerar ou piorar por overhead e cópias dependendo da base.
- [ ] Avaliar engine aproximado (`RcppHNSW`) para bases muito grandes, validando impacto estatístico da aproximação.

## Checklist de interrupção por `Ctrl + C`

- [x] Checar interrupção no início de `sby_adanear()`, `sby_adasyn()` e `sby_nearmiss()`.
- [x] Checar interrupção antes/depois de etapas pesadas em R.
- [x] Checar interrupção dentro dos loops C de z-score e geração sintética.
- [x] Checar interrupção entre blocos de consulta FNN/HNSW.
- [ ] Testar manualmente em uma sessão R interativa porque o ambiente de revisão não possui R instalado.
- [ ] Para `BiocNeighbors::queryKNN()`, validar na prática a responsividade durante a chamada nativa interna; caso a latência seja alta, considerar particionamento específico com índice pré-construído para evitar reconstrução por bloco.

## Checklist de bugs e robustez

- [x] Verificar compatibilidade entre `NAMESPACE` e registro nativo.
- [x] Verificar limites nativos de linhas sintéticas antes de alocar matriz C.
- [x] Verificar índices de vizinhos no C antes de acessar matriz minoritária.
- [x] Verificar divisões por desvio padrão zero/indefinido antes de escalar.
- [x] Verificar validação de número de vizinhos maior que zero.
- [x] Verificar preservação de nomes de colunas após escala/reversão.
- [ ] Adicionar testes automatizados de carregamento da DLL e uso de símbolos `.Call()` assim que houver infraestrutura de testes.
- [ ] Adicionar teste que force `sby_adanear_native_available()` a retornar `TRUE` após `library(instenginer)`.
- [ ] Adicionar teste de equivalência entre caminho C e fallback R para z-score e geração sintética com semente fixa.

## Checklist de memória

- [x] Evitar cópia de máscara lógica em ADASYN antes de `rowMeans()`.
- [x] Confirmar uso de matriz `double` para interfaces nativas.
- [x] Confirmar pré-alocação da matriz sintética no caminho C e no fallback R.
- [ ] Medir pico de memória em bases grandes com `Rprofmem()` ou `profmem`.
- [ ] Avaliar se `rbind(sby_x_scaled, sby_synthetic_matrix)` pode ser substituído por pré-alocação quando a memória for gargalo.
- [ ] Avaliar custo de conversões repetidas `as.data.frame()`/`tibble` em fluxos `recipes`.

## Checklist de interferencia do `.Renviron` no XGBoost

- [x] Identificar que `OMP_NUM_THREADS=4` pode limitar diretamente o paralelismo OpenMP usado pelo XGBoost quando `nthread` nao e informado explicitamente.
- [x] Separar variaveis que afetam treino XGBoost diretamente de variaveis que afetam apenas preprocessamento, tuning ou paralelismo externo.
- [x] Documentar plano de benchmark com `nthread = 4, 8, 16, 24, 32, 48`.
- [x] Adicionar script de diagnostico do ambiente e benchmark sintetico opcional em `tools/diagnose_xgboost_env.R`.
- [ ] Executar benchmark com a base real em uma maquina R com XGBoost instalado.
- [ ] Remover duplicidade do bloco no `.Renviron` operacional para reduzir risco de manutencao.

## Checklist de validação pós-ajuste

- [ ] `R CMD INSTALL .`
- [ ] `R CMD check . --no-manual --no-build-vignettes`
- [ ] Smoke test: `library(instenginer); getLoadedDLLs()[["instenginer"]]`.
- [ ] Smoke test: chamar `sby_adasyn()` em base pequena e confirmar que finaliza.
- [ ] Smoke test: chamar `sby_nearmiss()` em base pequena e confirmar que finaliza.
- [ ] Smoke test: chamar `sby_adanear()` em base pequena e confirmar que finaliza.
- [ ] Teste interativo: iniciar base grande e interromper com `Ctrl + C`.
- [ ] Benchmark real: comparar tempo total com a mesma base usada antes da regressão.
