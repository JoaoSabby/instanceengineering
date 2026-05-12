# Revisão do `.Renviron` para desempenho do XGBoost

## Veredito

Sim: a configuração informada pode interferir diretamente no desempenho do XGBoost, principalmente por causa de `OMP_NUM_THREADS=4`. O XGBoost usa paralelismo OpenMP em treinamento CPU; quando `nthread`/`nthreads` não é definido explicitamente no treino, o runtime OpenMP pode obedecer `OMP_NUM_THREADS` e limitar o treinamento a quatro threads, mesmo em um servidor com 48 núcleos físicos e 96 CPUs lógicas.

Essa configuração pode ser correta para evitar oversubscription quando vários modelos rodam simultaneamente. Porém, para um único treino XGBoost grande, ela provavelmente subutiliza o servidor.

Fontes oficiais consultadas:

- XGBoost Parameters, seção `nthread`: <https://xgboost.readthedocs.io/en/stable/parameter.html>
- XGBoost R global configuration, seção `xgb.set.config()` / OpenMP: <https://xgboost.readthedocs.io/en/release_3.0.0/r_docs/R-package/docs/reference/xgbConfig.html>

## Variáveis com maior chance de afetar XGBoost

| Variável | Configuração atual | Impacto provável no XGBoost | Recomendação |
| --- | ---: | --- | --- |
| `OMP_NUM_THREADS` | `4` | Limita o paralelismo OpenMP. É o item mais provável para explicar treino lento quando `nthread` não é passado ao XGBoost. | Para treino isolado, testar `nthread = 24`, `32` e `48` no `xgb.train()`/`xgboost()` ou ajustar `OMP_NUM_THREADS` antes de iniciar o R. |
| `OMP_DYNAMIC` | `FALSE` | Impede ajuste dinâmico do número de threads. Pode ser bom para reprodutibilidade, mas torna `OMP_NUM_THREADS=4` um teto rígido. | Manter `FALSE` se `nthread` for explicitamente calibrado. |
| `OMP_PROC_BIND` | `close` | Mantém threads próximas. Pode ajudar cache, mas em servidor NUMA com dois sockets pode concentrar carga em poucos núcleos/socket se o runtime interpretar afinidade de forma restritiva. | Benchmarkar `close` versus `spread`, especialmente para datasets grandes. |
| `OMP_PLACES` | `cores` | Define afinidade em núcleos. Em geral é razoável, mas interage com `OMP_PROC_BIND`. | Manter durante benchmark; testar sem afinidade se houver comportamento anormal. |
| `OMP_WAIT_POLICY` | `PASSIVE` | Reduz espera ativa. Economiza CPU em cargas concorrentes, mas pode aumentar latência de sincronização em loops paralelos curtos. | Para treino isolado, testar `ACTIVE` versus `PASSIVE`; para servidor compartilhado, `PASSIVE` é defensável. |
| `OMP_SCHEDULE` | `STATIC` | Pode impactar loops OpenMP que consultam schedule em runtime. XGBoost tende a gerenciar seus próprios loops, então o impacto é menos certo. | Não é primeira suspeita; testar apenas depois de calibrar `nthread`. |
| `R_FUTURE_PLAN` | `sequential` | Não limita o OpenMP interno do XGBoost, mas impede paralelismo externo em pipelines/tuning que dependem de `future`. | Se houver tuning/CV paralelo via `future`, configurar plano explicitamente no pipeline. |
| `MC_CORES` | `4` | Não limita diretamente o OpenMP do XGBoost, mas limita paralelismo por fork em código R que use `parallel::mclapply()`. | Manter se houver vários treinos; aumentar só se o pipeline usar fork com controle de `nthread` por processo. |
| `OPENBLAS_NUM_THREADS`, `MKL_NUM_THREADS`, `BLIS_NUM_THREADS` | `1` | Em geral não é gargalo principal do XGBoost com árvores; evita oversubscription de BLAS durante preprocessamento. | Manter `1` na maioria dos cenários com XGBoost. |
| `R_DATATABLE_NUM_THREADS` | `4` | Não afeta o treinamento XGBoost diretamente, mas pode limitar preparação de dados com `data.table`. | Se preparação de dados dominar tempo, testar 8, 16 ou 24. |
| `TMPDIR` | `~/R_TMP` | Pode afetar pipelines que materializam arquivos temporários, cache ou matriz externa; depende do filesystem. | Garantir que existe, tem espaço e está em storage rápido/local. |

## Itens que chamaram atenção na configuração enviada

1. O bloco inteiro do `.Renviron` aparece duplicado. Em geral, definições duplicadas com o mesmo valor não pioram performance durante o treino; o último valor prevalece. Mesmo assim, a duplicidade aumenta risco de manutenção e divergência futura.
2. `OMP_NUM_THREADS=4` é conservador para um servidor com 48 núcleos físicos. É apropriado se a factory roda muitos jobs simultâneos, mas não se a expectativa é acelerar um único treino XGBoost grande.
3. A combinação `OMP_NUM_THREADS=4`, `R_FUTURE_PLAN=sequential`, `MC_CORES=4`, `RCPP_PARALLEL_NUM_THREADS=4`, `QS_THREADS=4`, `VROOM_THREADS=4` e `R_DATATABLE_NUM_THREADS=4` representa uma política global de baixa concorrência. Ela reduz oversubscription, mas pode transformar o servidor de 48 núcleos em um ambiente efetivo de 4 threads para várias etapas.

## Configurações recomendadas para benchmark controlado

### Cenário A: um único treino XGBoost grande

Use `nthread` explicitamente no XGBoost e não dependa apenas do `.Renviron`:

```r
params <- list(
  objective = "binary:logistic",
  tree_method = "hist",
  nthread = 24
)

fit <- xgboost::xgb.train(
  params = params,
  data = dtrain,
  nrounds = 500,
  watchlist = list(train = dtrain),
  verbose = 1
)
```

Benchmarkar pelo menos `nthread = 4, 8, 16, 24, 32, 48`. Em CPUs com hyperthreading, 96 threads raramente é a melhor primeira escolha para treino de árvores; comece pelos núcleos físicos ou frações deles.

### Cenário B: vários treinos simultâneos no mesmo servidor

Manter `OMP_NUM_THREADS=4` pode ser adequado, mas cada job deve declarar `nthread = 4` para não depender de estado global. Exemplo: 12 jobs simultâneos × 4 threads = 48 threads, aproximadamente os 48 núcleos físicos.

### Cenário C: tuning/CV paralelo

Evite multiplicação explosiva:

- Se o tuning roda 8 workers externos, use `nthread = 4` ou menos por treino.
- Se há um único worker externo, use `nthread` maior.
- Nunca deixe workers externos e XGBoost interno ambos usando todos os núcleos.

## Comandos de diagnóstico no R

```r
Sys.getenv(c(
  "OMP_NUM_THREADS",
  "OMP_DYNAMIC",
  "OMP_PROC_BIND",
  "OMP_PLACES",
  "OMP_WAIT_POLICY",
  "OMP_SCHEDULE",
  "OPENBLAS_NUM_THREADS",
  "MKL_NUM_THREADS",
  "R_FUTURE_PLAN",
  "MC_CORES",
  "R_DATATABLE_NUM_THREADS",
  "TMPDIR"
))

parallel::detectCores(logical = FALSE)
parallel::detectCores(logical = TRUE)

xgboost::xgb.get.config()
```

Também foi adicionado `tools/diagnose_xgboost_env.R`, que imprime o ambiente relevante e, opcionalmente, executa um microbenchmark sintético quando `RUN_XGBOOST_BENCH=true`.

## Checklist específico para validar se o `.Renviron` é o culpado

- [ ] Rodar o mesmo dataset com `nthread = 4` e medir tempo.
- [ ] Rodar o mesmo dataset com `nthread = 8`, `16`, `24`, `32` e `48`.
- [ ] Repetir o melhor caso com `OMP_PROC_BIND=close` e `OMP_PROC_BIND=spread` em sessões R iniciadas separadamente.
- [ ] Confirmar se o pipeline usa `future`, `mclapply`, `foreach` ou outro paralelismo externo; se sim, calcular `workers_externos * nthread_xgboost`.
- [ ] Confirmar se a lentidão está no treino XGBoost ou na preparação de dados antes do treino.
- [ ] Remover duplicidade do `.Renviron` para evitar configurações divergentes em manutenção futura.
