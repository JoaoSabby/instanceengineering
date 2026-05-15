#!/usr/bin/env Rscript

# Diagnostico de ambiente para investigar impacto do .Renviron no XGBoost.
# Uso:
#   Rscript tools/diagnose_xgboost_env.R
#   RUN_XGBOOST_BENCH=true XGB_NTHREAD=24 Rscript tools/diagnose_xgboost_env.R

cat("# Diagnostico de ambiente XGBoost\n\n")

vars <- c(
  "OMP_NUM_THREADS",
  "OMP_DYNAMIC",
  "OMP_PROC_BIND",
  "OMP_PLACES",
  "OMP_WAIT_POLICY",
  "OMP_SCHEDULE",
  "RCPP_PARALLEL_NUM_THREADS",
  "OPENBLAS_NUM_THREADS",
  "MKL_NUM_THREADS",
  "BLIS_NUM_THREADS",
  "VECLIB_MAXIMUM_THREADS",
  "R_FUTURE_PLAN",
  "MC_CORES",
  "R_DATATABLE_NUM_THREADS",
  "R_DATATABLE_NUM_PROCS_PERCENT",
  "TMPDIR"
)

print(Sys.getenv(vars, unset = NA_character_))

cat("\n# Cores detectados\n")
print(c(
  physical = parallel::detectCores(logical = FALSE),
  logical = parallel::detectCores(logical = TRUE)
))

if(!requireNamespace("xgboost", quietly = TRUE)){
  cat("\nPacote xgboost nao esta instalado; benchmark nao executado.\n")
  quit(status = 0)
}

cat("\n# Configuracao global XGBoost\n")
print(xgboost::xgb.get.config())

run_bench <- identical(tolower(Sys.getenv("RUN_XGBOOST_BENCH", unset = "false")), "true")
if(!run_bench){
  cat("\nDefina RUN_XGBOOST_BENCH=true para executar benchmark sintetico.\n")
  quit(status = 0)
}

nthread <- as.integer(Sys.getenv("XGB_NTHREAD", unset = "4"))
if(is.na(nthread) || nthread < 1L){
  stop("XGB_NTHREAD deve ser inteiro positivo")
}

set.seed(20260512)
rows <- as.integer(Sys.getenv("XGB_BENCH_ROWS", unset = "200000"))
cols <- as.integer(Sys.getenv("XGB_BENCH_COLS", unset = "50"))
rounds <- as.integer(Sys.getenv("XGB_BENCH_ROUNDS", unset = "100"))

cat("\n# Benchmark sintetico\n")
cat("rows=", rows, " cols=", cols, " rounds=", rounds, " nthread=", nthread, "\n", sep = "")

x <- matrix(stats::rnorm(rows * cols), nrow = rows, ncol = cols)
y <- stats::rbinom(rows, size = 1L, prob = 0.5)
dtrain <- xgboost::xgb.DMatrix(data = x, label = y)

params <- list(
  objective = "binary:logistic",
  eval_metric = "logloss",
  tree_method = "hist",
  nthread = nthread
)

print(system.time({
  model <- xgboost::xgb.train(
    params = params,
    data = dtrain,
    nrounds = rounds,
    verbose = 0
  )
}))

cat("\nBenchmark concluido; modelo treinado com ", rounds, " iteracoes solicitadas.\n", sep = "")
