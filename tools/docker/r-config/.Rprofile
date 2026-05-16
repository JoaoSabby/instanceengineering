options(
  repos = c(
    source = Sys.getenv("CRAN", unset = "https://cloud.r-project.org"),
    CRAN = Sys.getenv("CRAN", unset = "https://cloud.r-project.org")
  ),
  encoding = "UTF-8",
  expressions = 50000L,
  pkgType = "source",
  papersize = "a4",
  TZ = Sys.getenv("TZ", unset = "America/Sao_Paulo"),
  dbi.fetch.size = 50000L,
  Ncpus = 2L,
  future.plan = Sys.getenv("R_FUTURE_PLAN", unset = "sequential"),
  mc.cores = 2L,
  future.globals.maxSize = 8L * 1024L^3L,
  future.rng.onMisuse = "ignore",
  future.globals.onReference = "error",
  yardstick.event_first = TRUE,
  tidymodels.quiet = TRUE,
  tidyverse.quiet = TRUE,
  dplyr.summarise.inform = FALSE,
  dbplyr.summarise.inform = FALSE,
  tibble.print_max = 50L,
  tibble.print_min = 10L,
  lubridate.verbose = FALSE,
  duckplyr_fallback_collect = FALSE,
  scipen = 999L,
  digits = 4L,
  OutDec = ".",
  pillar.sigfig = 4L,
  width = 160L,
  conflicts.policy = list(
    error = FALSE,
    warn = FALSE
  ),
  jit.level = 3L,
  stringsAsFactors = FALSE
)

compiler::enableJIT(3L)

invisible(TRUE)
