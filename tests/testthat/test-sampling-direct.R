test_that("sby_adasyn returns a tibble and audit metadata", {
  skip_if_not_installed("FNN")

  sby_data <- data.frame(
    x1 = c(0.0, 0.2, 0.4, 1.0, 1.2, 1.4, 1.6, 1.8, 2.0),
    x2 = c(1.0, 1.3, 1.6, 2.0, 2.4, 2.8, 3.2, 3.6, 4.0),
    y = factor(c("min", "min", "min", "maj", "maj", "maj", "maj", "maj", "maj"))
  )

  sby_result <- sby_adasyn(
    y ~ .,
    sby_data,
    sby_over_ratio = 0.2,
    sby_knn_over_k = 1L,
    sby_seed = 123L,
    sby_knn_engine = "FNN",
    sby_knn_algorithm = "brute"
  )

  expect_s3_class(sby_result, "tbl_df")
  expect_named(sby_result, c("x1", "x2", "TARGET"))
  expect_equal(nrow(sby_result), nrow(sby_data) + 1L)

  sby_audit <- sby_adasyn(
    y ~ .,
    sby_data,
    sby_over_ratio = 0.2,
    sby_knn_over_k = 1L,
    sby_seed = 123L,
    sby_audit = TRUE,
    sby_knn_engine = "FNN",
    sby_knn_algorithm = "brute"
  )

  expect_type(sby_audit, "list")
  expect_s3_class(sby_audit$sby_balanced_data, "tbl_df")
  expect_equal(sby_audit$sby_diagnostics$sby_generated_rows, 1L)
})

test_that("sby_nearmiss and sby_adanear return balanced tabular output", {
  skip_if_not_installed("FNN")

  sby_data <- data.frame(
    x1 = c(0.0, 0.2, 0.4, 1.0, 1.2, 1.4, 1.6, 1.8, 2.0),
    x2 = c(1.0, 1.3, 1.6, 2.0, 2.4, 2.8, 3.2, 3.6, 4.0),
    y = factor(c("min", "min", "min", "maj", "maj", "maj", "maj", "maj", "maj"))
  )

  sby_under <- sby_nearmiss(
    y ~ .,
    sby_data,
    sby_under_ratio = 1,
    sby_knn_under_k = 1L,
    sby_seed = 123L,
    sby_knn_engine = "FNN",
    sby_knn_algorithm = "brute"
  )

  expect_s3_class(sby_under, "tbl_df")
  expect_equal(nrow(sby_under), 6L)

  sby_hybrid <- sby_adanear(
    y ~ .,
    sby_data,
    sby_over_ratio = 0.5,
    sby_under_ratio = 1,
    sby_knn_over_k = 1L,
    sby_knn_under_k = 1L,
    sby_seed = 123L,
    sby_knn_engine = "FNN",
    sby_knn_algorithm = "brute"
  )

  expect_s3_class(sby_hybrid, "tbl_df")
  expect_named(sby_hybrid, c("x1", "x2", "TARGET"))
})

test_that("input validation rejects unsafe numeric inputs", {
  skip_if_not_installed("FNN")

  sby_data <- data.frame(
    x1 = c(0.0, 0.2, 0.4, 1.0, 1.2, 1.4),
    x2 = c(1.0, 1.3, 1.6, 2.0, 2.4, 2.8),
    y = factor(c("min", "min", "maj", "maj", "maj", "maj"))
  )

  expect_error(
    sby_adasyn(y ~ ., sby_data, sby_over_ratio = 0, sby_knn_over_k = 1L),
    "sby_over_ratio"
  )
  expect_error(
    sby_adasyn(y ~ ., sby_data, sby_over_ratio = 0.5, sby_knn_over_k = 1.5),
    "sby_knn_over_k"
  )
  expect_error(
    sby_adasyn(y ~ ., sby_data, sby_over_ratio = 0.5, sby_knn_over_k = 1L, sby_seed = 1.5),
    "sby_seed"
  )

  sby_data_inf <- sby_data
  sby_data_inf$x1[[1L]] <- Inf
  expect_error(
    sby_adasyn(y ~ ., sby_data_inf, sby_over_ratio = 0.5, sby_knn_over_k = 1L),
    "Inf"
  )

  expect_error(
    sby_adasyn(y ~ log(x1) + x2, sby_data, sby_over_ratio = 0.5, sby_knn_over_k = 1L),
    "colunas existentes"
  )
})
