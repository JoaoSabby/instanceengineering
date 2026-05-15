test_that("recipes steps prep, bake, tidy, print and required_pkgs work", {
  skip_if_not_installed("recipes")
  skip_if_not_installed("FNN")

  sby_data <- data.frame(
    x1 = c(0.0, 0.2, 0.4, 1.0, 1.2, 1.4, 1.6, 1.8, 2.0),
    x2 = c(1.0, 1.3, 1.6, 2.0, 2.4, 2.8, 3.2, 3.6, 4.0),
    y = factor(c("min", "min", "min", "maj", "maj", "maj", "maj", "maj", "maj"))
  )

  sby_recipe <- recipes::recipe(y ~ ., data = sby_data)
  sby_recipe <- sby_step_adasyn(
    sby_recipe,
    y,
    sby_over_ratio = 0.2,
    sby_knn_over_k = 1L,
    sby_seed = 123L,
    sby_knn_engine = "FNN",
    sby_knn_algorithm = "brute"
  )

  sby_prepped <- recipes::prep(sby_recipe, training = sby_data, retain = TRUE)
  sby_baked <- recipes::bake(sby_prepped, new_data = NULL)

  expect_s3_class(sby_baked, "tbl_df")
  expect_equal(nrow(sby_baked), nrow(sby_data) + 1L)
  expect_s3_class(generics::tidy(sby_prepped, number = 1L), "data.frame")
  sby_printed <- utils::capture.output(print(sby_prepped$steps[[1L]]))
  expect_gt(length(sby_printed), 0L)
  expect_true(all(c("instenginer", "recipes", "FNN") %in% instenginer:::required_pkgs.step_sby_step_adasyn()))
})
