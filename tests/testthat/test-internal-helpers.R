test_that("internal validators protect scale, seed and integer parameters", {
  expect_equal(instenginer:::sby_validate_seed(42L), 42L)
  expect_error(instenginer:::sby_validate_seed(42.5), "sby_seed")
  expect_equal(instenginer:::sby_validate_positive_integer_scalar(2L, "k"), 2L)
  expect_error(instenginer:::sby_validate_positive_integer_scalar(2.5, "k"), "k")

  expect_true(instenginer:::sby_validate_scaling_info(
    list(centers = c(0, 1), scales = c(1, 2)),
    2L
  ))
  expect_error(instenginer:::sby_validate_scaling_info(
    list(centers = c(0, 1), scales = c(1, 0)),
    2L
  ), "scales")
})

test_that("L2 normalization preserves zero rows and normalizes non-zero rows", {
  sby_x <- matrix(c(0, 0, 3, 4), nrow = 2, byrow = TRUE)
  sby_out <- instenginer:::sby_normalize_l2(sby_x)

  expect_equal(sby_out[1L, ], c(0, 0))
  expect_equal(sqrt(sum(sby_out[2L, ]^2)), 1)
})
