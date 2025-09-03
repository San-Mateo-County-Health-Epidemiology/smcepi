## test relative standard error functions -------------
library(testthat)
# test prop_rse() -------------------------
## test that output is a data frame ----
test_that("prop_rse", {

  rse_df_sample <- data.frame(
    sd = 25,
    n = 1000,
    method = "sample")

  expect_equal(class(rse_df_sample$sd), "numeric")
  expect_equal(class(rse_df_sample$n), "numeric")
  expect_equal(rse_df_sample$method == "sample", TRUE)
  expect_equal(prop_rse(rse_df_sample$sd, rse_df_sample$n, rse_df_sample$method), 0.790, tolerance=1e-3)

})

test_that("prop_rse", {

    rse_df_survey <- data.frame(
      sd = 25,
      n = 1000,
      method = "survey")

  expect_equal(class(rse_df_survey$sd), "numeric")
  expect_equal(class(rse_df_survey$n), "numeric")
  expect_equal(rse_df_survey$method == "survey", TRUE)
  expect_equal(prop_rse(rse_df_survey$sd, rse_df_survey$n, rse_df_survey$method), 0.079, tolerance=1e-3)

})
