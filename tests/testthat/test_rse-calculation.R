## test relative standard error functions -------------
library(testthat)
# test prop_rse() -------------------------
## test that output is a data frame ----
test_that("prop_rse", {

    sd = 25
    n = 1000
    method = "sample"

  expect_equal(class(sd), "numeric")
  expect_equal(class(n), "numeric")
  expect_equal(method == "sample", TRUE)
  expect_equal(prop_rse(sd, n, method), 0.790, tolerance=1e-3)

})

test_that("prop_rse", {

      sd = 25
      n = 1000
      method = "survey"

  expect_equal(class(sd), "numeric")
  expect_equal(class(n), "numeric")
  expect_equal(method == "survey", TRUE)
  expect_equal(prop_rse(sd, n, method), 0.079, tolerance=1e-3)

})
