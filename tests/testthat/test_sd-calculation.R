## test confidence interval function -------------
library(testthat)
# test prop_sd() -------------------------
## test that output is a data frame ----
test_that("prop_sd", {

    num <- 50
  denom <- 2000

  expect_equal(class(sd_df$num), "numeric")
  expect_equal(class(sd_df$denom), "numeric")
  expect_equal(prop_sd(sd_df$num, sd_df$denom), 0.003, tolerance=1e-2)

})
