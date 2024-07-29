# test smc_city_clean() -------------------------
test_that("gg_color_title", {

  test_df <- data.frame(
    city = c("S S Francisco", "South San Fransico", "SoSan Franc")
  ) %>%
    smc_city_clean(city_col = "city")

  expect_equal(3, ncol(test_df))
  expect_equal(c("city_orig", "city_temp", "city_clean"), colnames(test_df))
  expect_equal("South San Francisco", unique(test_df$city_clean))
})
