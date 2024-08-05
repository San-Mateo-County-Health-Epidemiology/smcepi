# test smc_city_clean() -------------------------
test_that("smc_city_clean", {

  test_df <- data.frame(
    city = c("S S Francisco", "South San Fransico", "SoSan Franc", "San Francisco")
  ) %>%
    smc_city_clean(city_col = "city")

  expect_equal(3, ncol(test_df))
  expect_equal(c("city_orig", "city_temp", "city_clean"), colnames(test_df))
  expect_equal(c("South San Francisco", NA), unique(test_df$city_clean))
  expect_equal(NA_character_, test_df[test_df$city_orig == "San Francisco", ]$city_clean)
})
