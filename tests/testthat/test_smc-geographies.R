# test smc_city_clean() -------------------------
test_that("smc_city_clean", {

  test_df <- data.frame(
    city = c("S S Francisco", "South San Fransico", "SoSan Franc", "San Francisco")
  ) %>%
    smc_city_clean(city_col = "city")

  expect_equal(2, ncol(test_df))
  expect_equal(c("city", "city_clean"), colnames(test_df))
  expect_equal(c("South San Francisco", NA), unique(test_df$city_clean))
  expect_equal(NA_character_, test_df[test_df$city == "San Francisco", ]$city_clean)
})


# test smc_zip_region_sort() -------------------------
test_that("smc_zip_region_sort", {

  test_df <- data.frame(
    cty_zip = c("94015", "94403", "94303", "94019", "94128", "94110")
  ) %>%
    smc_zip_region_sort(zip_col = "cty_zip")

  expect_equal(2, ncol(test_df))
  expect_equal(c("cty_zip", "zip_region"), colnames(test_df))
  expect_equal(NA_character_, test_df[test_df$cty_zip == "94110", ]$zip_region)
})

test_that("smc_zip_region_sort", {

  test_df <- data.frame(
    cty_zip = c("94015", "94403", "94303", "94019", "94128", "94110")
  ) %>%
    smc_zip_region_sort(zip_col = "cty_zip",
                        region_col = "cty_zip_region")

  expect_equal(2, ncol(test_df))
  expect_equal(c("cty_zip", "cty_zip_region"), colnames(test_df))
})


