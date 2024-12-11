## test life expectancy functions -------------

# test make_life_table() -------------------------
## test that output is a data frame ----
test_that("make_life_table", {
  test_data <- data.frame(
    year = c(rep(2020, 20), rep(2021, 20)),
    age_cat = rep(c("0", "1-4", "5-9", "10-14", "15-19","20-24", "25-29", "30-34", "35-39", "40-44",
                 "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90+"), 2),
    deaths = c(101, 10, 16, 18, 65, 102, 164, 218, 268, 301, 427,
               631, 999, 1414, 1661, 2244, 2583, 3234, 3841, 6546,
               114, 15, 24, 16, 59, 108, 149, 192, 231, 265, 403,
               620, 1010, 1353, 1692, 2083, 2495, 3126, 3812, 6443),
    population = c(37091, 159844, 226038, 232718, 208813, 187469, 219833, 244278, 268280,
                       274061, 284629, 289041, 269557, 257883, 230428, 192485, 139779, 85485, 57412, 38668,
                       38483, 165990, 228426, 231830, 208844, 189191, 219408, 245057, 267273, 274420,
                       288485, 286389, 271027, 254730, 222002, 179366, 126073, 85853, 57803, 38744))

  ## test that output is a data frame ----
  le_table <- make_life_table(test_data, grouping_vars = "year")
  expect_equal("data.frame", class(le_table))

  ## check actual output ---- 2020, 85.1, 2021, 85.2
  le_20 <- round(unlist(le_table[le_table$start_age == 0 & le_table$year == 2020, c("obs_le_int")]), 1)
  le_21 <- round(unlist(le_table[le_table$start_age == 0 & le_table$year == 2021, c("obs_le_int")]), 1)

  expect_equal(le_20, 85.1)
  expect_equal(le_21, 85.2)

  ## test not including groups ----
  test_data1 <- test_data[test_data$year == 2020,]
  le_table <- make_life_table(test_data1)
  expect_equal("data.frame", class(le_table))

  ## test renaming columns ----
  colnames(test_data) <- c("years", "ages", "deaths", "pop")
  le_table <- make_life_table(test_data, grouping_vars = "years", age_cat_col = "ages", population_col = "pop")
  expect_equal("data.frame", class(le_table))


})

# test get_le() ----------------------------------
test_that("get_le", {
  test_data <- data.frame(
    year = c(rep(2020, 20), rep(2021, 20)),
    age_cat = rep(c("0", "1-4", "5-9", "10-14", "15-19","20-24", "25-29", "30-34", "35-39", "40-44",
                    "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90+"), 2),
    deaths = c(101, 10, 16, 18, 65, 102, 164, 218, 268, 301, 427,
               631, 999, 1414, 1661, 2244, 2583, 3234, 3841, 6546,
               114, 15, 24, 16, 59, 108, 149, 192, 231, 265, 403,
               620, 1010, 1353, 1692, 2083, 2495, 3126, 3812, 6443),
    population = c(37091, 159844, 226038, 232718, 208813, 187469, 219833, 244278, 268280,
                   274061, 284629, 289041, 269557, 257883, 230428, 192485, 139779, 85485, 57412, 38668,
                   38483, 165990, 228426, 231830, 208844, 189191, 219408, 245057, 267273, 274420,
                   288485, 286389, 271027, 254730, 222002, 179366, 126073, 85853, 57803, 38744))
  le_table <- make_life_table(test_data, grouping_vars = "year")

  ## test that output is a data frame ----
  le <- get_le(le_table, grouping_vars = "year")
  expect_equal("data.frame", class(le_table))

  ## test that default output has ci columns
  expect_equal(c(TRUE, TRUE), c("ci_low_95", "ci_high_95") %in% colnames(le))

  ## test removing the confidence interval
  le <- get_le(le_table, grouping_vars = "year", include_ci = FALSE)
  expect_equal(c(FALSE, FALSE), c("ci_low_95", "ci_high_95") %in% colnames(le))

  ## test different start ages ----
  le <- get_le(le_table, start_age = "30", grouping_vars = "year", include_ci = FALSE)
  expect_equal(c(55.9, 56.0), round(unlist(le$obs_le_int), 1))

  ## test not including groups ----
  test_data1 <- test_data[test_data$year == 2020,]
  le_table <- make_life_table(test_data1)
  le1 <- get_le(le_table)
  expect_equal(1, nrow(le1))

})

