## test life expectancy functions -------------

# test make_life_table() -------------------------
## test that output is a data frame ----
test_that("make_life_table", {
  test_data <- data.frame(
    year = c(rep(2020, 20), rep(2021, 20)),
    age_cat = rep(c("0", "1-4", "5-9", "10-14", "15-19","20-24", "25-29", "30-34", "35-39", "40-44",
                 "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90+"), 2),
    deaths = c(206, 37, 23, 23, 105, 162, 268, 314, 413, 584, 954,
               1359, 1912, 2824, 4507, 5851, 7117, 8192, 7745, 6442,
               232, 30, 41, 22, 194, 168, 315, 313, 406, 643, 963,
               1446, 1979, 2814, 4587, 5874, 7111, 8221, 7825, 6540),
    population = c(50698, 215400, 280458, 258105, 282062, 329060, 306097, 274544, 260415,
                       267450, 311314, 324311, 296825, 271339, 284608, 228062, 162785, 111263, 58987, 26016,
                       51578, 215512, 279462, 257256, 282348, 329111, 306514, 274397, 259847, 267045,
                       311791, 323739, 297453, 271344, 285047, 227655, 162922, 110554, 58886, 26243))

  ## test that output is a data frame ----
  le_table <- make_life_table(test_data, grouping_vars = "year")
  expect_equal("data.frame", class(le_table))

  ## check actual output ---- 2020, 85.1, 2021, 85.2
  le_20 <- round(unlist(le_table[le_table$start_age == 0 & le_table$year == 2020, c("obs_le_int")]), 1)
  le_21 <- round(unlist(le_table[le_table$start_age == 0 & le_table$year == 2021, c("obs_le_int")]), 1)

  expect_equal(le_20, 78.3)
  expect_equal(le_21, 78)

  ## test not including groups ----
  test_data1 <- test_data[test_data$year == 2020,]
  le_table <- make_life_table(test_data1)
  expect_equal("data.frame", class(le_table))

  ## test renaming columns ----
  colnames(test_data) <- c("years", "ages", "deaths", "pop")
  le_table <- make_life_table(test_data, grouping_vars = "years", age_cat_var = "ages", population_var = "pop")
  expect_equal("data.frame", class(le_table))


})

# test get_le() ----------------------------------
test_that("get_le", {
  test_data <- data.frame(
    year = c(rep(2020, 20), rep(2021, 20)),
    age_cat = rep(c("0", "1-4", "5-9", "10-14", "15-19","20-24", "25-29", "30-34", "35-39", "40-44",
                 "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90+"), 2),
    deaths = c(206, 37, 23, 23, 105, 162, 268, 314, 413, 584, 954,
               1359, 1912, 2824, 4507, 5851, 7117, 8192, 7745, 6442,
               232, 30, 41, 22, 194, 168, 315, 313, 406, 643, 963,
               1446, 1979, 2814, 4587, 5874, 7111, 8221, 7825, 6540),
    population = c(50698, 215400, 280458, 258105, 282062, 329060, 306097, 274544, 260415,
                       267450, 311314, 324311, 296825, 271339, 284608, 228062, 162785, 111263, 58987, 26016,
                       51578, 215512, 279462, 257256, 282348, 329111, 306514, 274397, 259847, 267045,
                       311791, 323739, 297453, 271344, 285047, 227655, 162922, 110554, 58886, 26243))
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
  expect_equal(c(49.2, 49.1), round(unlist(le$obs_le_int), 1))

  ## test not including groups ----
  test_data1 <- test_data[test_data$year == 2020,]
  le_table <- make_life_table(test_data1)
  le1 <- get_le(le_table)
  expect_equal(1, nrow(le1))

})

