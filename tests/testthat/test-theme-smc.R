# test theme_gg_smc() ---------------------
## make sure it's a theme ----
test_that("theme smc is a theme", {
  theme <- theme_gg_smc()
  expect_s3_class(theme, "theme")
})

## test orientation ----
### horizontal ----
test_that("theme_gg_smc orientation", {
  theme <- theme_gg_smc(plot_lines = "horizontal")
  expect_equal(theme$panel.grid.major.x, element_blank())
  expect_equal(class(theme$panel.grid.major.y), c('element_line', 'element'))
})

### vertical ----
test_that("theme_gg_smc orientation", {
  theme <- theme_gg_smc(plot_lines = "vertical")
  expect_equal(class(theme$panel.grid.major.x), c('element_line', 'element'))
  expect_equal(theme$panel.grid.major.y, element_blank())
})

### both ----
test_that("theme_gg_smc orientation", {
  theme <- theme_gg_smc(plot_lines = "both")
  expect_equal(class(theme$panel.grid.major.x), c('element_line', 'element'))
  expect_equal(class(theme$panel.grid.major.y), c('element_line', 'element'))
})

### none ----
test_that("theme_gg_smc orientation", {
  theme <- theme_gg_smc(plot_lines = "none")
  expect_equal(theme$panel.grid.major.x, element_blank())
  expect_equal(theme$panel.grid.major.y, element_blank())
})

## test legend ----
test_that("theme_gg_smc legend default", {
  theme <- theme_gg_smc()
  expect_equal(theme$legend.position, "top")
})

test_that("theme_gg_smc legend bottom", {
  theme <- theme_gg_smc(legend_loc = "bottom")
  expect_equal(theme$legend.position, "bottom")
})

# test theme_ft_smc() -------------------------

# test gg_color_title() -------------------------
test_that("gg_color_title", {
  title <- gg_color_title("Blue and yellow colors", c("Blue", "yellow"), c("#006cb6", "#fcee00"))
  expect_equal(unlist(stringr::str_extract_all(title, "(?<=color\\:)[\\#A-Za-z0-9]{7}")), c("#006cb6", "#fcee00"))
  expect_equal(unlist(stringr::str_extract_all(title, "(?<=\\'\\>)[A-Za-z\\s]+(?=\\<\\/)")), c("Blue", "yellow"))
  expect_equal(unlist(stringr::str_extract_all(title, "(?<=span\\>)[A-Za-z\\s]+")), c(" and ", " colors"))
})
