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

## test fonts ----
### default ----
test_that("theme_gg_smc legend bottom", {
  theme <- theme_gg_smc()
  expect_equal(theme$plot.title$size, 18)
  expect_equal(theme$plot.subtitle$size, 12)
  expect_equal(theme$plot.caption$size, 8)
  expect_equal(theme$axis.title$size, 10)
  expect_equal(theme$axis.text$size, 10)
})

### adjusting sizes ----
test_that("theme_gg_smc legend bottom", {
  theme <- theme_gg_smc(title_font_size = 22, subtitle_font_size = 20, caption_font_size = 16, axis_font_size = 14)
  expect_equal(theme$plot.title$size, 22)
  expect_equal(theme$plot.subtitle$size, 20)
  expect_equal(theme$plot.caption$size, 16)
  expect_equal(theme$axis.title$size, 14)
  expect_equal(theme$axis.text$size, 14)
})

### invalid sizes ----
test_that("theme_gg_smc legend bottom", {
  theme <- theme_gg_smc(title_font_size = 200)
  expect_equal(theme$plot.title$size, 60)
})

test_that("theme_gg_smc legend bottom", {
  theme <- theme_gg_smc(title_font_size = 4)
  expect_equal(theme$plot.title$size, 6)
})

# test theme_ft_smc() -------------------------

