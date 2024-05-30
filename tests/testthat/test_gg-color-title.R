
# test gg_color_title() -------------------------
test_that("gg_color_title", {
  title <- gg_color_title("Blue and yellow colors", c("Blue", "yellow"), c("#006cb6", "#fcee00"))
  expect_equal(unlist(stringr::str_extract_all(title, "(?<=color\\:)[\\#A-Za-z0-9]{7}")), c("#006cb6", "#fcee00"))
  expect_equal(unlist(stringr::str_extract_all(title, "(?<=\\'\\>)[A-Za-z\\s]+(?=\\<\\/)")), c("Blue", "yellow"))
  expect_equal(unlist(stringr::str_extract_all(title, "(?<=span\\>)[A-Za-z\\s]+")), c(" and ", " colors"))
})

test_that("gg_color_title errors", {
  expect_error(gg_color_title("Blue and yellow colors", c("Blue", "Yellow"), c("#006cb6", "#fcee00")))
  expect_error(gg_color_title("Blue and yellow colors", c("Blue", "Yellow"), c("#006cb6", "#fcee00", "#f2f4f4")))
})
