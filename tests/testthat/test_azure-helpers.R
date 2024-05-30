# test azure helper functions ------------------
test_that("varchar_max return", {

  data <- data.frame(col1 = c("blue", "pink", "green", "yellow"),
                     col2 = c(paste(rep("a", 1000), collapse = ""), "b", "cc", "ddd"))
  varchar_test <- varchar_max(data)

  expect_equal(length(varchar_test), 2)
  expect_equal(unname(varchar_test), c("varchar(255)", "varchar(1000)"))
  expect_equal(names(varchar_test), c("col1", "col2"))

})

