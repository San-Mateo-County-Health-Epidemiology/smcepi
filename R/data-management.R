#' Remove all empty columns on the right end of a data set.
#'
#' @description
#' This function will drop empty columns at the far right of a data set. This is useful when you import data and get empty columns on the right that shouldn't be there. This function only removes empty columns on the right - it doesn't remove empty columns anywhere else.
#'
#' @usage remove_right_empty_cols(data)
#'
#' @param data a data frame
#'
#' @return the inputted data frame without empty columns on the right
#' @export
#'
#' @examples
#'
#' data <- data.frame(
#'   col1 = sample(1:100, 100, replace = TRUE),
#'   col2 = NA,
#'   col3 = runif(100),
#'   col4 = sample(c("blue", "green", "yellow", "orange"), 100, replace = TRUE),
#'   col5 = NA,
#'   col6 = NA)
#'
#' remove_right_empty_cols(data)
#'  # the function drops `col5` and `col6` but keeps `col3`:
#' head(data)
#'
#'
remove_right_empty_cols <- function(data) {

  stopifnot(sum(class(data) %in% c("data.frame", "tibble")) >= 1)

  # loop through columns starting at the right
  for (i in ncol(data):1) {

    # pull last variable into a vector
    var <- data[, i]

    # count number of non missing values
    val_ct <- length(which(!is.na(var)))

    # if there are 0 non missing values, drop that column
    if (val_ct == 0) {

      data <- data[, 1:(ncol(data)-1)]

    } else {

      break

    }

  }

  data

}
