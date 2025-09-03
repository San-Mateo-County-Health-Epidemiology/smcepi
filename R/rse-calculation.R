#' Relative standard error calculation for sample and survey data
#' @description Use this function to calculate the relative standard error (RSE) for a sample and survey data. The RSE measures the reliability of an estimate by dividing the standard error of the estimate by the estimate. The RSE is displayed as a percentage of the estimate.
#'
#' @param sd This is the standard deviation of the data.
#' @param n This is the sample size of the data.
#' @param method This is the method by which the RSE is calculated.
#'
#' @return The relative standard error of sample or survey data.
#' @export
#'
#' @examples
#' \dontrun {
#'
#' library(smcepi)
#'
#' rse(25, 1050, method = "survey")
#' }

prop_rse <- function(sd, n, method) {

  library(smcepi)

  stopifnot(is.numeric(sd) & is.numeric(n) & !method == "sample|survey")

  if (method == "sample") {

    rse <- sd/sqrt(n)

  } else if (method == "survey") {

    se <- sd/sqrt(n)

    rse <- 100 * (se/n)

  }

  return(rse)

}
