#' Relative standard error calculation for sample and survey data
#' @description Use this function to calculate the relative standard error (RSE) for a sample and survey data. The RSE measures the reliability of an estimate by dividing the standard error of the estimate by the estimate. The RSE is displayed as a percentage of the estimate.
#'
#' @param prop_sd This is the standard deviation of the proportion.
#' @param n This is the sample size of the data.
#' @param method This is the method by which the RSE is calculated.
#'  * `"sample"`: used to calculate the RSE of surveillance data sets
#'  * `"survey"`: utilized to calculate the RSE of data from a survey or a limited data set from a larger population.
#' @return The relative standard error of sample or survey data.
#' @export
#'
#' @examples
#' \dontrun{
#'
#' prop_rse(25, 1050, method = "survey")
#'}

prop_rse <- function(prop_sd, n, method) {

  stopifnot(is.numeric(prop_sd) & is.numeric(n))
  method <- rlang::arg_match(method, c("sample", "survey"))

  if (method == "sample") {

    rse <- prop_sd/sqrt(n)

 } else if (method == "survey") {

    se <- prop_sd/sqrt(n)

    rse <- 100 * (se/n)

  }

  return(rse)

}
