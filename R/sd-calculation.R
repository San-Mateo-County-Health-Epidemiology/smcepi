#' Standard deviation calculation for a proportion
#' @description Use this function to calculate the standard deviation of a proportion (e.g. rate). The standard deviation is a measure of spread that represents how far the population proportion is from the sample proportions on average.
#'
#' @param num This is the numerator of the proportion.
#' @param denom This is the denominator of the proportion.
#'
#' @return The standard deviation of a proportion.
#' @export
#'
#' @examples
#' prop_sd(5,20)
#'
prop_sd <- function(num, denom) {

  stopifnot(is.numeric(num) & is.numeric(denom))

  prop <- num / denom

  sd <- sqrt(prop*(1-prop)/denom)

  return(sd)

}
