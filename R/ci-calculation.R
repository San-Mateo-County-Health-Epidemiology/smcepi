#' Confidence interval for a proportion
#' @description Use this function to calculate the lower and upper 95% confidence intervals for a proportion (e.g. rate) using the normal approximation method. The confidence interval estimates a range of values that capture the true population value for a variable of interest. For a 95% confidence interval, if a rate of 5.5 falls within a 95% confidence interval of 4.5, 6.5, it means that one is 95% confidence that the true value from the population falls within that range.
#' @param num This is the numerator of the proportion.
#' @param denom This is the denominator of the proportion.
#'
#' @return The lower and upper bounds of the 95% confidence interval for a given proportion.
#' @export
#'
#' @examples
#'
#' ci(5, 25)
#'
#'
ci <- function(prop_sd, denom) {

  stopifnot(is.numeric(prop_sd) & is.numeric(denom))

  prop <- num / denom

  me <- 1.96*prop_sd

  ci_lower <- round(prop - me, digits = 3)
  ci_upper <- round(prop + me, digits = 3)

  combined_ci <- paste(ci_lower, ci_upper, sep = ", ")

  return(combined_ci)

}

