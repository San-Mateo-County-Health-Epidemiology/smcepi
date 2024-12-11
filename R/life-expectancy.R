#' life table
#'
#' this is the function called in the `make_life_table()`. It calculates the vectors required for creating life expectancy methods outlined in the Public Health England calculator that lives [here](https://fingertips.phe.org.uk/documents/phe%20life%20expectancy%20calculator.xlsm).
#'
#' Some things have been added, including the option for 0-4 ages (instead of 0 and 1-4) and 85+ (instead of 85-89 and 90+)
#'
#' @param data a data frame
#'
#' @return a data frame
#'
#' @examples
#' # don't run this on its own - run make_life_table()

life_table <- function(data) {

  # arrange data ----
  start_age = as.numeric(sub("[\\s\\-]+\\d{1,2}$|\\+$", "", data$age_cat))
  data <- data[order(start_age),]

  # make vectors ----
  max_age = ifelse(max(start_age) == start_age, 1, 0)

  int_width = as.numeric(ifelse(data[, c("age_cat")] == "0", 1,
                                ifelse(data[, c("age_cat")] == "1-4", 4,
                                       ifelse(data[, c("age_cat")] == "0-4", 5,
                                              ifelse(data[, c("age_cat")] == "85+", 17.3,
                                                     ifelse(data[, c("age_cat")] == "90+", 12.3, 5))))))
  fract_surv = as.numeric(ifelse(data[, c("age_cat")] == "0", 0.1,
                                 ifelse(data[, c("age_cat")] == "0-4", 0.02, 0.5)))

  death_rate = unlist(data[, c("deaths")]/data[, c("population")])
  prob_dying = int_width*death_rate/(1+int_width*(1-fract_surv)*death_rate)
  prob_surv = 1-prob_dying
  num_alive_int = Reduce(f = "*", prob_surv, init = 100000, accumulate = TRUE)[1:nrow(data)]

  num_dying_int = as.numeric(ifelse(max_age == 1, num_alive_int, num_alive_int - c(num_alive_int[-1], 0)))
  pers_yrs_lived_int = ifelse(max_age == 1, num_alive_int/death_rate,
                              int_width*(c(num_alive_int[-1], 0)+(fract_surv*num_dying_int)))

  pers_yrs_lived_past = rev(cumsum(rev(pers_yrs_lived_int)))

  obs_le_int = pers_yrs_lived_past/num_alive_int

  sample_var = ifelse(max_age == 1, (death_rate*(1-death_rate)/unlist(data[, c("population")])),
                      ifelse(unlist(data[, c("deaths")]) == 0, 0,
                             (prob_dying^2*(1-prob_dying)/unlist(data[, c("deaths")]))))

  weighted_var = ifelse(max_age == 1, (num_alive_int^2)/death_rate^4*sample_var,
                        (num_alive_int^2)*((1-fract_surv)*int_width+c(obs_le_int[-1], 0))^2*sample_var)

  sample_var_pers_yrs = rev(cumsum(rev(weighted_var)))
  sample_var_obs_le = sample_var_pers_yrs/num_alive_int^2
  ci_low_95 = round(obs_le_int-(1.96*sqrt(sample_var_obs_le)), 1)
  ci_high_95 = round(obs_le_int+(1.96*sqrt(sample_var_obs_le)), 1)

  # bind all of the vectors together ----
  new_data <- cbind(data, start_age, int_width, fract_surv, death_rate, prob_dying, prob_surv,
                    num_alive_int, num_dying_int, pers_yrs_lived_int, pers_yrs_lived_past,
                    obs_le_int, sample_var, weighted_var, sample_var_pers_yrs, sample_var_obs_le, ci_low_95, ci_high_95)

  return(new_data)

}

life_table_examples <- function(){
  ex <- '
@examples
# step 1: generate data from which to calculate life expectancy
test_data <- data.frame(
  year = c(rep(2020, 20), rep(2021, 20)),
  ages = rep(c("0", "1-4", "5-9", "10-14", "15-19","20-24", "25-29", "30-34", "35-39", "40-44",
  "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90+"), 2),
  deaths = c(101, 10, 16, 18, 65, 102, 164, 218, 268, 301, 427,
             631, 999, 1414, 1661, 2244, 2583, 3234, 3841, 6546,
            114, 15, 24, 16, 59, 108, 149, 192, 231, 265, 403,
             620, 1010, 1353, 1692, 2083, 2495, 3126, 3812, 6443),
  population_est = c(37091, 159844, 226038, 232718, 208813, 187469, 219833, 244278, 268280,
       274061, 284629, 289041, 269557, 257883, 230428, 192485, 139779, 85485, 57412, 38668,
             38483, 165990, 228426, 231830, 208844, 189191, 219408, 245057, 267273, 274420,
             288485, 286389, 271027, 254730, 222002, 179366, 126073, 85853, 57803, 38744))

# step 2: create a life table using the methods from Public Health England .xlsm file
le_table <- make_life_table(data = test_data,
                            grouping_vars = c("year"),
                            age_cat_var = "ages",
                            population_var = "population_est")

# step 3: pull out the life expectancy for each group
## 3a: get le for age 0 (typical estimation used) with confidence intervals
get_le(le_table, grouping_vars = c("year"))

## 3b. get le for the age group starting at 30 without confidence intervals
get_le(le_table, start_age = 30, grouping_vars = c("year"), include_ci = FALSE)

'
  return(strsplit(ex, split = "\n")[[1]]) # needed to have line jumps in the doc
}

#' Creating life tables
#'
#' @description This function creates a life table based on the life table from Public Health England that lives [here](https://fingertips.phe.org.uk/documents/phe%20life%20expectancy%20calculator.xlsm). It is designed to work with grouped data so that you can calculate life tables for multiple groups at once.
#'
#' @usage make_life_table(data,
#'                 grouping_vars = NULL,
#'                 age_cat_var = "age_cat",
#'                 deaths_var = "deaths",
#'                 population_var = "population")
#'
#' @param data a 2x2 data frame with variables for age categories, population years and death count.
#' @param grouping_vars a list of variables used to group the output.
#' @param age_cat_var the name of the variable with age categories if the variable has a name other than "age_cat"
#' @param deaths_var the name of the variable with death counts if the variable has a name other than "deaths"
#' @param population_var the name of the variable with the population years if the variable has a name other than "population"
#'
#' @return a data frame with a variable for each of the columns in the PHE life table
#' @export
#'
#' @eval life_table_examples()
make_life_table <- function(data, grouping_vars = NULL, age_cat_var = "age_cat", deaths_var = "deaths", population_var = "population") {

  # update variables with user specified names
  names(data)[names(data) == age_cat_var] <- 'age_cat'
  names(data)[names(data) == deaths_var] <- 'deaths'
  names(data)[names(data) == population_var] <- 'population'

  # check that all of our columns are in the data set
  stopifnot(all(c("age_cat", "deaths", "population") %in% colnames(data)))

  # create life table with specified groups
  if (is.null(grouping_vars)) {
    data1 <- life_table(data)
  } else {
    data1 <- do.call(rbind, by(data, data[, c(grouping_vars)], life_table))
  }

  rownames(data1) <- NULL
  return(data1)

}


#' Pulling Life Expectancy from a Life Table
#'
#' @description
#' This function is meant to be used on the output from the `make_life_table()` function. It will pull the life expectancy for a given age from the life table.
#'
#' @usage get_le(data,
#'        start_age = 0,
#'        grouping_vars = NULL,
#'        include_ci = TRUE)
#'
#' @param data a data frame generated from the `make_life_table()` function
#' @param start_age an integer for the start age of the age group for which you want life expectancy. Defaults to 0. Possible start ages are 0, 1 and multiples of 5 up to 90 (ie 5, 10, 15, 20...)
#' @param grouping_vars the same list of `grouping_vars` specified in the `make_life_table()` function
#' @param include_ci option to include the columns with the upper and lower 95% confidence interval for the estimate. Defaults to TRUE
#'
#' @return a data frame with life expectancy for specified ages for each specified group
#' @export
#' @eval life_table_examples()

get_le <- function(data, start_age = 0, grouping_vars = NULL, include_ci = TRUE) {

  # ensure the start_age is numeric or can be converted to a numeric
  stopifnot(!is.na(as.numeric(start_age)))

  # set vector of columns to be outputted
  if (include_ci == T) {
    selected_vars <- c("obs_le_int", "ci_low_95", "ci_high_95")
  } else {
    selected_vars <- c("obs_le_int")
  }

  # keep selected output
  le <- data[data$start_age == as.numeric(start_age), c(grouping_vars, selected_vars)]

  return(le)

}
