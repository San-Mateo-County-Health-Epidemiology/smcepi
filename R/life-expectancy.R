#' life table
#'
#' this is the function called in the `make_life_table()`. It calculates the vectors required for creating life expectancy methods outlined in the Public Health England calculator that lives [here](https://fingertips.phe.org.uk/documents/phe%20life%20expectancy%20calculator.xlsm).
#'
#' Some things have been added, including the option for 0-4 ages (instead of 0 and 1-4) and 85+ (instead of 85-89 and 90+)
#'
#' @param x a dataframe
#'
#' @return
#'
#' @examples

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
  num_alive_int = Reduce(f = "*", prob_surv, init = 100000, acc = TRUE)[1:nrow(data)]

  num_dying_int = as.numeric(ifelse(max_age == 1, num_alive_int, num_alive_int - c(num_alive_int[-1], 0)))
  pers_yrs_lived_int = ifelse(max_age == 1, num_alive_int/death_rate,
                              int_width*(lead(num_alive_int)+(fract_surv*num_dying_int)))

  pers_yrs_lived_past = rev(cumsum(rev(pers_yrs_lived_int)))

  obs_le_int = pers_yrs_lived_past/num_alive_int

  sample_var = ifelse(max_age == 1, (death_rate*(1-death_rate)/unlist(data[, c("population")])),
                      ifelse(unlist(data[, c("deaths")]) == 0, 0,
                             (prob_dying^2*(1-prob_dying)/unlist(data[, c("deaths")]))))

  weighted_var = ifelse(max_age == 1, (num_alive_int^2)/death_rate^4*sample_var,
                        (num_alive_int^2)*((1-fract_surv)*int_width+lead(obs_le_int))^2*sample_var)

  sample_var_pers_yrs = rev(cumsum(rev(weighted_var)))
  sample_var_obs_le = sample_var_pers_yrs/num_alive_int^2
  ci_low_95 = round(obs_le_int-(1.96*sqrt(sample_var_obs_le)), 1)
  ci_high_95 = round(obs_le_int+(1.96*sqrt(sample_var_obs_le)), 1)

  # bind all of the vectors together ----
  new_data <- cbind(data, start_age, max_age, int_width, fract_surv, death_rate, prob_dying, prob_surv,
                    num_alive_int, num_dying_int, pers_yrs_lived_int, pers_yrs_lived_past,
                    obs_le_int, sample_var, weighted_var, sample_var_pers_yrs, sample_var_obs_le, ci_low_95, ci_high_95)

  return(new_data)

}

#' Title
#'
#' @param data
#' @param year_col
#' @param age_cat_col
#' @param deaths_col
#' @param population_col
#'
#' @return
#' @export
#'
#' @examples
#'
#'
make_life_table <- function(data, group_cols = NULL, age_cat_col = "age_cat", deaths_col = "deaths", population_col = "population") {

  # update variables with user specified names
  names(data)[names(data) == age_cat_col] <- 'age_cat'
  names(data)[names(data) == deaths_col] <- 'deaths'
  names(data)[names(data) == population_col] <- 'population'

  # check that all of our columns are in the data set
  stopifnot(all(c("age_cat", "deaths", "population") %in% colnames(data)))

  # create life table with specified groups
  test_data <- do.call(rbind, by(data, data[, c(group_cols)], life_table))

}

