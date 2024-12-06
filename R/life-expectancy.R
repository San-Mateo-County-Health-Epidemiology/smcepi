#' life table
#'
#' this is the function called in the `make_life_table()`.
#'
#' @param x a dataframe
#'
#' @return
#'
#' @examples
life_table <- function(x) {

  start_age = as.numeric(sub("[\\s\\-]+\\d{1,2}$|\\+$", "", x[, c("age_cat")]))
  max_age = ifelse(max(start_age) == start_age, 1, 0)

  int_width = ifelse(x[, c("age_cat")] == "0", 1,
                     ifelse(x[, c("age_cat")] == "1-4", 4,
                            ifelse(x[, c("age_cat")] == "0-4", 5,
                                   ifelse(x[, c("age_cat")] == "85+", 17.3,
                                          ifelse(x[, c("age_cat")] == "90+", 12.3, 5)))))
  fract_surv = ifelse(x[, c("age_cat")] == "0", 0.1,
                      ifelse(x[, c("age_cat")] == "0-4", 0.02, 0.5))

  death_rate = x[, c("deaths")]/x[, c("population")]
  prob_dying = int_width*death_rate/(1+int_width*(1-fract_surv)*death_rate)
  prob_surv = 1-prob_dying
  num_alive_int = Reduce(sumprod, prob_surv, init = 100000, acc = TRUE)[1:nrow(x)]
  num_dying_int = ifelse(max_age == 1, num_alive_int, num_alive_int - c(num_alive_int[-1], 0))

  pers_yrs_lived_int = ifelse(max_age == 1, num_alive_int/death_rate,
                              int_width*(lead(num_alive_int)+(fract_surv*num_dying_int)))

  pers_yrs_lived_past = rev(cumsum(rev(pers_yrs_lived_int)))

  obs_le_int = pers_yrs_lived_past/num_alive_int

  sample_var = ifelse(max_age == 1, (death_rate*(1-death_rate)/x[, c("population")]),
                      ifelse(x[, c("deaths")] == 0, 0, (prob_dying^2*(1-prob_dying)/x[, c("deaths")])))
  weighted_var = ifelse(max_age == 1, (num_alive_int^2)/death_rate^4*sample_var,
                        (num_alive_int^2)*((1-fract_surv)*int_width+lead(obs_le_int))^2*sample_var)

  sample_var_pers_yrs = rev(cumsum(rev(weighted_var)))
  sample_var_obs_le = sample_var_pers_yrs/num_alive_int^2
  ci_low_95 = round(obs_le_int-(1.96*sqrt(sample_var_obs_le)), 1)
  ci_high_95 = round(obs_le_int+(1.96*sqrt(sample_var_obs_le)), 1)


  cbind(x, start_age, max_age, int_width, fract_surv, death_rate, prob_dying, prob_surv,
        num_alive_int, num_dying_int, pers_yrs_lived_int, pers_yrs_lived_past,
        obs_le_int, sample_var, weighted_var, sample_var_pers_yrs, sample_var_obs_le, ci_low_95, ci_high_95)

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
make_life_table <- function(data, year_col = "year", age_cat_col = "age_cat", deaths_col = "deaths", population_col = "population") {

  # allow user to specify names of variables
  names(df)[names(df) == year_col] <- 'year'

  # check that all of our columns are in the data set
  stopifnot(all(c("year", "age_cat", "deaths", "population") %in% colnames(data)))

  test_data <- do.call(rbind, by(test_data, test_data[, c("year")], make_life_table))

}

