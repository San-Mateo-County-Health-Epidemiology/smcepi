make_life_table <- function(data) {
  life_table <- data %>%
    mutate(int_width = case_when(age_cat == "0" ~ 1,
                                 age_cat == "1-4" ~ 4,
                                 age_cat == "0-4" ~ 5,
                                 age_cat == "85+" ~ 17.3,
                                 age_cat == "90+" ~ 12.3,
                                 TRUE ~ 5),
           fract_surv = case_when(age_cat == "0" ~ 0.1,
                                  age_cat == "0-4" ~ 0.02,
                                  TRUE ~ 0.5),
           death_rate = deaths/population,
           prob_dying = int_width*death_rate/(1+int_width*(1-fract_surv)*death_rate),
           prob_surv = 1-prob_dying,
           num_alive_int = purrr::accumulate(.x = head(prob_surv, -1),
                                      .init = 100000,
                                      .f = `*`),
           num_dying_int = case_when(row_number() == n() ~ num_alive_int,
                                     TRUE ~ num_alive_int - lead(num_alive_int)),
           pers_yrs_lived_int = case_when(row_number() == n() ~ num_alive_int/death_rate,
                                          TRUE ~ int_width*(lead(num_alive_int)+(fract_surv*num_dying_int))),
           pers_yrs_lived_past = purrr::accumulate(.x = pers_yrs_lived_int,
                                            .f = `+`,
                                            .dir = "backward"),
           obs_le_int = pers_yrs_lived_past/num_alive_int,
           sample_var = case_when(row_number() == n() ~ (death_rate*(1-death_rate)/population),
                                  deaths == 0 ~ 0,
                                  TRUE ~ (prob_dying^2*(1-prob_dying)/deaths)),
           weighted_var = case_when(row_number() == n() ~ (num_alive_int^2)/death_rate^4*sample_var,
                                    TRUE ~ (num_alive_int^2)*((1-fract_surv)*int_width+lead(obs_le_int))^2*sample_var),
           sample_var_pers_yrs = purrr::accumulate(.x = weighted_var,
                                            .f = `+`,
                                            .dir = "backward"),
           sample_var_obs_le = sample_var_pers_yrs/num_alive_int^2,
           ci_low_95 = round(obs_le_int-(1.96*sqrt(sample_var_obs_le)), 1),
           ci_high_95 = round(obs_le_int+(1.96*sqrt(sample_var_obs_le)), 1))

  life_table

}

# get start age ----
test_data$start_age <- as.numeric(sub("[\\s\\-]+\\d{1,2}$|\\+$", "", test_data$age_cat))

# set interval widths ----
test_data$int_width <- ifelse(test_data$age_cat == "0", 1,
                              ifelse(test_data$age_cat == "1-4", 4,
                                     ifelse(test_data$age_cat == "0-4", 5,
                                            ifelse(test_data$age_cat == "85+", 17.3,
                                                    ifelse(test_data$age_cat == "90+", 12.3, 5)))))
# set fraction of last age interval survivied ----
test_data$fract_surv <- ifelse(test_data$age_cat == "0", 0.1,
                               ifelse(test_data$age_cat == "0-4", 0.02, 0.5))

# get the death rate ----
test_data$death_rate <- test_data$deaths/test_data$population

# get the probability of dying ----
test_data$prob_dying <- test_data$int_width*test_data$death_rate/(1+test_data$int_width*(1-test_data$fract_surv)*test_data$death_rate)

# get the probability of survival ----
test_data$prob_surv <- 1-test_data$prob_dying

# get the number alive at the start of the interval ----
sumprod <- function(x, y) x * y
test_data$num_alive_int <- Reduce(sumprod, test_data$prob_surv, init = 100000, acc = TRUE)[1:nrow(test_data)]

# get the number dying in the interval ----
num_dying_int = case_when(row_number() == n() ~ num_alive_int,
                          TRUE ~ num_alive_int - lead(num_alive_int)),
pers_yrs_lived_int = case_when(row_number() == n() ~ num_alive_int/death_rate,
                               TRUE ~ int_width*(lead(num_alive_int)+(fract_surv*num_dying_int))),
pers_yrs_lived_past = purrr::accumulate(.x = pers_yrs_lived_int,
                                        .f = `+`,
                                        .dir = "backward"),
obs_le_int = pers_yrs_lived_past/num_alive_int,
sample_var = case_when(row_number() == n() ~ (death_rate*(1-death_rate)/population),
                       deaths == 0 ~ 0,
                       TRUE ~ (prob_dying^2*(1-prob_dying)/deaths)),
weighted_var = case_when(row_number() == n() ~ (num_alive_int^2)/death_rate^4*sample_var,
                         TRUE ~ (num_alive_int^2)*((1-fract_surv)*int_width+lead(obs_le_int))^2*sample_var),
sample_var_pers_yrs = purrr::accumulate(.x = weighted_var,
                                        .f = `+`,
                                        .dir = "backward"),
sample_var_obs_le = sample_var_pers_yrs/num_alive_int^2,
ci_low_95 = round(obs_le_int-(1.96*sqrt(sample_var_obs_le)), 1),
ci_high_95 = round(obs_le_int+(1.96*sqrt(sample_var_obs_le)), 1))

test_data <- data.frame(
  year = 2020,
  age_cat = c("0", "1-4", "5-9", "10-14", "15-19","20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90+"),
  deaths = c(101, 10, 16, 15, 65, 102, 164, 218, 268, 301, 427, 631, 999, 1414, 1661, 2244, 2583, 3234, 3841, 6546),
  population = c(37091, 159844, 226038, 232718, 208813, 187469, 219833, 244278, 268280, 274061, 284629, 289041, 269557, 257883, 230428, 192485, 139779, 85485, 57412, 38668)
)

library(dplyr)
test_le <- test_data %>%
  select(year, age_cat, deaths, population) %>%
  mutate(start_age = as.numeric(stringr::str_extract(age_cat, "^\\d{1,2}"))) %>% # start age makes sure the age groups sort correctly
  group_by(year) %>% # you can add other grouping variables (race, sex, etc here)
  arrange(start_age) %>%
  make_life_table() %>%
  ungroup()

test_le$num_alive_int == test_data$num_alive_int
