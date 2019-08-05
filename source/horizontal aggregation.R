# horizontal aggregation
## Pre-existing vulnerability indicators
r <-
  response %>%
  # Vulnerable heads of household
  new_recoding(target = hhh) %>%
  recode_to(to = "Adult Female-headed", 
            where = breadwinner == "adult_female" & household_expenditure.adult_female == 1) %>%
  recode_to(to = "Male Elderly-headed",
            where = breadwinner == "eldery_male" & household_expenditure.eldery_male == 1) %>%
  recode_to(to = "Female Elderly-headed",
            where = breadwinner == "eldery_female" & household_expenditure.eldery_female == 1) %>%
  recode_to(to = "Male Child-headed",
            where = breadwinner == "male_14_17" & household_expenditure.male_14_17 == 1) %>%
  recode_to(to = "Female Child-headed",
            where = breadwinner == "female_14_17" & household_expenditure.female_14_17 == 1) %>%
  # Vulnerable members in household
  # Documentation
  # Poverty levels
  ## poverty_level_income
  # new_recoding(target = income_score, source = income_average) %>%
  # recode_to(to = 2, where.selected.exactly = "200more") %>%
  # recode_to(to = 4, where.selected.exactly = )
  end_recoding()


