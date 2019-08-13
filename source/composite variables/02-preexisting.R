# horizontal aggregation
## Pre-existing vulnerability indicators
response <-
  response %>%
  # 1.1 Vulnerable heads of household
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
  # 1.2 Vulnerable members in household
  # 2.1 Documentation
  # Dependency level
  ##3.1 ADR
  ##3.2 WDR
  new_recoding(target = WDR) %>%
  recode_directly(to_expression = total_hh / working_persons) %>%
  new_recoding(target = WDR_score) %>%
  recode_to(to = 1, where = WDR <= .20) %>%
  recode_to(to = 2, where = WDR > .21 & WDR <= .30) %>%
  recode_to(to = 3, where = WDR >.31 & WDR <= .40) %>%
  recode_to(to = 4, where = WDR >.41 & WDR <= .60) %>%
  recode_to(to = 5, where = WDR >.61 & WDR <= .70) %>%
  recode_to(to = 6, where = WDR >.71 & WDR <= .80) %>%
  recode_to(to = 7, where = WDR >.81 & WDR <= .90) %>%
  recode_to(to = 8, where = WDR >.90) %>%
  ##3.3 CGT
  new_recoding(target = CGT_score, source = care_giving_time) %>%
  recode_to(to = 1, where.selected.exactly = "less_1h") %>%
  recode_to(to = 2, where.selected.exactly = "1h_2h") %>%
  recode_to(to = 4, where.selected.exactly = "2h_3h") %>%
  recode_to(to = 6, where.selected.exactly = "3h_4h") %>%
  recode_to(to = 8, where.selected.exactly = "4hmore") %>%
  # Poverty levels
  ## 4.1 poverty_level_income
  new_recoding(target = income_score, source = average_income) %>%
  recode_to(to = 2, where.selected.exactly = "200more") %>%
  recode_to(to = 4, where.selected.exactly = "151_200") %>%
  recode_to(to = 5, where.selected.exactly = "101_150") %>%
  recode_to(to = 6, where.selected.exactly = "61_100") %>%
  recode_to(to = 7, where.selected.exactly = "31_60") %>%
  recode_to(to = 8, where.selected.exactly = "less30") %>%
  recode_to(to = 9, where.selected.exactly = "none") %>%
  ## 4.2 Lost of income source
  new_recoding(target = loss_income_source_score, source = lost_income_source) %>%
  recode_to(to = 2, where.selected.exactly = "no") %>%
  recode_to(to = 5, where.selected.exactly = "yes") %>%
  ## 4.3 ownership assest 
  ## 4.4 lost of assests
  new_recoding(target = lost_assest_score) %>%
  recode_to(to = 2, where = (lost_livestock == "no" & lost_land_cultivation == "no") |
              (lost_livestock == "no" & land_cultivation == "no") |
              (own_livestock == "no" & lost_land_cultivation == "no")) %>%
  recode_to(to = 5, where = lost_livestock == "yes" | lost_land_cultivation == "yes") %>%
  ## 4.5 debt income ratio
  ### income_middle_point
  #see horizontal aggregation for all expenditure convertion
  
  ### debt_middle_point
  new_recoding(target = debt_middle, source = average_debt) %>%
  recode_to(to = 200, where.selected.exactly = "200more") %>%
  recode_to(to = 175, where.selected.exactly = "151_200") %>%
  recode_to(to = 125, where.selected.exactly = "101_150") %>%
  recode_to(to = 80, where.selected.exactly = "61_100") %>%
  recode_to(to = 45, where.selected.exactly = "31_60") %>%
  recode_to(to = 15, where.selected.exactly = "less30") %>%
  recode_to(to = 10, where.selected.exactly = "none") %>%
  ### DIR
  new_recoding(target = DIR) %>%
  recode_directly(to_expression = debt_middle / income_middle) %>%
  new_recoding(target = DIR_score) %>%
  recode_to(to = 1, where = DIR <= .25) %>%
  recode_to(to = 2, where = DIR > .25 & DIR <= .50) %>%
  recode_to(to = 3, where = DIR >.50 & DIR <= .60) %>%
  recode_to(to = 4, where = DIR >.61 & DIR <= .75) %>%
  recode_to(to = 5, where = DIR >.75 & DIR <= .85) %>%
  recode_to(to = 6, where = DIR >.85 & DIR <= 1) %>%
  recode_to(to = 7, where = DIR >1 & DIR <= 2) %>%
  recode_to(to = 8, where = DIR >2) %>%
  # 5.1 Expenditure on basic goods and services
  ## total expenditure
  #see horizontal aggregation for all expenditure convertion
  ## total_expenditure_middle
  new_recoding(target = total_expenditure_middle) %>%
  recode_directly(to_expression = sum(spent_education_middle, spent_health_middle, spent_water_middle,
                    spent_food_middle, na.rm = T)) %>%
  ## HHEX
  new_recoding(target = HHEX) %>%
  recode_directly(to_expression = total_expenditure_middle / income_middle) %>%
  ## HHEW_score
  new_recoding(target = HHEX_score) %>%
  recode_to(to = 1, where = HHEX <= .25) %>%
  recode_to(to = 2, where = HHEX > .25 & HHEX <= .50) %>%
  recode_to(to = 3, where = HHEX > .50 & HHEX <= .75) %>%
  recode_to(to = 4, where = HHEX > .75 & HHEX <= .80) %>%
  recode_to(to = 5, where = HHEX > .80 & HHEX <= .90) %>%
  recode_to(to = 6, where = HHEX > .90 & HHEX <= 1) %>%
  recode_to(to = 7, where = HHEX > 1 & HHEX <= 2) %>%
  recode_to(to = 8, where = HHEX > 2) %>%
  # 6.1 displacement (see horizontal aggregation.R for months convertion)
  new_recoding(target = length_of_displacement_score) %>%
  recode_to(to = 1, where.selected.exactly = "yes", source = yes_no_host) %>%
  recode_to(to = 4, where = diff_today_current_months <= 3) %>%
  recode_to(to = 3, where = diff_today_current_months > 6 & diff_today_current_months <= 12) %>%
  recode_to(to = 5, where = diff_today_current_months > 12 & diff_today_current_months <= 24) %>%
  recode_to(to = 6, where = diff_today_current_months > 24 & diff_today_current_months <= 36) %>%
  recode_to(to = 7, where = diff_today_current_months > 36 & diff_today_current_months <= 48) %>%
  recode_to(to = 8, where = diff_today_current_months > 48) %>%
  # 7.1 access to market
  new_recoding(target = access_to_market_score) %>%
  recode_to(to = 1, where.selected.exactly = "less15", source = time_market) %>%
  recode_to(to = 2, where.selected.exactly = "16_30", source = time_market) %>%
  recode_to(to = 3, where.selected.exactly = "31_60", source = time_market) %>%
  recode_to(to = 4, where.selected.exactly = "60_180", source = time_market) %>%
  recode_to(to = 4, where = time_market == "31_60" & transport_market %in% c("walking", "bicycle", "cart")) %>%
  recode_to(to = 5, where.selected.exactly = "above180", source = time_market) %>%
  recode_to(to = 5, where = time_market == "60_180" & transport_market %in% c("walking", "bicycle", "cart")) %>%
  recode_to(to = 6, where = time_market == "above180" & transport_market %in% c("walking", "bicycle", "cart")) %>%
  # 8.1 food insecurity
  end_recoding()

