# horizontal aggregation
## health

response <- 
  response %>%
  #1.1 under 5 morbidity
  ## under 5 morbidity sum
  new_recoding(target = u5_morbidity_sum) %>%
  recode_directly(to_expression =  sum(child_health.fever, child_health.malaria, child_health.awd, child_health.other,
                                      child_health.malnutrition, child_health.respiratory_problems)) %>%
  ## under 5 morbidity score
  new_recoding(target = u5_morbidity_score, source = u5_morbidity_sum) %>%
  recode_to(to = 1, where.num.equal = 0) %>%
  recode_to(to = 3, where.num.equal = 1) %>%
  recode_to(to = 5, where.num.equal = 2) %>%
  recode_to(to = 6, where.num.equal = 3) %>%
  recode_to(to = 7, where.num.larger.equal = 4) %>%
  #1.2 adult morbidity
  ## adult morbidity sum
  new_recoding(target = adult_morbidity_sum) %>%
  recode_directly(to_expression = sum(adult_heatlh.gastrointestinal, adult_heatlh.malaria, adult_heatlh.awd,
                                      adult_heatlh.fever, adult_heatlh.malnutrition, adult_heatlh.respiratory_problems,
                                      adult_heatlh.other)) %>%
  ## adult moribidity score
  new_recoding(target = adult_morbidity_score, source = adult_morbidity_sum) %>%
  recode_to(to = 1, where.num.equal = 0) %>%
  recode_to(to = 3, where.num.equal = 1) %>%
  recode_to(to = 5, where.num.equal = 2) %>%
  recode_to(to = 6, where.num.equal = 3) %>%
  recode_to(to = 7, where.num.larger.equal = 4) %>%
  #2.1 pregnancy score
  new_recoding(target = pregnancy_health_score, source = pregnancy_health) %>%
  recode_to(to = 1, where.selected.exactly = "no") %>%
  recode_to(to = 6, where.selected.exactly = "yes") %>%
  #2.2 birth location score
  new_recoding(target = birth_location_score, source = birth_where) %>%
  recode_to(to = 1, where.selected.exactly = "health_center") %>%
  recode_to(to = 3, where.selected.exactly = "traditional") %>%
  recode_to(to = 6, where.selected.exactly = "home") %>%
  #3.1 vaccinations
  #4.1 mental health
  #5.1 health expenditure score
  ## expenditure rate
  new_recoding(target = health_expenditure_rate) %>%
  recode_directly(to_expression = spent_health_middle / income_middle) %>%
  ## expenditure score
  new_recoding(target = health_expenditure_score) %>%
  recode_to(to = 1, where = health_expenditure_rate <= .25) %>%
  recode_to(to = 3, where = health_expenditure_rate > .25 & health_expenditure_rate <= .50) %>%
  recode_to(to = 4, where = health_expenditure_rate > .50 & health_expenditure_rate <= .75) %>%
  recode_to(to = 6, where = health_expenditure_rate > .75 & health_expenditure_rate <= 1) %>%
  # recode_to(to = 7, where = health_expenditure_rate > 1) %>%
  #5.2 health expenditure change
  new_recoding(target = health_expenditure_change_score, source = health_price_change) %>%
  recode_to(to = 1, where.selected.exactly = "decreased") %>%
  recode_to(to = 2, where.selected.exactly = "no_change") %>%
  recode_to(to = 4, where.selected.exactly = "increased") %>%
  # recode_to(to = "dd", where.selected.exactly = "dnk") %>%
  #6.1 time to health center score
  new_recoding(target = time_to_health_score) %>%
  recode_to(to = 1, where.selected.exactly = "less15", source = time_health) %>%
  recode_to(to = 2, where.selected.exactly = "16_30", source = time_health) %>%
  recode_to(to = 3, where.selected.exactly = "31_60", source = time_health) %>%
  recode_to(to = 4, where.selected.exactly = "60_180", source = time_health) %>%
  recode_to(to = 4, where = time_health == "31_60" & transport_health %in% c("walking", "bicycle", "cart")) %>%
  recode_to(to = 5, where.selected.exactly = "above180", source = time_health) %>%
  recode_to(to = 5, where = time_health == "60_180" & transport_health %in% c("walking", "bicycle", "cart")) %>%
  recode_to(to = 6, where = time_health == "above180" & transport_health %in% c("walking", "bicycle", "cart")) %>%
  #6.2 type of health center score
  new_recoding(target = health_facility_score, source = health_facility) %>%
  recode_to(to = 1, where.selected.any = c("government_clinic", "private_clinic")) %>%
  recode_to(to = 2, where.selected.exactly = "ngo_clinic") %>%
  recode_to(to = 3, where.selected.exactly = "traditional_midwife") %>%
  recode_to(to = 4, where.selected.any = c("pharmacy", "shop_drugs")) %>%
  recode_to(to = 6, where.selected.exactly = "traditional_healer") %>%
  #6.3 mobile team score
  new_recoding(target = mobile_team_score, source = mobile_health_outreach) %>%
  recode_to(to = 1, where.selected.exactly = "yes") %>%
  recode_to(to = 4, where.selected.exactly = "no") %>%
  #recode_to(to = dd, where.selected.exactly = "dontknow")
  #7.1 access to health care
  new_recoding(target = non_health_access_sum) %>%
  ######!!!!!!!!!!####### reversed coded, no is coded instead of yes #######!!!!!#####
  recode_directly(to_expression = sum(c(child_health_access, pregnancy_health_access, adult_health_access) == "no")) %>%
  new_recoding(target = health_access_score, source = non_health_access_sum) %>%
  recode_to(to = 1, where.num.equal = 0) %>%
  recode_to(to = 3, where.num.equal = 1) %>%
  recode_to(to = 4, where.num.equal = 2) %>%
  recode_to(to = 6, where.num.equal = 3) %>%
  end_recoding()


