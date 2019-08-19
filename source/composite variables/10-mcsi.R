# horizontal aggregation
## mcsi

response <- 
  response %>%
  #1.1 water
  ## sum
  new_recoding(target = mcsi_water_sum) %>%
  recode_directly(to_expression = 
                    coping_water.borrow_mat_cash * 1 +
                    coping_water.extra_time_secure * 2 +
                    coping_water.reduce_domestic_water * 3 +
                    coping_water.seasonal_source * 3 +
                    coping_water.hum_assistance * 3 + 
                    coping_water.adult_extra_job * 4 +
                    coping_water.use_money_other * 4 +
                    coping_water.children_fetch_water * 5 +
                    coping_water.sell_assets_other * 5 +
                    coping_water.reduce_drinking_water * 6 +
                    coping_water.reduce_adult_cons * 6 +
                    coping_water.adults_beg * 7 +
                    coping_water.minors_work * 7 +
                    coping_water.travel_insecure * 7 +
                    coping_water.drink_unsafe_water * 8 +
                    coping_water.minors_beg * 8 +
                    coping_water.exploit_hum * 8) %>%
  ## mcsi_water_score
  new_recoding(target = mcsi_water_score, source = mcsi_water_sum) %>%
  recode_to(to = 8, where.num.larger.equal = 84) %>%
  recode_to(to = 7, where.num.smaller = 84) %>%
  recode_to(to = 6, where.num.smaller = 72) %>%
  recode_to(to = 5, where.num.smaller = 60) %>%
  recode_to(to = 4, where.num.smaller = 48) %>%
  recode_to(to = 3, where.num.smaller = 36) %>%
  recode_to(to = 2, where.num.smaller = 24) %>%
  recode_to(to = 1, where.num.smaller = 12) %>%
  #1.2 sanitation
  ## sum
  new_recoding(target = mcsi_sanitation_sum) %>% 
  recode_directly(to_expression = 
                    coping_sanitation.share_latrines * 2 +
                    coping_sanitation.extra_time_secure * 2 +
                    coping_sanitation.use_unhygienic * 3 +
                    coping_sanitation.hum_assistance * 3 +
                    coping_sanitation.use_money_other * 4 +
                    coping_sanitation.use_insecure * 6 +
                    coping_sanitation.travel_insecure * 7 +
                    coping_sanitation.open_defaecation * 8 +
                    coping_sanitation.exploit_hum * 8) %>%
  new_recoding(target = mcsi_sanitation_score, source = mcsi_sanitation_sum) %>%
  recode_to(to = 8, where.num.larger.equal = 42) %>%
  recode_to(to = 7, where.num.smaller = 42) %>%
  recode_to(to = 6, where.num.smaller = 36) %>%
  recode_to(to = 5, where.num.smaller = 30) %>% 
  recode_to(to = 4, where.num.smaller = 24) %>%
  recode_to(to = 3, where.num.smaller = 18) %>%
  recode_to(to = 2, where.num.smaller = 12) %>%
  recode_to(to = 1, where.num.smaller = 6) %>%
  #1.3 hygienic materials
  new_recoding(target = mcsi_hygienic_mat_sum) %>%
  recode_directly(to_expression = 
                    coping_hygiene.soap_sub_cloth * 1 +
                    coping_hygiene.borrow_mat_cash * 1 +
                    coping_hygiene.soap_sub_hands * 2 +
                    coping_hygiene.soap_sub_menstrual * 2 +
                    coping_hygiene.extra_time_secure * 2 +
                    coping_hygiene.wash_less * 3 +
                    coping_hygiene.no_wash_menstrual * 3 +
                    coping_hygiene.hum_assistance * 3 +
                    coping_hygiene.adult_extra_job * 4 +
                    coping_hygiene.use_money_other * 4 +
                    coping_hygiene.sell_assets_other * 5 +
                    coping_hygiene.bathe_latrine * 6 +
                    coping_hygiene.no_soap_hands * 6 +
                    coping_hygiene.no_use_menstrual * 7 +
                    coping_hygiene.adults_beg * 7 +
                    coping_hygiene.minors_work * 7 +
                    coping_hygiene.travel_insecure * 7 +
                    coping_hygiene.no_wash_hands * 8 +
                    coping_hygiene.minors_beg * 8 +
                    coping_hygiene.exploit_hum * 8) %>%
  new_recoding(target = mcsi_hygienic_mat_score, source = mcsi_hygienic_mat_sum) %>%
  recode_to(to = 8, where.num.larger.equal = 84) %>%
  recode_to(to = 7, where.num.smaller = 84) %>%
  recode_to(to = 6, where.num.smaller = 72) %>%
  recode_to(to = 5, where.num.smaller = 60) %>%
  recode_to(to = 4, where.num.smaller = 48) %>%
  recode_to(to = 3, where.num.smaller = 36) %>%
  recode_to(to = 2, where.num.smaller = 24) %>%
  recode_to(to = 1, where.num.smaller = 12) %>%
  #1.4 food
  new_recoding(target = mcsi_food_sum) %>%
  recode_directly(to_expression = 
                    coping_food.borrow_food * 1 +
                    coping_food.change_food * 2 +
                    coping_food.extra_time_secure * 2 +
                    coping_food.hum_assistance * 3 +
                    coping_food.borrow_mat_cash * 4 +
                    coping_food.children_relative * 4 + 
                    coping_food.adult_extra_job * 4 +
                    coping_food.reduce_meal_size * 5 +
                    coping_food.sell_assets_other * 5 +
                    coping_food.reduce_meal_cons * 6 +
                    coping_food.reduce_adult_food_cons * 6 +
                    coping_food.travel_insecure * 7 +
                    coping_food.adults_beg * 7 +
                    coping_food.minors_work * 7 +
                    coping_food.minors_beg * 8 +
                    coping_food.exploit_hum * 8) %>%
  new_recoding(target = mcsi_food_score, source = mcsi_food_sum) %>%
  recode_to(to = 8, where.num.larger.equal = 84) %>%
  recode_to(to = 7, where.num.smaller = 84) %>%
  recode_to(to = 6, where.num.smaller = 72) %>%
  recode_to(to = 5, where.num.smaller = 60) %>%
  recode_to(to = 4, where.num.smaller = 48) %>%
  recode_to(to = 3, where.num.smaller = 36) %>%
  recode_to(to = 2, where.num.smaller = 24) %>%
  recode_to(to = 1, where.num.smaller = 12) %>%
  #1.5 shelter
  new_recoding(target = mcsi_shelter_sum) %>%
  recode_directly(to_expression = 
                    coping_shelter.borrow_mat_cash * 1 +
                    coping_shelter.extra_time_secure * 2 +
                    coping_shelter.hosted_other * 3 +
                    coping_shelter.scavenge_materials * 3 +
                    coping_shelter.hum_assistance * 3 +
                    coping_shelter.move_shelter * 4 +
                    coping_shelter.send_children * 4 +
                    coping_shelter.adult_extra_job * 4 + 
                    coping_shelter.use_money_other * 4 +
                    coping_shelter.move_settlement * 5+
                    coping_shelter.squat * 5 +
                    coping_shelter.sell_assets_other * 5 +
                    coping_shelter.travel_insecure * 7 +
                    coping_shelter.adults_beg * 7 +
                    coping_shelter.minors_work * 7 +
                    coping_shelter.open_air * 8 +
                    coping_shelter.minors_beg * 8 +
                    coping_shelter.exploit_hum * 8) %>%
  new_recoding(target = mcsi_shelter_score, source = mcsi_shelter_sum) %>%
  recode_to(to = 8, where.num.larger.equal = 84) %>%
  recode_to(to = 7, where.num.smaller = 84) %>%
  recode_to(to = 6, where.num.smaller = 72) %>%
  recode_to(to = 5, where.num.smaller = 60) %>%
  recode_to(to = 4, where.num.smaller = 48) %>%
  recode_to(to = 3, where.num.smaller = 36) %>%
  recode_to(to = 2, where.num.smaller = 24) %>%
  recode_to(to = 1, where.num.smaller = 12) %>%
  #1.6 nfi
  new_recoding(target = mcsi_nfi_sum) %>%
  recode_directly(to_expression = 
                    coping_nfi.borrow_mat_cash * 1 +
                    coping_nfi.extra_time_secure * 2 +
                    coping_nfi.hum_assistance * 3 +
                    coping_nfi.adult_extra_job * 4 +
                    coping_nfi.use_money_other * 4 +
                    coping_nfi.sell_assets_other * 5 +
                    coping_nfi.scavenge_nfi * 6 +
                    coping_nfi.travel_insecure * 7 +
                    coping_nfi.adults_beg * 7 +
                    coping_nfi.minors_work * 7 +
                    coping_nfi.minors_beg * 8 +
                    coping_nfi.exploit_hum * 8) %>%
  new_recoding(target = mcsi_nfi_score, source = mcsi_nfi_sum) %>%
  recode_to(to = 8, where.num.larger.equal = 58) %>%
  recode_to(to = 7, where.num.smaller = 58) %>%
  recode_to(to = 6, where.num.smaller = 50) %>%
  recode_to(to = 5, where.num.smaller = 42) %>%
  recode_to(to = 4, where.num.smaller = 34) %>% 
  recode_to(to = 3, where.num.smaller = 25) %>%
  recode_to(to = 2, where.num.smaller = 17) %>%
  recode_to(to = 1, where.num.smaller = 9) %>%
  #1.7 education
  new_recoding(target = mcsi_education_sum) %>%
  recode_directly(to_expression = 
                    coping_education.borrow_mat_cash * 1 +
                    coping_education.extra_time_secure * 2 +
                    coping_education.peer_learning * 2 +
                    coping_education.part_time_school * 2 +
                    coping_education.home_school * 3 +
                    coping_education.hum_assistance * 3 +
                    coping_education.adult_extra_job * 4 +
                    coping_education.use_money_other * 4 +
                    coping_education.sell_assets_other * 5 +
                    coping_education.travel_insecure * 7 +
                    coping_education.adults_beg * 7 + 
                    coping_education.minors_work * 7 +
                    coping_education.minors_beg * 8 +
                    coping_education.exploit_hum * 8) %>%
  new_recoding(target = mcsi_education_score, source = mcsi_education_sum) %>%
  recode_to(to = 8, where.num.larger.equal = 58) %>%
  recode_to(to = 7, where.num.smaller = 58) %>%
  recode_to(to = 6, where.num.smaller = 50) %>%
  recode_to(to = 5, where.num.smaller = 42) %>%
  recode_to(to = 4, where.num.smaller = 34) %>% 
  recode_to(to = 3, where.num.smaller = 25) %>%
  recode_to(to = 2, where.num.smaller = 17) %>%
  recode_to(to = 1, where.num.smaller = 9) %>%
  #1.8 health
  new_recoding(target = mcsi_health_sum) %>%
  recode_directly(to_expression = 
                    coping_health.borrow_mat_cash * 1 + 
                    coping_health.extra_time_secure * 2 +
                    coping_health.hum_assistance * 3 +
                    coping_health.self_medicate * 4 +
                    coping_health.adult_extra_job * 4 + 
                    coping_health.use_money_other * 4 +
                    coping_health.sell_assets_other * 5 +
                    coping_health.unqualified_treatment * 5 +
                    coping_health.no_treatment * 7 +
                    coping_health.travel_insecure * 7 +
                    coping_health.adults_beg * 7 +
                    coping_health.minors_work * 7 +
                    coping_health.minors_beg * 8 +
                    coping_health.exploit_hum * 8) %>%
  new_recoding(target = mcsi_health_score, source = mcsi_health_sum) %>%
  recode_to(to = 8, where.num.larger.equal = 59) %>%
  recode_to(to = 7, where.num.smaller = 59) %>%
  recode_to(to = 6, where.num.smaller = 51) %>%
  recode_to(to = 5, where.num.smaller = 43) %>%
  recode_to(to = 4, where.num.smaller = 35) %>% 
  recode_to(to = 3, where.num.smaller = 26) %>%
  recode_to(to = 2, where.num.smaller = 18) %>%
  recode_to(to = 1, where.num.smaller = 10) %>%
  # #1.9 resources
  # new_recoding(target = mcsi_resources_score, source = coping_general) %>%
  # recode_to(to = 1, where.selected.any = "borrow_mat_cash") %>%
  # recode_to(to = 2, where.selected.any = "extra_time_secure") %>%
  # recode_to(to = 3, where.selected.any = "hum_assistance") %>%
  # recode_to(to = 4, where.selected.any = c("adult_extra_job", "use_money_other")) %>%
  # recode_to(to = 5, where.selected.any = "sell_assets_other") %>%
  # recode_to(to = 7, where.selected.any = c("travel_insecure", "adults_beg", "minors_work")) %>%
  # recode_to(to = 8, where.selected.any = c("minors_beg", "exploit_hum")) %>%
  end_recoding()
