# horizontal aggregation
## mcsi

r <- response %>%
  #1.1 water
  new_recoding(target = mcsi_water_score, source = coping_water) %>%
  recode_to(to = 1, where.selected.exactly = "borrow_mat_cash") %>%
  recode_to(to = 2, where.selected.exactly = "extra_time_secure") %>%
  recode_to(to = 3, where.selected.any = c("reduce_domestic_water", "seasonal_source", "hum_assistance")) %>%
  recode_to(to = 4, where.selected.any = c("adult_extra_job", "use_money_other")) %>%
  recode_to(to = 5, where.selected.any = c("children_fetch_water", "sell_assets_other")) %>%
  recode_to(to = 6, where.selected.any = c("reduce_drinking_water", "reduce_adult_cons")) %>%
  recode_to(to = 7, where.selected.any = c("adults_beg", "minors_work", "travel_insecure")) %>%
  recode_to(to = 8, where.selected.any = c("drink_unsafe_water", "minors_beg", "exploit_hum")) %>%
  #1.2 sanitation
  new_recoding(target = mcsi_sanitation_score, source = coping_sanitation) %>%
  recode_to(to = 2, where.selected.any = c("share_latrines", "extra_time_secure")) %>%
  recode_to(to = 3, where.selected.any = c("use_unhygienic", "hum_assistance")) %>%
  recode_to(to = 4, where.selected.exactly = "use_money_other") %>%
  recode_to(to = 6, where.selected.exactly = "use_insecure") %>%
  recode_to(to = 7, where.selected.exactly = "travel_insecure") %>%
  recode_to(to = 8, where.selected.any = c("open_defaecation", "exploit_hum")) %>%
  #1.3 hygienic materials
  new_recoding(target = mcsi_hygienic_mat_score, source = coping_hygiene) %>%
  recode_to(to = 1, where.selected.any = c("soap_sub_cloth", "borrow_mat_cash")) %>%
  recode_to(to = 2, where.selected.any = c("soap_sub_hands", "soap_sub_menstrual", "extra_time_secure")) %>%
  recode_to(to = 3, where.selected.any = c("wash_less", "no_wash_menstrual", "hum_assistance")) %>%
  recode_to(to = 4, where.selected.any = c("adult_extra_job", "use_money_other")) %>%
  recode_to(to = 5, where.selected.exactly = c("sell_assets_other")) %>%
  recode_to(to = 6, where.selected.any = c("bathe_latrine", "no_soap_hands")) %>%
  recode_to(to = 7, where.selected.any = c("no_use_menstrual", "adults_beg", "minors_work", "travel_insecure")) %>%
  recode_to(to = 8, where.selected.any = c("no_wash_hands", "minors_beg", "exploit_hum")) %>%
  #1.4 food
  new_recoding(target = mcsi_food_score, source = coping_food) %>%
  recode_to(to = 1, where.selected.exactly = "borrow_food") %>%
  recode_to(to = 2, where.selected.any = c("change_food", "extra_time_secure")) %>%
  recode_to(to = 3, where.selected.exactly = c("hum_assistance")) %>%
  recode_to(to = 4, where.selected.any = c("borrow_mat_cash", "children_relative", "adult_extra_job", "use_money_other")) %>%
  recode_to(to = 5, where.selected.any = c("reduce_meal_size", "sell_assets_other")) %>%
  recode_to(to = 6, where.selected.any = c("reduce_meal_cons", "reduce_adult_food_cons")) %>%
  recode_to(to = 7, where.selected.any = c("travel_insecure", "adults_beg", "minors_work")) %>%
  recode_to(to = 8, where.selected.any = c("minors_beg", "exploit_hum")) %>%
  #1.5 shelter
  new_recoding(target = mcsi_shelter_score, source = coping_shelter) %>%
  recode_to(to = 1, where.selected.exactly = "borrow_mat_cash") %>%
  recode_to(to = 2, where.selected.exactly = "extra_time_secure") %>%
  recode_to(to = 3, where.selected.any = c("hosted_other", "scavenge_materials", "hum_assistance")) %>%
  recode_to(to = 4, where.selected.any = c("move_shelter", "send_children", "adult_extra_job", "use_money_other")) %>%
  recode_to(to = 5, where.selected.any = c("move_settlement", "squat", "sell_assets_other")) %>%
  recode_to(to = 7, where.selected.any = c("travel_insecure", "adults_beg", "minors_work")) %>%
  recode_to(to = 8, where.selected.any = c("open_air", "minors_beg", "exploit_hum")) %>%
  
  #1.6 nfi
  #1.7 education
  #1.8 health
  #1.9 resources
  end_recoding()