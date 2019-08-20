# horizontal aggregation
## item-repo

response <- 
  response %>%
  new_recoding(target = item_repo_sum) %>%
  recode_directly(to_expression = 
                    (sleeping_mats == "yes") * 1 +
                    (blankets == "yes") * 2 +
                    (mosquito_nets == "yes") * 3 +
                    (jer_water_tanks == "yes") * 4 +
                    (kitchen_kettle == "yes") * 5 +
                    (light == "yes") * 6 +
                    (clothes == "yes") * 7 +
                    (toileteries == "yes") * 8 +
                    (sana_diapers == "yes") * 9 +
                    (cooking_stove == "yes") * 10 +
                    (fuel_generator == "yes") * 11 +
                    (locks_doors == "yes") * 12 +
                    (phone_radio == "yes") * 13 +
                    (cleaning_equi == "yes") * 14 +
                    (waste_bin == "yes") * 15 +
                    (houtehold_etc == "yes") * 16 +
                    (school_stationery == "yes") * 17 +
                    (children_toys == "yes") * 18 +
                    (furniture_stools == "yes") * 19 +
                    (storage_space == "yes") * 20 +
                    (fan == "yes") * 21 + 
                    (refrigerator == "yes") * 22 +
                    (trasport == "yes") * 23) %>%
  new_recoding(target = item_repo_score, source = item_repo_sum) %>%
  recode_to(to = 8, where.num.larger.equal = 0) %>%
  recode_to(to = 7, where.num.larger = 20) %>%
  recode_to(to = 6, where.num.larger = 45) %>%
  recode_to(to = 5, where.num.larger = 66) %>%
  recode_to(to = 4, where.num.larger = 91) %>%
  recode_to(to = 3, where.num.larger = 136) %>%
  recode_to(to = 2, where.num.larger = 171) %>%
  recode_to(to = 1, where.num.larger = 210) %>% 
  end_recoding()
  
  

