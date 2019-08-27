# horizontal aggregation
## fsl

response <- 
  response %>%
  #4.1 FCS computation (from previous code)
  new_recoding(target = main_staples_sum) %>%
  recode_directly(to_expression = cereals + roots) %>%
  new_recoding(target = main_staples_freq) %>%
  recode_directly(to_expression = ifelse(main_staples_sum > 7, 7, main_staples_sum)) %>%
  new_recoding(target = main_staples_score) %>%
  recode_directly(to_expression = main_staples_freq * 2) %>%
  new_recoding(target = pulses_score) %>%
  recode_directly(to_expression = pulses * 3) %>%
  new_recoding(target = vegetables_sum) %>%
  recode_directly(to_expression = vit_a_veg + vegetables + other_veg) %>%
  new_recoding(target = vegetables_freq) %>%
  recode_directly(to_expression = ifelse(vegetables_sum > 7, 7, vegetables_sum)) %>%
  new_recoding(target = vegetables_score) %>%
  recode_directly(to_expression = vegetables_freq * 1) %>%
  new_recoding(target = fruits_sum) %>%
  recode_directly(to_expression = fruits + other_fruit) %>%
  new_recoding(target = fruits_freq) %>%
  recode_directly(to_expression = ifelse(fruits_sum > 7, 7, fruits_sum)) %>%
  new_recoding(target = fruits_score) %>%
  recode_directly(to = fruits_freq * 1) %>%
  new_recoding(target = meat_sum) %>%
  recode_directly(meat + fish + eggs) %>%
  new_recoding(target = meat_freq) %>%
  recode_directly(to_expression = ifelse(meat_sum > 7, 7, meat_sum)) %>%
  new_recoding(target = meat_score) %>%
  recode_directly(to_expression = meat_freq * 4) %>%
  new_recoding(target = dairy_score) %>%
  recode_directly(to_expression = dairy * 4) %>%
  new_recoding(target = sweet_score) %>%
  recode_directly(sweet * 0.5) %>%
  new_recoding(target = fats_score) %>%
  recode_directly(fats * 0.5) %>%
  new_recoding(target = fcs) %>%
  recode_directly(main_staples_score + pulses_score + vegetables_score +
                    fruits_score + meat_score + dairy_score + sweet_score + fats_score) %>%
  new_recoding(fcs_ranking, source = fcs) %>%
  recode_to(to = "acceptable", where.num.larger.equal = 42) %>%
  recode_to(to = "borderline", where.num.smaller = 42) %>%
  recode_to(to = "poor", where.num.smaller = 28) %>%
  #1.1 food source and change
  ## sustainable source
  new_recoding(target = type_source_food, source = source_food) %>%
  recode_to(to = "sustainable", where.selected.any = c("purchased", "cultivated", "livestock", "fishing")) %>%
  recode_to(to = "peer", where.selected.any = c("NGO_aid", "government_aid", "familyfriends")) %>%
  recode_to(to = "not_sustainable", where.selected.any = c("foraging", "hunting", "barter", "other")) %>%
  new_recoding(target = type_change_source_food, source = source_change) %>%
  recode_to(to = "better", where.selected.any = c("change_to_purchase", "change_to_production")) %>%
  recode_to(to = "no_change", where.selected.any = c("not_changed", "dnk")) %>%
  recode_to(to = "worse", where.selected.any = c("change_to_borrow", "change_to_aid", "change_to_gift", "change_to_barter",
                                                 "change_to_wild")) %>%
  new_recoding(target = food_source_and_change_score) %>%
  recode_to(to = 1, where = type_source_food == "sustainable" & type_change_source_food == "no_change") %>%
  recode_to(to = 2, where = type_change_source_food == "better") %>%
  recode_to(to = 3, where = type_source_food == "peer" & type_change_source_food == "no_change") %>%
  recode_to(to = 4, where = type_source_food == "not_sustainable" & type_change_source_food == "no_change") %>%
  recode_to(to = 5, where = type_source_food == "sustainable" & type_change_source_food == "worse") %>%
  recode_to(to = 6, where = type_source_food == "peer" & type_change_source_food == "worse") %>%
  recode_to(to = 7, where = type_source_food == "not_sustainable" & type_change_source_food == "worse") %>%
  #2.1 availability of markets
  new_recoding(target = access_to_market_score) %>%
  recode_to(to = 1, where.selected.exactly = "less15", source = time_market) %>%
  recode_to(to = 2, where.selected.exactly = "16_30", source = time_market) %>%
  recode_to(to = 3, where.selected.exactly = "31_60", source = time_market) %>%
  recode_to(to = 4, where.selected.exactly = "60_180", source = time_market) %>%
  recode_to(to = 5, where = time_market == "31_60" & transport_market %in% c("walking", "bicycle")) %>%
  recode_to(to = 6, where.selected.exactly = "above180", source = time_market) %>%
  recode_to(to = 7, where = time_market == "60_180" & transport_market %in% c("walking", "bicycle")) %>%
  recode_to(to = 8, where = time_market == "above180" & transport_market %in% c("walking", "bicycle")) %>%
  #3.1 suffencient quantity
  new_recoding(target = sufficent_quantity_score, source = food_now) %>%
  recode_to(to = 1, where.selected.exactly = "yes") %>%
  recode_to(to = 7, where.selected.exactly = "no") %>%
  #3.2 cereal stocks and meals skipped
  new_recoding(target = cereal_skip_meal_score) %>%
  recode_to(to = 1, where = stock_last == "10more" & skip_meal == "no") %>%
  recode_to(to = 2, where = stock_last == "10more" & skip_meal == "yes") %>%
  recode_to(to = 3, where = stock_last == "8_10" & skip_meal == "no") %>%
  recode_to(to = 4, where = stock_last == "8_10" & skip_meal == "yes") %>%
  recode_to(to = 5, where = stock_last == "5_7" & skip_meal == "no") %>%
  recode_to(to = 6, where = stock_last == "5_7" & skip_meal == "yes") %>%
  recode_to(to = 7, where = stock_last == "2_4") %>%
  recode_to(to = 8, where = stock_last == "less_day") %>%
  #4.1 fcs MCNI score
  new_recoding(target = fcs_score) %>%
  recode_to(to = 1, where = fcs_ranking == "acceptable" & fcs >= 52) %>%
  recode_to(to = 2, where = fcs_ranking == "acceptable" & fcs < 52 & fcs >= 42) %>%
  recode_to(to = 4, where = fcs_ranking == "borderline" & fcs < 42 & fcs >= 35) %>%
  recode_to(to = 5, where = fcs_ranking == "borderline" & fcs < 35 & fcs >= 28) %>%
  recode_to(to = 6, where = fcs_ranking == "poor" & fcs < 28 & fcs >= 22) %>%
  recode_to(to = 7, where = fcs_ranking == "poor" & fcs < 22 & fcs >= 15) %>%
  recode_to(to = 8, where = fcs_ranking == "poor" & fcs < 15) %>%
  #4.2 change in consumption patterns
  new_recoding(target = change_consumption_sum) %>%
  recode_directly(to_expression = change_food.amount_increased * 3 +
                    change_food.quality_increased * 2 +
                    change_food.variety_increased * 1 +
                    change_food.none * 0 +
                    change_food.amount_reduced * (-3) +
                    change_food.quality_reduced * (-2) + 
                    change_food.variety_reduced * (-1)) %>%
  new_recoding(target = change_consumption_score, source = change_consumption_sum) %>%
  recode_to(to = 1, where.num.equal = 6) %>%
  recode_to(to = 2, where.num.smaller.equal = 5) %>%
  recode_to(to = 3, where.num.smaller.equal = 3) %>%
  recode_to(to = 4, where.num.smaller.equal = 1) %>%
  recode_to(to = 5, where.num.smaller.equal = -1) %>%
  recode_to(to = 6, where.num.smaller.equal = -3) %>%
  recode_to(to = 7, where.num.smaller.equal = -5) %>%
  recode_to(to = 8, where.num.equal = -6)  %>%
  #5.1 capacity to prepare food
  new_recoding(target = capacity_prepare_food_score) %>%
  recode_to(to = 1, where = water == "yes" & fuel == "yes" & cooking_utensils == "yes") %>%
  recode_to(to = 2, where = water == "yes" & fuel == "yes" & cooking_utensils == "no") %>%
  recode_to(to = 3, where = water == "yes" & fuel == "no" & cooking_utensils == "yes") %>%
  recode_to(to = 4, where = water == "yes" & fuel == "no" & cooking_utensils == "no") %>%
  recode_to(to = 5, where = water == "no" & fuel == "yes" & cooking_utensils == "yes") %>%
  recode_to(to = 6, where = water == "no" & fuel == "yes" & cooking_utensils == "no") %>%
  recode_to(to = 7, where = water == "no" & fuel == "no" & cooking_utensils == "yes") %>%
  recode_to(to = 8, where = water == "no" & fuel == "no" & cooking_utensils == "no") %>%
  #6.1 food expenditure
  ## expenditure rate
  new_recoding(target = food_expenditure_rate) %>%
  recode_directly(to_expression = spent_food_middle / income_middle) %>%
  ## expenditure score
  new_recoding(target = food_expenditure_score) %>%
  recode_to(to = 1, where = food_expenditure_rate <= .25) %>%
  recode_to(to = 3, where = food_expenditure_rate > .25 & food_expenditure_rate <= .50) %>%
  recode_to(to = 4, where = food_expenditure_rate > .50 & food_expenditure_rate <= .75) %>%
  recode_to(to = 6, where = food_expenditure_rate > .75) %>%
  # recode_to(to = 6, where = food_expenditure_rate > .75 & food_expenditure_rate <= 1) %>%
  # recode_to(to = 7, where = food_expenditure_rate > 1) %>%
  #6.2 expenditure change
  new_recoding(target = food_expenditure_change_score, source = cost_food) %>%
  recode_to(to = 1, where.selected.exactly = "decreased") %>%
  recode_to(to = 2, where.selected.exactly = "no_change") %>%
  recode_to(to = 4, where.selected.exactly = "increased") %>%
  #7.1 livelihood incomes
  new_recoding(target = livelihood_income_score, source = income_source) %>%
  recode_to(to = 1, where.selected.any = c("business", "contracted_job", "rent")) %>%
  recode_to(to = 3, where.selected.any = c("livestock")) %>%
  recode_to(to = 4, where.selected.any = c("cash_fishing", "cash_farming", "daily_labour")) %>%
  recode_to(to = 5, where.selected.any = c("subsistence_farming_fishin", "other")) %>%
  recode_to(to = 6, where.selected.any = c("remittances", "humanitarian_assisstance")) %>%
  recode_to(to = 7, where.selected.any = c("sale_humanitarian_assistance")) %>%
  recode_to(to = 8, where.selected.any = c("none")) %>%
  #7.2 lost of income source
  new_recoding(target = lost_income_score, source = lost_income_source) %>%
  recode_to(to = 1, where.selected.exactly = "no") %>%
  recode_to(to = 7, where.selected.exactly = "yes") %>%
  #8.1 assets
  new_recoding(livelihood_assests_score) %>%
  recode_to(to = 1, where = own_livestock == "yes" | own_livestock == "yes" | access_saving == "yes") %>%
  recode_to(to = 4, where = own_livestock == "no" | own_livestock == "no" | access_saving == "no") %>%
  #8.2 loss of assets livestock
  new_recoding(target = lost_livestock_score, source = lost_livestock) %>%
  recode_to(to = 1, where.selected.exactly = "no_lost") %>%
  recode_to(to = 4, where.selected.exactly = "yes_25") %>%
  recode_to(to = 5, where.selected.exactly = "yes_50") %>%
  recode_to(to = 6, where.selected.exactly = "yes_75") %>%
  recode_to(to = 7, where.selected.exactly = "yes_all") %>%
  #8.3 loss of assets cultivable land
  new_recoding(target = lost_land_cultivation_score, source = lost_land_cultivation) %>%
  recode_to(to = 1, where.selected.exactly = "no_lost") %>%
  recode_to(to = 4, where.selected.exactly = "yes_25") %>%
  recode_to(to = 5, where.selected.exactly = "yes_50") %>%
  recode_to(to = 6, where.selected.exactly = "yes_75") %>%
  recode_to(to = 7, where.selected.exactly = "yes_all") %>%
  end_recoding()
