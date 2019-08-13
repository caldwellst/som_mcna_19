# horizontal aggregation
## fsl

r <-
  response %>%
  #4.1 FCS computation (from previous code)
  mutate(main_staples_sum = cereals + roots,
         main_staples_freq = ifelse(main_staples_sum > 7, 7, main_staples_sum),
         main_staples_score = main_staples_freq * 2,
         pulses_score = pulses * 3,
         vegetables_sum = vit_a_veg + vegetables + other_veg,
         vegetables_freq = ifelse(vegetables_sum > 7, 7, vegetables_sum),
         vegetables_score = vegetables_freq * 1,
         fruits_sum = fruits + other_fruit,
         fruits_freq = ifelse(fruits_sum > 7, 7, fruits_sum),
         fruits_score = fruits_freq * 1,
         meat_sum = meat + fish + eggs,
         meat_freq = ifelse(meat_sum > 7, 7, meat_sum),
         meat_score = meat_freq * 4,
         dairy_score = dairy * 4,
         sweet_score = sweet * 0.5,
         fats_score = fats * 0.5,
         FCS = main_staples_score + pulses_score + vegetables_score +
           fruits_score + meat_score + dairy_score + sweet_score + fats_score,
         FCS_ranking = ifelse(FCS >= 42, "acceptable", 
                              ifelse(FCS < 42 & FCS >= 28, "bordeline",
                                     ifelse( FCS < 28, "poor", NA)))) %>%
  #1.1 food source and change
  #2.1 availability of markets
  ## see 7.1 access_to_market_score in pre-existing vulnerability 
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
  #4.1 FCS MCNI score
  new_recoding(target = FCS_score) %>%
  recode_to(to = 1, where = FCS_ranking == "acceptable" & FCS >= 52) %>%
  recode_to(to = 2, where = FCS_ranking == "acceptable" & FCS < 52 & FCS >= 42) %>%
  recode_to(to = 4, where = FCS_ranking == "bordeline" & FCS < 42 & FCS >= 35) %>%
  recode_to(to = 5, where = FCS_ranking == "bordeline" & FCS < 35 & FCS >= 28) %>%
  recode_to(to = 6, where = FCS_ranking == "poor" & FCS < 28 & FCS >= 22) %>%
  recode_to(to = 7, where = FCS_ranking == "poor" & FCS < 22 & FCS >= 15) %>%
  recode_to(to = 8, where = FCS_ranking == "poor" & FCS < 15) %>%
  #4.2 change in consumption patterns
  new_recoding(target = change_consumption_score) %>%
  recode_to(to = 1, where = change_food.amount_increased == 1 & 
                            change_food.quality_increased == 1 & 
                            change_food.variety_increased == 1) %>%
  recode_to(to = 3, where = change_food.amount_increased == 1 & 
                            change_food.quality_increased == 1 & 
                            change_food.variety_reduced == 1) %>%
  recode_to(to = 4, where = change_food.amount_increased == 1 & 
                            change_food.quality_reduced == 1) %>%
  recode_to(to = 5, where = change_food.amount_reduced == 1 & 
                            change_food.quality_increased == 1) %>%
  recode_to(to = 6, where = change_food.amount_reduced == 1 & 
                            change_food.quality_reduced == 1) %>%
  recode_to(to = 7, where = change_food.amount_reduced == 1 & 
                            change_food.quality_reduced == 1 & 
                            change_food.variety_reduced == 1) %>%

  #5.1 capacity to prepare food
  new_recoding(target = capacity_prepare_food_score) %>%
  recode_to(to = 1, where = fuel == "yes" & water == "yes" & cooking_utensils == "yes") %>%
  recode_to(to = 2, where = fuel == "yes" & water == "yes" & cooking_utensils == "no") %>%
  recode_to(to = 3, where = fuel == "no" & water == "yes" & cooking_utensils == "yes") %>%
  recode_to(to = 4, where = fuel == "yes" & water == "no" & cooking_utensils == "yes") %>%
  recode_to(to = 5, where = fuel == "no" & water == "no" & cooking_utensils == "yes") %>%
  recode_to(to = 6, where = fuel == "no" & water == "no" & cooking_utensils == "no") %>%
  #6.1 food expenditure
  ## expenditure rate
  new_recoding(target = food_expenditure_rate) %>%
  recode_directly(to_expression = spent_food_middle / income_middle) %>%
  ## expenditure score
  new_recoding(target = food_expenditure_score) %>%
  recode_to(to = 1, where = food_expenditure_rate <= .25) %>%
  recode_to(to = 3, where = food_expenditure_rate > .25 & food_expenditure_rate <= .50) %>%
  recode_to(to = 4, where = food_expenditure_rate > .50 & food_expenditure_rate <= .75) %>%
  recode_to(to = 6, where = food_expenditure_rate > .75 & food_expenditure_rate <= 1) %>%
  # recode_to(to = 7, where = food_expenditure_rate > 1) %>%
  #6.2 expenditure change
  new_recoding(target = food_expenditure_change_score, source = cost_food) %>%
  recode_to(to = 1, where.selected.exactly = "decreased") %>%
  recode_to(to = 2, where.selected.exactly = "no_change") %>%
  recode_to(to = 4, where.selected.exactly = "increased") %>%
  # recode_to(to = "dd", where.selected.exactly = "dnk") %>%
  #7.1 livelihood incomes
  new_recoding(target = sum_livelihood) %>%
  recode_directly(sum(income_source.humanitarian_assisstance, income_source.daily_labour, income_source.livestock,
                      income_source.business, income_source.cash_fishing, income_source.remittances, 
                      income_source.contracted_job, income_source.cash_farming, income_source.rent,
                      income_source.subsistence_farming_fishin, income_source.other, income_source.sale_humanitarian_assistance)) %>%
  #7.2 lost of income source
  new_recoding(target = lost_income_score, source = lost_income_source) %>%
  recode_to(to = 1, where.selected.exactly = "no") %>%
  recode_to(to = 7, where.selected.exactly = "yes") %>%
  #8.1 assets
  # see pre existing vulnerability
  #8.2 loss of assets livestock
  new_recoding(target = lost_livestock_score, source = lost_livestock) %>%
  recode_to(to = 1, where.selected.exactly = "no_lost") %>%
  recode_to(to = 4, where.selected.exactly = "yes_25") %>%
  recode_to(to = 5, where.selected.exactly = "yes_50") %>%
  recode_to(to = 6, where.selected.exactly = "yes_75") %>%
  recode_to(to = 7, where.selected.exactly = "yes_all") %>%
  ##recode_to(to = dd, where.selected.exactly = "dnk")
  #8.3 loss of assets cultivable land
  new_recoding(target = lost_land_cultivation_score, source = lost_land_cultivation) %>%
  recode_to(to = 1, where.selected.exactly = "no_lost") %>%
  recode_to(to = 4, where.selected.exactly = "yes_25") %>%
  recode_to(to = 5, where.selected.exactly = "yes_50") %>%
  recode_to(to = 6, where.selected.exactly = "yes_75") %>%
  recode_to(to = 7, where.selected.exactly = "yes_all") %>%
  ##recode_to(to = dd, where.selected.exactly = "dnk")
  end_recoding()
