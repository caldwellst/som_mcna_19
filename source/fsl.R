# horizontal aggregation
## fsl

r <-
  response %>%
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
  #4.1 FCS
  #4.2 consumption patterns
  #5.1 capacity to prepare food
  #6.1 food expenditure
  #6.2 expenditure
  #7.1 livelihood incomes
  #7.2 change in livelihood
  #8.1 assets
  #8.2 loss of assets livestock
  #8.3 loss of assets cultivable land
  end_recoding()
