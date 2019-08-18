## wash calculation

un_r <- 
  un_response %>%
  #1.1.1 Access to an improved water source 
  new_recoding(target = acces_to_improved_watersource) %>%
  recode_to(to = 1, where = primary_source_drinking_water %in% c("piped_system", "protected_well_no_hand_pump", 
                                                                 "protected_well_with_hand_pump", "tank_and_tap", "borehole")
                    & time_to_reach_water_source == "less15") %>%
  recode_to(to = 2, where = primary_source_drinking_water %in% c("piped_system", "protected_well_no_hand_pump",
                                                                 "protected_well_with_hand_pump", "tank_and_tap", "borehole")
            & time_to_reach_water_source == "16_30") %>%
  recode_to(to = 3, where = primary_source_drinking_water %in% c("piped_system", "protected_well_no_hand_pump",
                                                                 "protected_well_with_hand_pump", "tank_and_tap", "borehole")
            & time_to_reach_water_source %in% c("31_60", "60_180", "above180")) %>%
  recode_to(to = 4, where = primary_source_drinking_water %in% c("unprotected_well", "water_trucking_distrib",
                                                                 "water_kiosk", "vendors", "berkad")) %>%
  recode_to(to = 5, where = primary_source_drinking_water %in% c("river", "other")) %>%
  #1.1.2 Access to a sufficient quantity of water
  ## good_storage_score
  new_recoding(target = water_storage_capacity) %>% 
  recode_to(to = "enough storage", where = how_water_stored.water_tank == 1 | how_water_stored.water_gallon == 1 |
              (how_water_stored.jerry_can == 1 & refill_jerrycan != "more_than_three")) %>%
  recode_to(to = "not enough storage", where = is.na(water_storage_capacity)) %>%
  ##Access to a sufficient quantity of water
  new_recoding(target = access_to_suff_qty_water) %>%
  recode_to(to = 5, where = enough_drinking_water == "no" & enough_cooking_water == "no") %>%
  recode_to(to = 4, where = (enough_drinking_water == "yes" | enough_cooking_water == "yes") &
              water_storage_capacity == "not enough storage") %>%
  recode_to(to = 3, where = (enough_drinking_water == "yes" | enough_cooking_water == "yes") &
              water_storage_capacity == "enough storage") %>%
  recode_to(to = 2, where = enough_drinking_water == "yes" & enough_cooking_water == "yes" &
              water_storage_capacity == "not enough storage") %>%
  recode_to(to = 1, where = enough_drinking_water == "yes" & enough_cooking_water == "yes" &
              water_storage_capacity == "enough storage") %>%
  end_recoding()
