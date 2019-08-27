# horizontal aggregation
## wash

response <- 
  response %>%
  #1.1 improved drinking water source
  new_recoding(target = drinking_water_source_score, source = primary_source_drinking_water) %>%
  recode_to(to = 1, where.selected.exactly = "piped_system") %>%
  recode_to(to = 2, where.selected.any = c("borehole", "protected_well_with_hand_pump")) %>%
  recode_to(to = 3, where.selected.any = c("tank_and_tap", "vendors")) %>%
  recode_to(to = 4, where.selected.any = c("protected_well_no_hand_pump", "water_trucking_distrib", "water_kiosk")) %>%
  recode_to(to = 6, where.selected.any = c("unprotected_well", "other")) %>%
  recode_to(to = 7, where.selected.exactly = "berkad") %>%
  recode_to(to = 8, where.selected.exactly = "river") %>%
  #1.2 improved domestic water source
  new_recoding(target = domestics_water_source_score, source = primary_water_for_cooking) %>%
  recode_to(to = 1, where.selected.exactly = "piped_system") %>%
  recode_to(to = 2, where.selected.any = c("borehole", "protected_well_with_hand_pump")) %>%
  recode_to(to = 3, where.selected.any = c("tank_and_tap", "vendors")) %>%
  recode_to(to = 4, where.selected.any = c("protected_well_no_hand_pump", "water_trucking_distrib", "water_kiosk")) %>%
  recode_to(to = 6, where.selected.any = c("unprotected_well", "other")) %>%
  recode_to(to = 7, where.selected.exactly = "berkad") %>%
  recode_to(to = 8, where.selected.exactly = "river") %>%
  #1.3 water treatment
  new_recoding(target = water_treatment_score) %>%
  recode_to(to = 1, where.selected.any = c("boiling", "other_filter", "chlorine"), source = water_treat_how) %>%
  recode_to(to = 2, where = primary_source_drinking_water %in% c("borehole", "protected_well_with_hand_pump", "tank_and_tap", "piped_system") &
                            (treat_drinking_water == "no" | water_treat_how == "other") ) %>%
  recode_to(to = 3, where.selected.exactly = "cloth_filter", source = water_treat_how) %>%
  recode_to(to = 4, where = primary_source_drinking_water %in% c("protected_well_no_hand_pump") & treat_drinking_water == "no") %>%
  recode_to(to = 5, where = primary_source_drinking_water %in% c("vendors", "water_trucking_distrib", "water_kiosk") & 
              treat_drinking_water == "no") %>%
  recode_to(to = 5, where.selected.exactly = "other", source = water_treat_how) %>%
  recode_to(to = 6, where = primary_source_drinking_water %in% c ("unprotected_well", "other") & treat_drinking_water == "no") %>%
  recode_to(to = 7,  where = primary_source_drinking_water =="berkad" & treat_drinking_water == "no") %>%
  recode_to(to = 8,  where = primary_source_drinking_water == "river" & treat_drinking_water == "no") %>%
  #1.4 time to water source
  new_recoding(target = time_to_water_source_score, source = time_to_reach_water_source) %>%
  recode_to(to = 1, where.selected.exactly = "less15") %>%
  recode_to(to = 3, where.selected.exactly = "16_30") %>%
  recode_to(to = 5, where.selected.exactly = "31_60") %>%
  recode_to(to = 7, where.selected.exactly = "60_180") %>%
  recode_to(to = 8, where.selected.exactly = "above180") %>%
  #2.1 sufficient drinking water quantity
  new_recoding(target = drinking_water_quantity_score, source = enough_drinking_water) %>%
  recode_to(to = 1, where.selected.exactly = "yes") %>%
  recode_to(to = 7, where.selected.exactly = "no") %>%
  #2.2 sufficient domestic water quantity
  new_recoding(target = domestic_water_quantity_score, source = enough_cooking_water) %>%
  recode_to(to = 1, where.selected.exactly = "yes") %>%
  recode_to(to = 5, where.selected.exactly = "no") %>%
  #2.3 storage capacity
  new_recoding(target = water_storage_score) %>%
  recode_to(to = 8, where = grepl(pattern = "bucket_no_lid", x = how_water_stored) | 
                            grepl(pattern = "plastic_bottle", x = how_water_stored)) %>%
  recode_to(to = 7, where = grepl(pattern = "jerry_can", x = how_water_stored) & refill_jerrycan == "more_than_three") %>%
  recode_to(to = 6, where = grepl(pattern = "bucket_lid", x = how_water_stored)) %>%
  recode_to(to = 5, where = grepl(pattern = "jerry_can", x = how_water_stored) & refill_jerrycan == "thrice") %>%
  recode_to(to = 4, where = grepl(pattern = "jerry_can", x = how_water_stored) & refill_jerrycan == "twice") %>%
  recode_to(to = 4, where = grepl(pattern = "other", x = how_water_stored)) %>%
  recode_to(to = 3, where = grepl(pattern = "jerry_can", x = how_water_stored) & refill_jerrycan == "once") %>%
  recode_to(to = 2, where = grepl(pattern = "water_gallon", x = how_water_stored)) %>%
  recode_to(to = 1, where = grepl(pattern = "water_tank", x = how_water_stored)) %>%
  #3.1 jerrycan quality
  new_recoding(target = jerrycan_quality_score) %>%
  recode_to(to = 6, where = jerrycan_condition.none == 1) %>%
  recode_to(to = 5, where = jerrycan_condition.covered == 0) %>%
  recode_to(to = 4, where = jerrycan_condition.covered == 1 & jerrycan_condition.close_necked == 0) %>%
  recode_to(to = 2, where = jerrycan_condition.covered == 1 & jerrycan_condition.close_necked == 1 & 
                    jerrycan_condition.stored_on_a_table == 0) %>%
  recode_to(to = 1, where = jerrycan_condition.covered == 1 & jerrycan_condition.close_necked == 1 & 
                    jerrycan_condition.stored_on_a_table == 1) %>%
  #4.1 water expendinture
  ## expenditure rate
  new_recoding(target = water_expenditure_rate) %>%
  recode_directly(to_expression = spent_water_middle / income_middle) %>%
  ## expenditure score
  new_recoding(target = water_expenditure_score) %>%
  recode_to(to = 1, where = water_expenditure_rate <= .25) %>%
  recode_to(to = 3, where = water_expenditure_rate > .25 & water_expenditure_rate <= .50) %>%
  recode_to(to = 4, where = water_expenditure_rate > .50 & water_expenditure_rate <= .75) %>%
  recode_to(to = 6, where = water_expenditure_rate > .75) %>%
  # recode_to(to = 6, where = water_expenditure_rate > .75 & water_expenditure_rate <= 1) %>%
  # recode_to(to = 7, where = water_expenditure_rate > 1) %>%
  #4.2 change in water expenditure
  #6.2 expenditure change
  new_recoding(target = water_expenditure_change_score, source = water_price_changed) %>%
  recode_to(to = 1, where.selected.exactly = "decrease") %>%
  recode_to(to = 2, where.selected.exactly = "no_change") %>%
  recode_to(to = 4, where.selected.exactly = "increase") %>%
  #5.1 use of latrine
  new_recoding(target = latrine_use_score) %>%
  recode_to(to = 1, where = household_access_latrine == "yes_personal") %>%
  recode_to(to = 3, where = household_access_latrine == "yes_shared" & sharing_latrines == "no") %>%
  recode_to(to = 6, where = household_access_latrine == "yes_shared" & sharing_latrines == "yes") %>%
  recode_to(to = 8, where = household_access_latrine == "no_latrine") %>%
  #5.2 type of latrine
  new_recoding(target = latrine_type_score) %>%
  recode_to(to = 1, where.selected.exactly = "flush_improved", source = latrine_type) %>%
  recode_to(to = 3, where.selected.exactly = "pit_improved", source = latrine_type) %>%
  recode_to(to = 5, where.selected.any = c("flush_unimproved", "other"), source = latrine_type) %>%
  recode_to(to = 6, where.selected.exactly = "pit_unimproved", source = latrine_type) %>%
  #6.1 gender segregation
  new_recoding(target = gender_separation_latrine_score) %>%
  recode_to(to = 5, where = latrines_seperated_by_gender == "no") %>%
  recode_to(to =1, where = latrines_seperated_by_gender == "no" & household_access_latrine == "yes_personal") %>%
  recode_to(to = 1, where = latrines_seperated_by_gender == "yes") %>%
  #6.2 access to pwd
  new_recoding(target = pwd_access_latrine_score) %>%
  recode_to(to = 5, where = latrines_accessible_to_disabled == "no") %>%
  recode_to(to =1, where = latrines_accessible_to_disabled == "no" & household_access_latrine == "yes_personal") %>%
  recode_to(to = 1, where = latrines_accessible_to_disabled == "yes") %>%
  #6.3 internal locks
  new_recoding(target = lock_latrine_score) %>%
  recode_to(to = 5, where = latrines_locked_from_inside == "no") %>%
  recode_to(to = 1, where = latrines_locked_from_inside == "no" & household_access_latrine == "yes_personal") %>%
  recode_to(to = 1, where = latrines_locked_from_inside == "yes") %>%
  #6.4 availability of water and soap
  new_recoding(target = soap_availability_latrine_score, source = functional_handwashing) %>%
  recode_to(to = 1, where.selected.exactly = "yes") %>%
  recode_to(to = 5, where.selected.exactly = "no") %>%
  #6.5 internal light
  new_recoding(target = internal_light_latrine_score, source = latrines_have_light) %>%
  recode_to(to = 1, where.selected.exactly = "yes") %>%
  recode_to(to = 5, where.selected.exactly = "no") %>%
  #6.6 hygiene latrine
  new_recoding(target = hygiene_latrine_score, source = latrine_hygiene) %>%
  recode_to(to = 1, where.selected.exactly = "v_hygienic") %>%
  recode_to(to = 2, where.selected.exactly = "hygienic") %>%
  recode_to(to = 5, where.selected.exactly = "unhygienic") %>%
  recode_to(to = 7, where.selected.exactly = "v_hygienic") %>%
  #7.1 distance to latrine
  new_recoding(target = distance_latrine_score, source = time_to_reach_latrine) %>%
  recode_to(to = 1, where.selected.exactly = "less15") %>%
  recode_to(to = 3, where.selected.exactly = "16_30") %>%
  recode_to(to = 5, where.selected.exactly = "31_60") %>%
  recode_to(to = 7, where.selected.exactly = "60_180") %>%
  recode_to(to = 8, where.selected.exactly = "above180") %>%
  #8.1 faecal disposal
  new_recoding(target = faecal_matter_disposal_score, source = dispose_children_feaces) %>%
  recode_to(to = 1, where.selected.exactly = "covered_pit") %>%
  recode_to(to = 2, where.selected.exactly = "burial") %>%
  recode_to(to = 5, where.selected.exactly = "burning") %>%
  recode_to(to = 7, where.selected.exactly = "in_open") %>%
  #8.2 environmental contamination
  ## environmental contamination sum
  new_recoding(target = environmental_contamination_sum) %>%
  recode_directly(to_expression = sum(enviromental_sanitaiton_problems.decying_organic_matter, 
                                      enviromental_sanitaiton_problems.stagnent_water,
                                      enviromental_sanitaiton_problems.feacal_matter,
                                      enviromental_sanitaiton_problems.rodents,
                                      enviromental_sanitaiton_problems.solid_household_waste)) %>%
  ## evenvironmental contamination score
  new_recoding(target = environmental_contamination_score, source = environmental_contamination_sum) %>%
  recode_to(to = 1, where.num.equal = 0) %>%
  recode_to(to = 4, where.num.equal = 1) %>%
  recode_to(to = 5, where.num.equal = 2) %>%
  recode_to(to = 6, where.num.equal = 3) %>%
  recode_to(to = 7, where.num.equal = 4) %>%
  recode_to(to = 8, where.num.equal = 5) %>%
  #9.1 hygiene awareness
  ## hygiene awareness sum
  new_recoding(target = hygiene_awareness_sum) %>%
  recode_directly(to_expression = sum(when_wash_hands.before_eating,
                                      when_wash_hands.after_defecating, 
                                      when_wash_hands.before_food_baby,
                                      when_wash_hands.serving_food,
                                      when_wash_hands.before_food,
                                      when_wash_hands.after_baby_poop,
                                      when_wash_hands.after_eating)) %>%
  ## hygiene awareness score
  new_recoding(target = hygiene_awareness_score, source = hygiene_awareness_sum) %>%
  recode_to(to = 1, where.num.equal = 7) %>%
  recode_to(to = 2, where.num.smaller.equal = 6) %>%
  recode_to(to = 3, where.num.equal = 4) %>%
  recode_to(to = 4, where.num.equal = 3) %>%
  recode_to(to = 6, where.num.equal = 2) %>%
  recode_to(to = 7, where.num.equal = 1) %>%
  recode_to(to = 8, where.num.equal = 0) %>%
  #10.1 soap availability
  new_recoding(target = soap_access_score, source = access_to_soap) %>%
  recode_to(to = 1, where.selected.exactly = "yes") %>%
  recode_to(to = 7, where.selected.exactly = "no") %>%
  #10.2 menstrual materials availability
  new_recoding(target = menstrual_mat_access_score, source = hygienic_menstruction_materials) %>%
  recode_to(to = 1, where.selected.exactly = "yes") %>%
  recode_to(to = 7, where.selected.exactly = "no") %>%
  # recode_to(to = dd, where.selected.exactly = "dnk") %>%
  #11.1 access to hand washing facility
  new_recoding(target = handwashing_access_score, source = nearest_handwashing_facility) %>%
  recode_to(to = 1, where.selected.exactly = "less15") %>%
  recode_to(to = 3, where.selected.exactly = "16_30") %>%
  recode_to(to = 5, where.selected.exactly = "31_60") %>%
  recode_to(to = 7, where.selected.exactly = "60_180") %>%
  recode_to(to = 8, where.selected.exactly = "above180") %>%
  #12.1 aap water
  new_recoding(target = aap_water_source_score, source = household_been_consulted_water) %>%
  recode_to(to = 1, where.selected.exactly = "yes") %>%
  recode_to(to = 4, where.selected.exactly = "no") %>%
  #12.2 aap sanitation
  new_recoding(target = aap_sanitation_score, source = household_been_consulted_sanitaiton) %>%
  recode_to(to = 1, where.selected.exactly = "yes") %>%
  recode_to(to = 4, where.selected.exactly = "no") %>%
  #12.3 aap satisfaction
  new_recoding(target = aap_satisfaction_score, source = water_sourcces_well_developed) %>%
  recode_to(to = 1, where.selected.exactly = "yes") %>%
  recode_to(to = 5, where.selected.exactly = "no") %>%
  end_recoding()
