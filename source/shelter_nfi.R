# horizontal aggregation
## shelter nfi

r <-
  response %>%
  #1.1 shelter density
  #2.1 shelter quality
  #3.1 shelter condition
  new_recoding(target = shelter_condition_score) %>%
  recode_to(to = 1, where = internal_seperation_rooms == "yes" & 
                            source_of_light_at_night ==  "yes" &
                            shelter_lock_from_inside ==  "yes" &
                            shelter_lock_from_outside == "yes" &
                            theft_from_shelter == "no") %>%
  recode_to(to = 2, where = internal_seperation_rooms == "yes" & 
                            source_of_light_at_night ==  "yes" &
                            shelter_lock_from_inside ==  "yes" &
                            shelter_lock_from_outside == "yes" &
                            theft_from_shelter == "yes") %>%
  recode_to(to = 3, where = internal_seperation_rooms == "yes" & 
                            source_of_light_at_night ==  "yes" &
                            shelter_lock_from_inside ==  "yes" &
                            shelter_lock_from_outside == "yes" &
                            theft_from_shelter == "yes") %>%
                
  #4.1 shelter damage
  new_recoding(target = shelter_damage_score, source = shelter_damaged_last_90_days) %>%
  recode_to(to = 1, where.selected.exactly = "yes") %>%
  recode_to(to = 6, where.selected.exactly = "no") %>%
  #5.1 hlp
  new_recoding(target = hlp_score) %>%
  recode_to(to = 1, where = own_land == "yes" &
                            doc_land_tenure == "yes" &
                            hlp_dispute == "no") %>%
  recode_to(to = 2, where = own_land == "yes" &
                            doc_land_tenure == "yes" &
                            hlp_dispute == "yes") %>%
  recode_to(to = 3, where = own_land == "yes" &
                            doc_land_tenure == "no" &
                            hlp_dispute == "no") %>%
  recode_to(to = 4, where = own_land == "yes" &
                            doc_land_tenure == "no" &
                            hlp_dispute == "yes") %>%
  recode_to(to = 5, where = own_land == "no" &
                            doc_land_tenure == "yes" &
                            hlp_dispute == "no") %>%
  recode_to(to = 6, where = own_land == "no" &
                            doc_land_tenure == "yes" &
                            hlp_dispute == "yes") %>%
  recode_to(to = 7, where = own_land == "no" &
                            doc_land_tenure == "no" &
                            hlp_dispute == "no") %>%
  recode_to(to = 8, where = own_land == "no" &
                            doc_land_tenure == "no" &
                            hlp_dispute == "yes") %>%
  
  #6.1 NFI
  new_recoding(target = nfi_sum) %>%
  recode_directly(to_expression = sum(household_nfi.cups, household_nfi.mats, household_nfi.kettle, household_nfi.bucket,
                                      household_nfi.lamp, household_nfi.jerry_can, household_nfi.spoon, household_nfi.knives,
                                      household_nfi.wash_basin, household_nfi.plates, household_nfi.cooking_pot, 
                                      household_nfi.blankets, household_nfi.plastic_sheet)) %>%
  new_recoding(target = nfi_score) %>%
  recode_to(to = 1, where.num.equal = 13, source = nfi_sum) %>%
  recode_to(to = 2, where = nfi_sum < 13 & nfi_sum >= 10) %>%
  recode_to(to = 3, where = nfi_sum < 10 & nfi_sum >= 8) %>%
  recode_to(to = 4, where = nfi_sum < 8 & nfi_sum >= 7) %>%
  recode_to(to = 5, where = nfi_sum < 7 & nfi_sum >= 6) %>%
  recode_to(to = 6, where = nfi_sum < 6 & nfi_sum >= 3) %>%
  recode_to(to = 7, where = nfi_sum < 3 & nfi_sum >= 1) %>%
  recode_to(to = 8, where.num.equal = 0, source = nfi_sum) %>%
  # recode_to(where.num.larger.equal = )
  end_recoding()
  
  