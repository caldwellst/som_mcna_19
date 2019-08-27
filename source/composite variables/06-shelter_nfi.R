# horizontal aggregation
## shelter nfi

response <- 
  response %>%
  #1.1 shelter density
  new_recoding(target = sd) %>%
  recode_directly(to_expression = total_hh / (shelter_occupy * 4 )) %>%
  new_recoding(target = shelter_density_score) %>%
  recode_to(to = 1, where = sd < 1) %>%
  recode_to(to = 2, where = sd == 1) %>%
  recode_to(to = 3, where = sd > 1 & sd <= 1.25) %>%
  recode_to(to = 4, where = sd > 1.25 & sd <= 1.50) %>%
  recode_to(to = 5, where = sd > 1.50 & sd <= 1.75) %>%
  recode_to(to = 6, where = sd > 1.75 & sd <= 2) %>%
  recode_to(to = 7, where = sd > 2 & sd <= 2.25) %>%
  recode_to(to = 8, where = sd > 2.25) %>%
  #2.1 shelter quality 
  ## shelter components
  new_recoding(target = primary_floor_material_quality, source = primary_floor_material) %>%
  recode_to(to = 1, where.selected.any = c("earth", "cement")) %>%
  recode_to(to = 0,  where.selected.any = c("plastic_sheet", "cloth","vegetation", "none", "other")) %>%
  new_recoding(target = primary_structural_material_quality, source = primary_structural_material) %>%
  recode_to(to = 1, where.selected.any = c("wood", "metal", "bricks", "stones")) %>%
  recode_to(to = 0, where.selected.any = c("other")) %>%
  new_recoding(target = primary_roof_material_quality, source = primary_roof_material) %>%
  recode_to(to = 1, where.selected.any = c("earth", "cement", "wood", "cgi", "tin")) %>%
  recode_to(to = 0, where.selected.any = c("plastic_sheet", "cloth", "vegetation", "none")) %>%
  new_recoding(target = primary_wall_material_quality, source = primary_wall_material) %>%
  recode_to(to = 1, where.selected.any = c("earth", "cement", "bricks", "wood", "cgi", "tin")) %>%
  recode_to(to = 0, where.selected.any = c("plastic_sheet", "cloth", "vegetation", "none")) %>%
  new_recoding(target = primary_door_material_quality, source = primary_door_material) %>%
  recode_to(to = 1, where.selected.any = c("wood", "cgi", "tin")) %>%
  recode_to(to = 0, where.selected.any = c("plastic_sheet", "cloth", "vegetation", "none")) %>%
  ## shelter quality score
  new_recoding(target = shelter_quality_score) %>%
  recode_to(to = 1, where = primary_floor_material_quality == 1 & primary_structural_material_quality == 1 &
              primary_roof_material_quality == 1 & primary_wall_material_quality == 1 & primary_door_material_quality == 1) %>%
  recode_to(to = 2, where = primary_floor_material_quality == 0) %>%
  recode_to(to = 3, where = primary_door_material_quality == 0) %>%
  recode_to(to = 4, where = primary_wall_material_quality == 0) %>%
  recode_to(to = 5, where = primary_door_material_quality == 0 & primary_wall_material_quality == 0) %>%
  recode_to(to = 6, where = primary_structural_material_quality == 0 | primary_roof_material_quality == 0) %>%
  recode_to(to = 7, where = primary_structural_material_quality == 0 & primary_roof_material_quality == 0) %>%
  recode_to(to = 8, where = primary_floor_material_quality == 0 & primary_structural_material_quality == 0 &
              primary_roof_material_quality == 0 & primary_wall_material_quality == 0 & primary_door_material_quality == 0) %>%
  #3.1 shelter condition
  new_recoding(target = shelter_condition_sum) %>%
  recode_directly(to_expression = (ifelse(internal_seperation_rooms == "yes", 1, 0)) * 2 +
                                  (ifelse(source_of_light_at_night == "yes", 1, 0)) * 2 +
                                  (ifelse(shelter_lock_from_inside == "yes", 1, 0)) * 1 +
                                  (ifelse(shelter_lock_from_outside == "yes", 1, 0)) * 1 +
                                  (ifelse(theft_from_shelter == "no", 1, 0)) * 1) %>%
  mutate(shelter_condition_sum = ifelse(is.na(shelter_occupy), NA, shelter_condition_sum)) %>%
  new_recoding(target = shelter_condition_score, source = shelter_condition_sum) %>%
  recode_to(to = 1, where.num.equal = 7) %>%
  recode_to(to = 2, where.num.equal = 6) %>%
  recode_to(to = 3, where.num.equal = 5) %>%
  recode_to(to = 4, where.num.equal = 4) %>%
  recode_to(to = 5, where.num.equal = 3) %>%
  recode_to(to = 6, where.num.equal = 2) %>%
  recode_to(to = 7, where.num.equal = 1) %>%
  recode_to(to = 8, where.num.equal = 0) %>%
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
  end_recoding()
  
  