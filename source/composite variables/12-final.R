## scoring

r <- response %>%
  #1. pev
  #1.1 hhvulnerability_score
  new_recoding(target = hhvulnerability_score_pev) %>%
  recode_directly(to_expression = sum(c(hhh_score, primary_income_earner_score, vulnerable_members_score), na.rm = T)) %>%
  #1.2 legal_status_score 
  new_recoding(target = legal_status_score_pev) %>%
  recode_directly(to_expression = sum(c(id_score), na.rm = T)) %>%
  #1.3 dependengy level
  new_recoding(target = dependency_levels_score_pev) %>%
  recode_directly(to_expression = sum(c(adr_score, wdr_score, cgt_score), na.rm = T)) %>%
  #1.4 poverty score
  new_recoding(target = poverty_score_pev) %>%
  recode_directly(to_expression = sum(c(income_score, dir_score), na.rm = T)) %>%
  #1.5 hhexpenditure_score 
  new_recoding(target = hhexpenditure_score_pev) %>%
  recode_directly(to_expression = sum(c(hhex_score), na.rm = T)) %>%
  #1.6 displacement score
  new_recoding(target = displacement_score_pev) %>%
  recode_directly(to_expression = sum(c(length_of_displacement_score), na.rm = T)) %>%
  
  ## pev_score
  new_recoding(target = pev_score) %>%
  recode_directly(to_expression = sum(c(hhvulnerability_score_pev, legal_status_score_pev, dependency_levels_score_pev, 
                                        poverty_score_pev, hhexpenditure_score_pev, displacement_score_pev), na.rm = T)/ 8.4) %>%
  #2.education
  #2.1 long_term disruption
  new_recoding(target = long_term_disrup_score_edu) %>%
  recode_directly(to_expression = sum(c(education_level_score), na.rm = T)) %>%
  #2.2 mid term disruptio
  new_recoding(target = mid_term_disrup_score_edu) %>%
  recode_directly(to_expression = sum(c(enrollement_rate_score, drop_out_score, attendance_rate_score, 
                                        attend_previous_score), na.rm = T)) %>%
  #2.3 cost
  new_recoding(target = cost_score_edu) %>%
  recode_directly(to_expression = sum(c(education_expenditure_score, education_expenditure_change_score), na.rm = T)) %>%
  #2.4 availability
  new_recoding(target = availability_score_edu) %>%
  recode_directly(to_expression = sum(c(time_to_school_score), na.rm = T)) %>%
  #2.5 access
  new_recoding(target = access_score_edu) %>%
  recode_directly(to_expression = sum(c(access_to_school_score), na.rm = T)) %>%
  
  ##edu score
  new_recoding(target = edu_score) %>%
  recode_directly(to_expression = sum(c(long_term_disrup_score_edu, mid_term_disrup_score_edu, 
                                        cost_score_edu, availability_score_edu, access_score_edu), na.rm = T)/4) %>%
  
  #3.nutrition
  #3.1 coverage
  new_recoding(target = coverage_score_nut) %>%
  recode_directly(to_expression = sum(c(muac_coverage_score, nutrition_mobile_team_visit_score), na.rm = T)) %>%
  #3.2 use
  new_recoding(target = use_score_nut) %>%
  recode_directly(to_expression = sum(c(supplements_score, children_enrolled_nutrition_centers_score), na.rm = T)) %>%
  #3.3 availability 
  new_recoding(target = availability_score_nut) %>%
  recode_directly(to_expression = sum(c(time_to_nutrition_center_score), na.rm = T)) %>%
  #3.4 access
  new_recoding(target = access_score_nut) %>%
  recode_directly(to_expression = sum(c(access_to_nutrition_center_score), na.rm = T)) %>%
  
  ## nut score
  new_recoding(target = nut_score) %>%
  recode_directly(to_expression = sum(c(coverage_score_nut, use_score_nut, availability_score_nut, access_score_nut), na.rm = T) / 3.1) %>%
  
  #4. health
  #4.1 burden of disease
  new_recoding(target = burden_disease_score_health) %>%
  recode_directly(to_expression = sum(c(u5_morbidity_score, adult_morbidity_score), na.rm = T)) %>%
  #4.2 maternal health
  new_recoding(target = maternal_health_score_health) %>%
  recode_directly(to_expression = sum(c(pregnancy_health_score, birth_location_score), na.rm = T)) %>%
  #4.3 vaccination 
  new_recoding(target = vaccination_score_health) %>%
  recode_directly(to_expression = sum(c(vaccination_score), na.rm = T)) %>% 
  #4.4 mentalhealth
  new_recoding(target = mental_health_score_health) %>%
  recode_directly(to_expression = sum(c(mental_health_score), na.rm = T)) %>%
  #4.5 cost
  new_recoding(target = cost_score_health) %>%
  recode_directly(to_expression = sum(c(health_expenditure_score, health_expenditure_change_score), na.rm = T)) %>%
  #4.6 availability
  new_recoding(target = availability_score_health) %>%
  recode_directly(to_expression = sum(c(time_to_health_score, health_facility_score, mobile_team_score), na.rm = T)) %>%
  #4.7 access
  new_recoding(target = access_score_health) %>%
  recode_directly(to_expression = sum(c(health_access_score), na.rm = T)) %>%
  
  ##health
  new_recoding(target = health_score) %>%
  recode_directly(to_expression = sum(c(burden_disease_score_health, maternal_health_score_health, vaccination_score_health, 
                                      mental_health_score_health, cost_score_health, availability_score_health, 
                                      access_score_health), na.rm = T) / 6.4) %>%
  
  #5. snfi
  #5.1 shelter density
  new_recoding(target = sd_score_snfi) %>%
  recode_directly(to_expression = sum(c(shelter_density_score), na.rm = T)) %>%
  #5.2 shelter quality
  new_recoding(target = shelter_quality_score_snfi) %>%
  recode_directly(to_expression = sum(c(shelter_quality_score), na.rm = T)) %>%
  #5.3 shelter condition
  new_recoding(target = shelter_condition_score_snfi) %>%
  recode_directly(to_expression = sum(c(shelter_condition_score), na.rm = T)) %>%
  #5.4 shelter damage
  new_recoding(target = shelter_damage_score_snfi) %>%
  recode_directly(to_expression = sum(c(shelter_damage_score), na.rm = T)) %>%
  #5.5 security of tenure
  new_recoding(target = security_tenure_score_snfi) %>%
  recode_directly(to_expression = sum(c(hlp_score), na.rm = T)) %>%
  #5.6 nfi score
  new_recoding(target = nfi_score_snfi) %>%
  recode_directly(to_expression = sum(c(nfi_score), na.rm = T)) %>% 
  
  ## snfi
  new_recoding(target = snfi_score) %>%
  recode_directly(to_expression = sum(c(sd_score_snfi, shelter_quality_score_snfi, shelter_condition_score_snfi, 
                                        shelter_damage_score_snfi, security_tenure_score_snfi, nfi_score_snfi), na.rm = T) / 4.6) %>%
  
  #6. fsl
  #6.1 source
  new_recoding(target = source_score_fsl) %>%
  recode_directly(to_expression = sum(c(food_source_and_change_score), na.rm = T)) %>%
  #6.2 availability 
  new_recoding(target = availability_score_fsl) %>%
  recode_directly(to_expression = sum(c(access_to_market_score), na.rm = T)) %>%
  #6.3 sufficiency 
  new_recoding(target = sufficency_score_fsl) %>%
  recode_directly(to_expression = sum(c(sufficent_quantity_score, cereal_skip_meal_score), na.rm = T)) %>%
  #6.4 consumption
  new_recoding(target = consumption_score_fsl) %>%
  recode_directly(to_expression = sum(c(fcs_score, change_consumption_score), na.rm = T)) %>%
  #6.5 capacity
  new_recoding(target = capacity_score_fsl) %>%
  recode_directly(to_expression = sum(c(capacity_prepare_food_score), na.rm = T)) %>%
  #6.6 cost
  new_recoding(target = cost_score_fsl) %>%
  recode_directly(to_expression = sum(c(food_expenditure_score, food_expenditure_change_score), na.rm = T)) %>%
  #6.7 income
  new_recoding(target = income_score_fsl) %>%
  recode_directly(to_expression = sum(c(livelihood_income_score, lost_income_score), na.rm = T)) %>%
  #6.8 assests
  new_recoding(target = assets_score_fsl) %>%
  recode_directly(to_expression = sum(c(livelihood_assests_score, lost_livestock_score, lost_land_cultivation_score), na.rm = T)) %>%
  
  ## fsl
  new_recoding(target = fsl_score) %>%
  recode_directly(to_expression = sum(c(source_score_fsl, availability_score_fsl, sufficency_score_fsl, consumption_score_fsl,
                                        capacity_score_fsl, cost_score_fsl, income_score_fsl, assets_score_fsl), na.rm = T) / 8.6) %>%
  
  #7. wash
  #7.1 access to improved water source 
  new_recoding(target = improved_water_source_score_wash) %>%
  recode_directly(to_expression = sum(c(drinking_water_source_score, domestics_water_source_score, 
                                        water_treatment_score, time_to_water_source_score), na.rm = T)) %>%
  #7.2 suffiency
  new_recoding(target = suffiency_score_wash) %>%
  recode_directly(to_expression = sum(c(drinking_water_quantity_score, domestic_water_quantity_score, water_storage_score),
                                      na.rm = T)) %>%
  #7.3 safe storage
  new_recoding(target = safe_storage_score_wash) %>%
  recode_directly(to_expression = sum(c(jerrycan_quality_score), na.rm = T)) %>% 
  #7.4 cost
  new_recoding(target = cost_score_wash) %>%
  recode_directly(to_expression = sum(c(water_expenditure_score, water_expenditure_change_score), na.rm = T)) %>%
  #7.5 latrine use
  new_recoding(target = latrine_use_score_wash) %>%
  recode_directly(to_expression = sum(c(latrine_use_score, latrine_type_score), na.rm = T)) %>%
  #7.6 diginified latrine
  new_recoding(target = dignified_latrine_score_wash) %>%
  recode_directly(to_expression = sum(c(gender_separation_latrine_score, pwd_access_latrine_score, lock_latrine_score,
                                        soap_availability_latrine_score, internal_light_latrine_score, hygiene_latrine_score), na.rm = T)) %>%
  #7.7 access latrine
  new_recoding(target = access_latrine_score_wash) %>%
  recode_directly(to_expression = sum(c(distance_latrine_score), na.rm = T)) %>%
  #7.8 environmental sanitation
  new_recoding(target = environmental_sanitation_score_wash) %>%
  recode_directly(to_expression = sum(c(faecal_matter_disposal_score, environmental_contamination_score), na.rm = T)) %>%
  #7.9 
                    
  
  end_recoding()
                  
                  
                  
                  
                  
                  
  

