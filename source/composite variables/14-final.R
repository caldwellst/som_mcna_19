## scoring
# response <- readRDS("input/data/01-data_before_final_scoring_27082019.RDS")
response <- 
  response %>%
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
  recode_directly(to_expression = round(sum(c(hhvulnerability_score_pev, legal_status_score_pev, dependency_levels_score_pev, 
                                        poverty_score_pev, hhexpenditure_score_pev, displacement_score_pev), na.rm = T) / 21)) %>%

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
  
  ##edu score -- cost_score_edu not counted in the sum
  new_recoding(target = edu_score) %>%
  recode_directly(to_expression = round(sum(c(long_term_disrup_score_edu, mid_term_disrup_score_edu, 
                                         availability_score_edu, access_score_edu), na.rm = T)/10)) %>%

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
  recode_directly(to_expression = round(sum(c(coverage_score_nut, use_score_nut, availability_score_nut, 
                                              access_score_nut), na.rm = T) / 7.75)) %>%
  
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
  
  ##health -- cost_score_health not counted in the score
  new_recoding(target = health_score) %>%
  recode_directly(to_expression = round(sum(c(burden_disease_score_health, maternal_health_score_health, vaccination_score_health, 
                                      mental_health_score_health, availability_score_health, 
                                      access_score_health), na.rm = T) / 16)) %>%
  
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
  recode_directly(to_expression = round(sum(c(sd_score_snfi, shelter_quality_score_snfi, shelter_condition_score_snfi, 
                                        shelter_damage_score_snfi, security_tenure_score_snfi, nfi_score_snfi), 
                                        na.rm = T) / 11.5)) %>%
  
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
  
  ## fsl -- cost_score_fsl not counted in the score
  new_recoding(target = fsl_score) %>%
  recode_directly(to_expression = round(sum(c(source_score_fsl, availability_score_fsl, sufficency_score_fsl, 
                                              consumption_score_fsl, capacity_score_fsl, income_score_fsl, 
                                              assets_score_fsl), na.rm = T) / 21.5)) %>%
  
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
  #7.9 hygiene awareness
  new_recoding(target = hygiene_awareness_score_wash) %>%
  recode_directly(to_expression = sum(c(hygiene_awareness_score), na.rm = T)) %>%
  #7.10 availability hygienic materials 
  new_recoding(target = hygiene_mats_availability_score_wash) %>%
  recode_directly(to_expression = sum(c(soap_access_score, menstrual_mat_access_score), na.rm = T)) %>%
  #7.11 access to handwashing facilities
  new_recoding(target = access_handwashing_score_wash) %>%
  recode_directly(to_expression = sum(c(handwashing_access_score), na.rm = T)) %>%
  #7.12 aap wash
  new_recoding(target = aap_score_wash) %>%
  recode_directly(to_expression = sum(c(aap_water_source_score, aap_sanitation_score, aap_satisfaction_score), na.rm = T)) %>%
  
  ##wash score -- cost_score_wash,  hygiene_awareness_score_wash not counted in the score
  new_recoding(target = wash_score) %>%
  recode_directly(to_expression = round(sum(c(improved_water_source_score_wash, suffiency_score_wash, safe_storage_score_wash,
                                        latrine_use_score_wash, dignified_latrine_score_wash, 
                                        access_latrine_score_wash, environmental_sanitation_score_wash,
                                        hygiene_mats_availability_score_wash, access_handwashing_score_wash, aap_score_wash), 
                                        na.rm = T) / 40)) %>%
  
  #prot
  #8.1 freedom of movement
  new_recoding(target = freedom_movement_score_prot) %>%
  recode_directly(to_expression = sum(c(freedom_movement_score), na.rm = T)) %>%
  #8.2 family separation 
  new_recoding(target = family_separation_score_prot) %>%
  recode_directly(to_expression = sum(c(family_separation_score), na.rm = T)) %>%
  #8.3 safety and security
  new_recoding(target = safety_security_score_prot) %>%
  recode_directly(to_expression = sum(c(safety_security_concern_score), na.rm = T)) %>%
  #8.4 hazardous work
  new_recoding(target = hazardous_work_score_prot) %>%
  recode_directly(to_expression = sum(c(hazardous_work_score), na.rm = T)) %>%
  #8.5 hlp
  new_recoding(target = hlp_score_prot) %>%
  recode_directly(to_expression = sum(c(land_ownership_score, hlp_resolution_score, land_seizure_score), na.rm = T)) %>%
  #8.6 GBV
  new_recoding(target = gbv_score_prot) %>%
  recode_directly(to_expression = sum(c(sgbv_referral_score, sgbv_justice_recourse_score), na.rm = T)) %>%
  #8.7 rule of law
  new_recoding(target = rule_of_law_score_prot) %>%
  recode_directly(to_expression = sum(c(access_judicial_remedy_score, justice_recourse_score), na.rm = T)) %>%
  #8.8 child protection
  new_recoding(target = child_protection_score_prot) %>%
  recode_directly(to_expression = sum(c(child_injury_score, cfs_score), na.rm = T)) %>%
  #8.9 exploitation
  new_recoding(target = exploitation_score_prot) %>%
  recode_directly(to_expression = sum(c(exploitation_score), na.rm = T)) %>%
  #8.10 representation of women
  new_recoding(target = representation_women_score_prot) %>%
  recode_directly(to_expression = sum(c(women_committee_score), na.rm = T)) %>%
  #8.11 host community idp relation
  new_recoding(target = hc_idp_relation_score_prot) %>%
  recode_directly(to_expression = sum(c(relation_hc_idp_score), na.rm = T)) %>%
  
  ## prot score
  new_recoding(target = prot_score) %>%
  recode_directly(to_expression = round(sum(c(freedom_movement_score_prot, family_separation_score_prot, safety_security_score_prot,
                                        hazardous_work_score_prot, hlp_score_prot, gbv_score_prot, rule_of_law_score_prot, 
                                        child_protection_score_prot, exploitation_score_prot, representation_women_score_prot,
                                        hc_idp_relation_score_prot), na.rm = T) / 26.5)) %>%
  
  #mcsi
  
  ## mcsi score
  new_recoding(target = mcsi_score) %>%
  recode_directly(to_expression = round(sum(c(mcsi_water_score, mcsi_sanitation_score, mcsi_hygienic_mat_score, 
                                        mcsi_food_score, mcsi_shelter_score, mcsi_nfi_score, mcsi_education_score,
                                        mcsi_health_score), na.rm = T) / 16)) %>%
  
  #impact
  ## impact score
  new_recoding(target = impact_score) %>%
  ## for the time being IPC is being removed so it cannot be scored to 25.5
  # recode_directly(to_expression = round(sum(c(drought_score, conflict_environment_score, reason_separation_score,
  #                                             reason_lost_employment_score, reason_shelter_damage_score, reason_displacement_score,
  #                                             barriers_hum_score, ipc_score), na.rm = T) / 12.75)) %>%
  recode_directly(to_expression = round(sum(c(drought_score, conflict_environment_score, reason_separation_score,
                                              reason_lost_employment_score, reason_shelter_damage_score, reason_displacement_score,
                                              barriers_hum_score), na.rm = T) / 10.75)) %>%
                                              
   
  end_recoding()


all_scores <- c("pev_score", "edu_score", "nut_score", "health_score", "snfi_score", "fsl_score", "wash_score", 
                "prot_score", "mcsi_score", "impact_score")
new_order <- c(names(response)[!(names(response) %in% all_scores)], all_scores)
response <- response[, new_order] 


lapply(all_scores, function(x) table(response[,x], useNA = "ifany"))

sum(response[, all_scores] == 0)
response$pev_score <- ifelse(response$pev_score == 0, 1, response$pev_score)
response$edu_score <- ifelse(response$edu_score == 0, 1, response$edu_score)
response$nut_score <- ifelse(response$nut_score == 0, 1, response$nut_score)
response$health_score <- ifelse(response$health_score == 0, 1, response$health_score)
response$snfi_score <- ifelse(response$snfi_score == 0, 1, response$snfi_score)
response$fsl_score <- ifelse(response$fsl_score == 0, 1, response$fsl_score)
response$wash_score <- ifelse(response$wash_score == 0, 1, response$wash_score)
response$prot_score <- ifelse(response$prot_score == 0, 1, response$prot_score)
response$mcsi_score <- ifelse(response$mcsi_score == 0, 1, response$mcsi_score)
response$impact_score <- ifelse(response$impact_score == 0, 1, response$impact_score)
sum(response[, all_scores] == 0)


response$msni <- msni(education_lsg = response$edu_score, 
                       fsl_lsg = response$fsl_score, 
                       health_lsg = response$health_score,
                       protection_lsg = response$prot_score,
                       shelter_lsg = response$snfi_score,
                       wash_lsg = response$wash_score,
                       capacity_gaps = response$mcsi_score,
                       impact = response$impact_score)


response$msni %>% table(useNA = "ifany")


change_2_cat <- function(df, lsg_2_cat) {
  new_col <- rep(NA, nrow(df))
  new_col <- ifelse(df[[lsg_2_cat]] < 3, "not_in_need", "in_need")
  return(new_col)
}

binary_scores <- lapply(X = c(all_scores, "msni"), FUN = change_2_cat, df = response) %>% do.call(cbind, .) %>% data.frame(stringsAsFactors = F)
names(binary_scores) <- paste0(c(all_scores, "msni"), "_2_cat")
response <- cbind(response, binary_scores)

lsg <- c("edu_score", "nut_score", "health_score", "snfi_score", "fsl_score", "wash_score", "prot_score")

response$num_above_sev_3 <- rowSums(response[, lsg] > 2)
response$at_least_lsg_above_sev_3 <- NA
response$at_least_lsg_above_sev_3[response$num_above_sev_3 == 0] <- FALSE
response$at_least_lsg_above_sev_3[response$num_above_sev_3 > 0] <- TRUE

response$mcsi_score_2_cat %>% table()
response$lsg_cg <- NA
response$lsg_cg[response$at_least_lsg_above_sev_3 == T & response$mcsi_score_2_cat == "not_in_need"] <- "one_lsg_no_cg"
response$lsg_cg[response$at_least_lsg_above_sev_3 == T & response$mcsi_score_2_cat == "in_need"] <- "one_lsg_one_cg"
response$lsg_cg[response$at_least_lsg_above_sev_3 == F & response$mcsi_score_2_cat == "in_need"] <- "no_lsg_one_cg"
response$lsg_cg[response$at_least_lsg_above_sev_3 == F & response$mcsi_score_2_cat == "not_in_need"] <- "no_lsg_no_cg"


response$lsg_or_cg <- FALSE
response$lsg_or_cg[response$at_least_lsg_above_sev_3 == T | response$mcsi_score_2_cat == "in_need"] <- TRUE



response <- response %>%
  ## pev_score
  new_recoding(target = pev_score_no_disp) %>%
    recode_directly(to_expression = round(sum(c(hhvulnerability_score_pev, legal_status_score_pev, dependency_levels_score_pev, 
                                                poverty_score_pev, hhexpenditure_score_pev), na.rm = T) / 19)) %>%
  #impact
  ## impact score
  new_recoding(target = impact_score_no_disp) %>%
  recode_directly(to_expression = round(sum(c(drought_score, conflict_environment_score, reason_separation_score,
                                              reason_lost_employment_score, reason_shelter_damage_score, barriers_hum_score), 
                                            na.rm = T) / 9)) %>%  
  end_recoding()
response$pev_score_no_disp <- ifelse(response$pev_score_no_disp == 0, 1, response$pev_score_no_disp)
response$impact_score_no_disp <- ifelse(response$impact_score_no_disp == 0, 1, response$impact_score_no_disp)

