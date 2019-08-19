## wash calculation

un_response <- 
  un_response %>%
  #1.1.1 Access to an improved water source 
  new_recoding(target = access_to_improved_watersource_score) %>%
  recode_to(to = 1, where = primary_source_drinking_water %in% c("piped_system", "protected_well_no_hand_pump", 
                                                          "protected_well_with_hand_pump", "tank_and_tap", "borehole") & 
              time_to_reach_water_source == "less15") %>%
  recode_to(to = 2, where = primary_source_drinking_water %in% c("piped_system", "protected_well_no_hand_pump",
                                                                 "protected_well_with_hand_pump", "tank_and_tap", "borehole") & 
              time_to_reach_water_source == "16_30") %>%
  recode_to(to = 3, where = primary_source_drinking_water %in% c("piped_system", "protected_well_no_hand_pump",
                                                                 "protected_well_with_hand_pump", "tank_and_tap", "borehole") & 
              time_to_reach_water_source %in% c("31_60", "60_180", "above180")) %>%
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
  new_recoding(target = access_to_suff_qty_water_score) %>%
  recode_to(to = 5, where = enough_drinking_water == "no" & enough_cooking_water == "no") %>%
  recode_to(to = 4, where = (enough_drinking_water == "yes" | enough_cooking_water == "yes") &
              water_storage_capacity == "not enough storage") %>%
  recode_to(to = 3, where = (enough_drinking_water == "yes" | enough_cooking_water == "yes") &
              water_storage_capacity == "enough storage") %>%
  recode_to(to = 2, where = enough_drinking_water == "yes" & enough_cooking_water == "yes" &
              water_storage_capacity == "not enough storage") %>%
  recode_to(to = 1, where = enough_drinking_water == "yes" & enough_cooking_water == "yes" &
              water_storage_capacity == "enough storage") %>%
  #1.1.3.	Access to adequate, appropriate and functional sanitation facilities 
  new_recoding(target = access_sanitation_score) %>%
  recode_to(to = 5, where = household_access_latrine == "no_latrine" ) %>%
  recode_to(to = 4, where = latrine_type %in% c("flush_unimproved", "pit_unimproved", "other") & sharing_latrines == "yes") %>%
  recode_to(to = 3, where = latrine_type %in% c("flush_unimproved", "pit_unimproved", "other") &
                            (sharing_latrines == "no" | household_access_latrine == "yes_personal")) %>%
  recode_to(to = 2, where = latrine_type %in% c("flush_improved", "pit_improved") & sharing_latrines == "yes") %>%
  recode_to(to = 1, where = latrine_type %in% c("flush_improved", "pit_improved") &
              (sharing_latrines == "no" | household_access_latrine == "yes_personal")) %>%
  #1.1.4.	Access to functional handwashing facilities and soap
  new_recoding(target = access_handwashing_score) %>%
  recode_to(to = 5, where = access_to_soap == "no" & functional_handwashing == "no") %>%
  recode_to(to = 5, where = access_to_soap == "no" & functional_handwashing == "") %>%
  recode_to(to = 3, where = access_to_soap == "yes" | functional_handwashing == "yes") %>%
  recode_to(to = 1, where = access_to_soap == "yes" & functional_handwashing == "yes") %>%
  #1.1.5.	Access to menstrual hygiene material
  new_recoding(target = menstrual_hygiene_score, source = hygienic_menstruction_materials) %>%
  recode_to(to = 2, where.selected.exactly = "yes") %>%
  recode_to(to = 4, where.selected.exactly = "no") %>%
  # 1.1.6.	Access to environmental sanitation 
  new_recoding(target = environmental_sanitation_sum) %>%
  recode_directly(to_expression = sum(enviromental_sanitaiton_problems.decying_organic_matter, 
                                      enviromental_sanitaiton_problems.stagnent_water,
                                      enviromental_sanitaiton_problems.feacal_matter,
                                      enviromental_sanitaiton_problems.rodents,
                                      enviromental_sanitaiton_problems.solid_household_waste)) %>%
  ## environmental contamination score
  new_recoding(target = environmental_sanitation_score, source = environmental_sanitation_sum) %>%
  recode_to(to = 1, where.num.equal = 0) %>%
  recode_to(to = 2, where.num.equal = 1) %>%
  recode_to(to = 3, where.num.equal = 2) %>%
  recode_to(to = 4, where.num.equal = 3) %>%
  recode_to(to = 5, where.num.larger.equal = 4) %>%
  #1.1.7.	Availability of healthcare facilities
  new_recoding(target = availability_health_care_score, source = time_health) %>% 
  recode_to(to = 1, where.selected.exactly = "less15") %>%
  recode_to(to = 2, where.selected.exactly = "16_30") %>%
  recode_to(to = 3, where.selected.exactly = "31_60") %>%
  recode_to(to = 4, where.selected.exactly = "60_180") %>%
  recode_to(to = 5, where.selected.exactly = "above180") %>%
  # 1.2.1.	Water Coping Sub-Index
  new_recoding(target = water_coping_sum) %>%
  recode_directly(to_expression = 0) %>%
  recode_directly(to_expression = 
                    coping_water.seasonal_source * 1 +
                    coping_water.borrow_mat_cash * 1 +
                    coping_water.adult_extra_job * 1 + 
                    coping_water.extra_time_secure * 1 + 
                    coping_water.use_money_other * 1 + 
                    
                    coping_water.reduce_domestic_water * 2 +
                    coping_water.children_fetch_water * 2 +
                    coping_water.reduce_adult_cons * 2 + 
                    coping_water.drink_unsafe_water * 2 +
                    coping_water.hum_assistance * 2 +
                    coping_water.sell_assets_other * 2 +
                    coping_water.adults_beg * 2 +
                    coping_water.minors_work * 2 +
                    
                    coping_water.reduce_drinking_water * 3 +
                    coping_water.travel_insecure * 3 +
                    coping_water.minors_beg * 3 +
                    coping_water.exploit_hum * 3) %>%
  new_recoding(water_coping_score, source = water_coping_sum) %>%
  recode_to(to = 5, where.num.larger.equal = 14) %>%
  recode_to(to = 4, where.num.smaller = 14) %>%
  recode_to(to = 3, where.num.smaller = 9) %>%
  recode_to(to = 2, where.num.smaller = 5) %>%
  recode_to(to = 1, where.num.smaller = 2) %>%
  #1.2.2.	Sanitation coping strategies 
  new_recoding(sanitation_coping_sum) %>%
  recode_directly(to_expression = 0) %>%
  recode_directly(to_expression = 
                    coping_sanitation.share_latrines * 1 +
                    coping_sanitation.use_unhygienic * 1 +
                    coping_sanitation.extra_time_secure * 1 +
                    coping_sanitation.use_money_other * 1 +
                    
                    coping_sanitation.use_insecure * 2 +
                    coping_sanitation.hum_assistance * 2 +
                    
                    coping_sanitation.open_defaecation * 3 +
                    coping_sanitation.travel_insecure * 3 +
                    coping_sanitation.exploit_hum * 3) %>%
  new_recoding(target = sanitation_coping_score, source = sanitation_coping_sum) %>%
  recode_to(to = 5, where.num.larger.equal = 14) %>%
  recode_to(to = 4, where.num.smaller = 14) %>%
  recode_to(to = 3, where.num.smaller = 9) %>%
  recode_to(to = 2, where.num.smaller = 5) %>%
  recode_to(to = 1, where.num.smaller = 2) %>%
  #1.2.3.	Hygiene coping strategies  
  new_recoding(hygiene_coping_sum) %>%
  recode_directly(to_expression = 0) %>%
  recode_directly(to_expression = 
                    coping_hygiene.soap_sub_cloth * 1 +
                    coping_hygiene.borrow_mat_cash * 1 +
                    coping_hygiene.adult_extra_job * 1 +
                    coping_hygiene.extra_time_secure * 1 +
                    coping_hygiene.use_money_other * 1 +
                    
                    coping_hygiene.wash_less * 2 +
                    coping_hygiene.no_wash_menstrual * 2 +
                    coping_hygiene.no_soap_hands * 2 +
                    coping_hygiene.bathe_latrine * 2 +
                    coping_hygiene.hum_assistance * 2 +
                    coping_hygiene.sell_assets_other * 2 +
                    coping_hygiene.adults_beg * 2 +
                    coping_hygiene.minors_work * 2 +
                    
                    coping_hygiene.no_use_menstrual * 3 +
                    coping_hygiene.no_wash_hands * 3 +
                    coping_hygiene.travel_insecure * 3 +
                    coping_hygiene.minors_beg * 3 +
                    coping_hygiene.exploit_hum * 3) %>%
  new_recoding(target = hygiene_coping_score, source = hygiene_coping_sum) %>%
  recode_to(to = 5, where.num.larger.equal = 14) %>%
  recode_to(to = 4, where.num.smaller = 14) %>%
  recode_to(to = 3, where.num.smaller = 9) %>%
  recode_to(to = 2, where.num.smaller = 5) %>%
  recode_to(to = 1, where.num.smaller = 2) %>%
  #1.3.3.	Self-reported WASH-related morbidity in children under five
  new_recoding(target = self_reported_water_born_disease_sum) %>%
  recode_directly(to_expression = child_health.fever + child_health.malaria + child_health.awd + child_health.malnutrition) %>%
  new_recoding(target = self_reported_morbidity_score, source = self_reported_water_born_disease_sum) %>%
  recode_to(to = 1, where.num.equal = 0) %>%
  recode_to(to = 3, where.num.equal = 1) %>%
  recode_to(to = 5, where.num.larger.equal = 2) %>%
  # 1.3.4.	WASH Safety Index
  new_recoding(target = wash_safety_sum) %>%
  recode_directly(to_expression = 
                    (latrines_locked_from_inside == "no"| household_access_latrine == "no_latrine") * 2 +
                    (latrines_have_light == "no"| household_access_latrine == "no_latrine") * 1 +
                    (latrines_seperated_by_gender == "no"| household_access_latrine == "no_latrine") * 1 +
                    (time_to_reach_latrine != "less15" | household_access_latrine == "no_latrine") * 3 +
                    ifelse(time_to_reach_water_source %in% c("31_60", "60_180", "above180"), 1, 0) * 1 +
                    ifelse(household_been_consulted_water == "no", 1, 0) * 1 +
                    ifelse(household_been_consulted_sanitaiton == "no", 1, 0) * 2 +
                    ifelse(water_sourcces_well_developed == "no", 1, 0) * 1 +
                    ifelse(water_access_barrier_first == "insecurity ", 1, 0) * 3 +
                    ifelse(water_access_barrier_second == "insecurity ", 1, 0) * 2 +
                    ifelse(water_access_barrier_third == "insecurity ", 1, 0) * 1 +
                    ifelse(latrine_access_barrier_first %in% c("in_security_traveling", "in_security_within", 
                                             "not_accessible_to_disabled", "not_accessible_to_minority", "quality2", 
                                             "o_wall_light", "no_lock", "crowded"), 1, 0) * 3 +
                    ifelse(latrine_access_barrier_second %in% c("in_security_traveling", "in_security_within", 
                                                               "not_accessible_to_disabled", "not_accessible_to_minority", "quality2", 
                                                               "o_wall_light", "no_lock", "crowded"), 1, 0) * 2 +
                    ifelse(latrine_access_barrier_third %in% c("in_security_traveling", "in_security_within", 
                                                               "not_accessible_to_disabled", "not_accessible_to_minority", "quality2", 
                                                               "o_wall_light", "no_lock", "crowded"), 1, 0) * 1 +
                    ifelse(dificulty_obtaining_soap == "insecurity_traveling", 1, 0) * 2 + 
                    ifelse(dificulty_obtaining_menstruation == "insecurity_traveling", 1, 0) * 2) %>%
  new_recoding(target = wash_safety_score, source = wash_safety_sum) %>%
  recode_to(to = 5, where.num.larger.equal = 21) %>%
  recode_to(to = 4, where.num.smaller = 21) %>%
  recode_to(to = 3, where.num.smaller = 16) %>%
  recode_to(to = 2, where.num.smaller = 11) %>%
  recode_to(to = 1, where.num.smaller = 6) %>%
  new_recoding(target = wash_lvgs_median) %>%
  recode_directly(to_expression = round(median(c(access_to_improved_watersource_score, access_to_suff_qty_water_score, 
                                         access_sanitation_score, access_handwashing_score, menstrual_hygiene_score,
                                         environmental_sanitation_score, availability_health_care_score), na.rm = T))) %>%
  new_recoding(target = wash_coping_median) %>%
  recode_directly(to_expression = round(median(c(water_coping_score, sanitation_coping_score, hygiene_coping_score), na.rm = T))) %>%
  new_recoding(target = wash_phywell_median) %>%
  recode_directly(to_expression = round(median(c(self_reported_morbidity_score, wash_safety_score), na.rm = T))) %>%
  end_recoding() 

un_response <- 
  un_response %>%
  new_recoding(target = wash_humanitarian_condition) %>%
  recode_directly(ifelse(wash_phywell_median > wash_lvgs_median | wash_phywell_median > wash_coping_median, 
                         wash_phywell_median,
                         lookup_table[wash_lvgs_median, wash_coping_median])) %>%
  new_recoding(target = wash_pin) %>%
  recode_directly(ifelse(wash_humanitarian_condition >= 3, "in need", "not inneed")) %>%
  new_recoding(target = wash_severe_pin, source = wash_humanitarian_condition) %>%
  recode_to(to = "severe pin", where.num.larger.equal = 4) %>%
  recode_to(to = "pin", where.num.equal = 3) %>%
  recode_to(to = "not pin", where.num.smaller.equal = 2) %>%
  end_recoding()

un_response$wash_humanitarian_condition <- as.character(un_response$wash_humanitarian_condition)
short_df <- un_response  %>%
  select(region, settlement, yes_no_host, strata, wash_humanitarian_condition, wash_pin, wash_severe_pin)

analysisplan<-make_analysisplan_all_vars(short_df,
                                         questionnaire,
                                         independent.variable = "yes_no_host",
                                         repeat.for.variable = "region",
                                         hypothesis.type = "group_difference") 
analysisplan <- analysisplan[c(5:7), ]
analysisplan

strata_weight_fun <- map_to_weighting(sampling.frame = samplingframe,
                                      sampling.frame.population.column = "Population",
                                      sampling.frame.stratum.column = "strata",
                                      data.stratum.column = "strata")

short_df$general_weights <- strata_weight_fun(short_df)

results <- from_analysisplan_map_to_output(short_df, 
                                           analysisplan = analysisplan,
                                           weighting = strata_weight_fun,
                                           cluster_variable_name = "settlement",
                                           questionnaire)


hypegrammaR:::map_to_generic_hierarchical_html(results,
                                               render_result_with = hypegrammaR:::from_result_map_to_md_table,
                                               by_analysisplan_columns = c("dependent.var","repeat.var.value"),
                                               by_prefix =  c("",""),
                                               level = 2,
                                               questionnaire = questionnaire,
                                               label_varnames = TRUE,
                                               dir = "./output",
                                               filename = "summary_by_dependent_var_then_by_repeat_var.html"
)
browseURL("summary_by_dependent_var_then_by_repeat_var.html")

big_table <- results$results %>% lapply(function(x) x[["summary.statistic"]]) %>% do.call(rbind, .)

#pin disagregation geographic area / affected group
pin_table <- big_table %>%
  select(-c(se, min, max)) %>%
  filter(dependent.var == "wash_pin", 
         dependent.var.value == "in need") %>%
  spread(independent.var.value, numbers) %>%
  select(region = repeat.var.value,
         hc = yes,
         idp = no)

#severity phases
## creating table
severity_phase <- big_table %>%
  select(-c(se, min, max)) %>%
  filter(dependent.var == "wash_humanitarian_condition") %>%
  spread(dependent.var.value, numbers) %>%
  mutate(independent.var.value = as.character(independent.var.value)) %>%
  select(-c(dependent.var, independent.var, repeat.var)) %>%
  rename(region = repeat.var.value,
         population_group = independent.var.value) %>%
  arrange(region)

## renaming population group
severity_phase$population_group[severity_phase$population_group == "yes"] <- "hc"
severity_phase$population_group[severity_phase$population_group == "no"] <- "idp"


## adding missing severity phase
if(sum(c(1:5) %in% names(severity_phase)) != 5) {
  missing_severity_class <- c(1:5)[!(c(1:5) %in% names(severity_phase))]
  missing_severity_class <- as.character(missing_severity_class)
  severity_phase[, missing_severity_class] <- 0
}
severity_phase[is.na(severity_phase)] <- 0

## adding "s" to the severity score names to avoid confusion
names(severity_phase)[names(severity_phase) == "1"] <- "s1"
names(severity_phase)[names(severity_phase) == "2"] <- "s2"
names(severity_phase)[names(severity_phase) == "3"] <- "s3"
names(severity_phase)[names(severity_phase) == "4"] <- "s4"
names(severity_phase)[names(severity_phase) == "5"] <- "s5"

severity_phase
##selection of the phase
severity_phase <- severity_phase %>%
  new_recoding(target = education_severity_phase) %>%
  recode_directly(ifelse(s5 > .2, 5,
                         ifelse((s4 + s5) > 0.2, 4, 
                                ifelse((s3 + s4 + s5) > 0.2, 3,
                                       ifelse((s2 + s3 + s4 + s5) > 0.2, 2,
                                              1))))) %>%
  end_recoding()
severity_phase
data_frame_to_write <- list(un_response, pin_table, severity_phase, big_table)
names_data_frame_to_write <- c("dataset.csv", "pin_table.csv", "severity_phase.csv", "big_table.csv")
names_data_frame_to_write <- paste0("output/unicef/wash/", names_data_frame_to_write)

mapply(write.csv, x = data_frame_to_write, file = names_data_frame_to_write, row.names = F)
file.copy(from = "output/summary_by_dependent_var_then_by_repeat_var.html", to = "output/unicef/wash/")
