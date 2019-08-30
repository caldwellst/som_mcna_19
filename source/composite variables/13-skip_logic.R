# horizontal aggregation
## skip logic
#education
hh_without_school_age_children <- response$school_age_total == 0

response$enrollement_rate_score[hh_without_school_age_children] <- NA
response$drop_out_score[hh_without_school_age_children] <- NA
response$attendance_rate_score[hh_without_school_age_children] <- NA
response$attend_previous_score[hh_without_school_age_children] <- NA
response$education_expenditure_change_score[hh_without_school_age_children] <- NA
response$access_to_school_score[hh_without_school_age_children] <- NA

#nutrition
hh_without_plw_u5 <- response$plw != "yes" & response$children_0_4 == 0

response$muac_coverage_score[hh_without_plw_u5] <- NA
response$supplements_score[hh_without_plw_u5] <- NA


hh_without_children <- response$total_children == 0

response$children_enrolled_nutrition_centers_score[hh_without_children] <- NA

#health
hh_without_u5 <- response$children_0_4 == 0

response$u5_morbidity_score[hh_without_u5] <- NA


hh_without_plw <- response$plw != "yes"

response$pregnancy_health_score[hh_without_plw] <- NA
response$birth_location_score[hh_without_plw] <- NA

hh_without_vaccine_age_children <- response$children_vaccine_age == 0


response$vaccination_score[hh_without_vaccine_age_children] <- NA

#fsl
hh_without_income <- response$income_source == "none"
response$lost_income_score[hh_without_income] <- NA

hh_without_livestock <- response$own_livestock == "no"
response$lost_livestock_score[hh_without_livestock] <- NA

hh_without_land_cultivation <- response$land_cultivation == "no"
response$lost_land_cultivation_score[hh_without_land_cultivation] <- NA

hh_without_jerry_can <- response$how_water_stored.jerry_can == 0
response$jerrycan_quality_score[hh_without_jerry_can] <- NA

hh_without_latrines <- response$household_access_latrine == "no_latrine"
response$latrine_type_score[hh_without_latrines] <- NA
response$gender_separation_latrine_score[hh_without_latrines] <- NA
response$pwd_access_latrine_score[hh_without_latrines] <- NA
response$lock_latrine_score[hh_without_latrines] <- NA
response$soap_availability_latrine_score[hh_without_latrines] <- NA
response$internal_light_latrine_score[hh_without_latrines] <- NA
response$hygiene_latrine_score[hh_without_latrines] <- NA
response$distance_latrine_score[hh_without_latrines] <- NA
response$latrine_type_score[hh_without_latrines] <- NA
response$latrine_type_score[hh_without_latrines] <- NA

#protection

hh_without_gbv_consent <- response$sgbv_consent == "no"
response$sgbv_referral_score[hh_without_gbv_consent] <- NA
response$sgbv_justice_recourse_score[hh_without_gbv_consent] <- NA

response$child_injury_score[hh_without_children] <- NA
response$cfs_score[hh_without_children] <- NA


