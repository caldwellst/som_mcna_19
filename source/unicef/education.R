## education calculation

un_response <- 
  un_response %>%
  #1.1.1 education degreese
  new_recoding(target = education_level_score) %>%
  recode_to(to = 5, where = tertiary_degree == "none" & vocational_degree == "none" & 
              secondary_degree == "none" & primary_degree == "none") %>%
  recode_to(to = 4, where = primary_degree == "one") %>%
  recode_to(to = 3, where = vocational_degree == "one") %>%
  recode_to(to = 2, where = secondary_degree == "one" | primary_degree == "two_more" | vocational_degree == "two_more") %>%
  recode_to(to = 1, where = tertiary_degree == "one" | tertiary_degree == "two_more" | secondary_degree == "two_more") %>%
  #1.1.2 enrollment rate
  new_recoding(target = enrollement_rate) %>%
  recode_directly(to_expression = enrolled_total / school_age_total) %>%
  ### family with no school age children?
  # recode_directly(ifelse(is.na(enrollement_rate), 0, enrollement_rate)) %>%
  new_recoding(target = enrollement_rate_score) %>%
  recode_to(to = 1, where = enrollement_rate > .6) %>%
  recode_to(to = 2, where = enrollement_rate <= .6 & enrollement_rate > 0.40) %>%
  recode_to(to = 3, where = enrollement_rate <= .40 & enrollement_rate > 0.20) %>%
  recode_to(to = 4, where = enrollement_rate <= .2 & enrollement_rate > 0) %>%
  recode_to(to = 5, where = enrollement_rate == 0) %>%
  #1.1.3 attendance rate
  new_recoding(target = attendance_rate) %>%
  recode_directly(to_expression = attend_total / school_age_total) %>%
  ### family with no children?
  # recode_directly(ifelse(is.na(attendance_rate), 0, attendance_rate)) %>%
  # verify with complete dataset if attendace number are corrected
  new_recoding(target = attendance_rate_score) %>%
  recode_to(to = 1, where = attendance_rate > .6) %>%
  recode_to(to = 2, where = attendance_rate <= .6 & attendance_rate > 0.40) %>%
  recode_to(to = 3, where = attendance_rate <= .40 & attendance_rate > 0.20) %>%
  recode_to(to = 4, where = attendance_rate <= .2 & attendance_rate > 0) %>%
  recode_to(to = 5, where = attendance_rate == 0) %>%
  # #2.2 drop-outs
  new_recoding(target = drop_out_score) %>%
  recode_to(to = 1, where.selected.exactly = "none", source = drop_out) %>%
  recode_to(to = 3, where.selected.exactly = "some", source = drop_out) %>%
  recode_to(to = 5, where.selected.exactly = "all", source = drop_out) %>%
  # recode_to(to = NA, where = school_age_total == 0) %>% not working, have to add condition in the previous lines if not
  # corrected in the dataset. something like recode_to(to =1, where = drop_out == "none" & school_age_total > 0)
  # seems NA is not a usable value
  #1.2 coping mechanisms
  new_recoding(target = education_coping_sum) %>%
  recode_directly(to_expression = 0) %>%
  recode_directly(to_expression = 
                    coping_education.borrow_mat_cash * 1 +
                    coping_education.peer_learning * 2 +
                    coping_education.part_time_school * 2 +
                    coping_education.extra_time_secure * 2 + 
                    coping_education.home_school * 3 + 
                    coping_education.hum_assistance * 3 +
                    coping_education.adult_extra_job * 4 + 
                    coping_education.use_money_other * 4 + 
                    coping_education.sell_assets_other * 5 +
                    coping_education.travel_insecure * 7 +
                    coping_education.adults_beg * 7 +
                    coping_education.minors_work * 7 +
                    # coping_education.minors_beg * 8 +
                    coping_education.exploit_hum * 8) %>%
  new_recoding(target = education_coping_score, source = education_coping_sum) %>%
  recode_to(to = 1, where.num.smaller.equal = 2) %>%
  recode_to(to = 2, where.num.larger.equal = 3) %>%
  recode_to(to = 3, where.num.larger.equal = 7) %>%
  recode_to(to = 4, where.num.larger.equal = 13) %>%
  recode_to(to = 5, where.num.larger.equal = 21) %>%
  new_recoding(target = education_lvgs_median) %>%
  recode_directly(round(median(c(education_level_score, enrollement_rate_score, attendance_rate_score, drop_out_score), na.rm = T))) %>%
  new_recoding(target = education_coping_median) %>%
  recode_directly(round(median(c(education_coping_score), na.rm = T))) %>%
  new_recoding(target = education_phywell_median) %>%
  recode_directly(1) %>%
  end_recoding()

# 
# un_response <- 
#   un_response %>%
#   new_recoding(target = education_humanitarian_condition) %>%
#   recode_directly(ifelse(education_phywell_median > education_lvgs_median | education_phywell_median > education_coping_median, 
#                          as.character(education_phywell_median),
#                          as.character(lookup_table[education_lvgs_median, education_coping_median]))) %>%
#   new_recoding(target = education_pin) %>%
#   recode_directly(ifelse(education_humanitarian_condition %in% c("3", "4", "5"), "in need", "not inneed")) %>%
#   end_recoding()
# 
# short_df <- un_response  %>%
#   select(region, settlement, yes_no_host, strata, education_humanitarian_condition, education_pin)
#   
# analysisplan<-make_analysisplan_all_vars(short_df,
#                                          questionnaire,
#                                          independent.variable = "yes_no_host",
#                                          repeat.for.variable = "region",
#                                          hypothesis.type = "group_difference") 
# analysisplan <- analysisplan[c(5:6), ]
# 
# strata_weight_fun <- map_to_weighting(sampling.frame = samplingframe,
#                                       sampling.frame.population.column = "Population",
#                                       sampling.frame.stratum.column = "strata",
#                                       data.stratum.column = "strata")
# 
# short_df$general_weights <- strata_weight_fun(short_df)
# 
# results <- from_analysisplan_map_to_output(short_df, 
#                                            analysisplan = analysisplan,
#                                            weighting = strata_weight_fun,
#                                            cluster_variable_name = "settlement",
#                                            questionnaire)
# 
# 
# hypegrammaR:::map_to_generic_hierarchical_html(results,
#                                                render_result_with = hypegrammaR:::from_result_map_to_md_table,
#                                                by_analysisplan_columns = c("dependent.var","repeat.var.value"),
#                                                by_prefix =  c("",""),
#                                                level = 2,
#                                                questionnaire = questionnaire,
#                                                label_varnames = TRUE,
#                                                dir = "./output",
#                                                filename = "summary_by_dependent_var_then_by_repeat_var.html"
# )
# browseURL("summary_by_dependent_var_then_by_repeat_var.html")
# 
# big_table <- results$results %>% lapply(function(x) x[["summary.statistic"]]) %>% do.call(rbind, .)
# 
# #pin disagregation geographic area / affected group
# pin_table <- big_table %>%
#   select(-c(se, min, max)) %>%
#   filter(dependent.var == "education_pin", 
#          dependent.var.value == "in need") %>%
#   spread(independent.var.value, numbers) %>%
#   select(region = repeat.var.value,
#          hc = yes,
#          idp = no)
# 
# #severity phases
# ## creating table
# severity_phase <- big_table %>%
#   select(-c(se, min, max)) %>%
#   filter(dependent.var == "education_humanitarian_condition") %>%
#   spread(dependent.var.value, numbers) %>%
#   mutate(independent.var.value = as.character(independent.var.value)) %>%
#   select(-c(dependent.var, independent.var, repeat.var)) %>%
#   rename(region = repeat.var.value,
#          population_group = independent.var.value) %>%
#   arrange(region)
# 
# ## renaming population group
# severity_phase$population_group[severity_phase$population_group == "yes"] <- "hc"
# severity_phase$population_group[severity_phase$population_group == "no"] <- "idp"
# 
# 
# ## adding missing severity phase
# if(sum(c(1:5) %in% names(severity_phase)) != 5) {
#   missing_severity_class <- c(1:5)[!(c(1:5) %in% names(severity_phase))]
#   missing_severity_class <- as.character(missing_severity_class)
#   severity_phase[, missing_severity_class] <- 0
# }
# severity_phase[is.na(severity_phase)] <- 0
# 
# ## adding "s" to the severity score names to avoid confusion
# names(severity_phase)[names(severity_phase) == "1"] <- "s1"
# names(severity_phase)[names(severity_phase) == "2"] <- "s2"
# names(severity_phase)[names(severity_phase) == "3"] <- "s3"
# names(severity_phase)[names(severity_phase) == "4"] <- "s4"
# names(severity_phase)[names(severity_phase) == "5"] <- "s5"
# 
# severity_phase
# ##selection of the phase
# severity_phase <- severity_phase %>%
#   new_recoding(target = education_severity_phase) %>%
#   recode_directly(ifelse(s5 > .2, 5,
#                          ifelse((s4 + s5) > 0.2, 4, 
#                                 ifelse((s3 + s4 + s5) > 0.2, 3,
#                                        ifelse((s2 + s3 + s4 + s5) > 0.2, 2,
#                                               1))))) %>%
#   end_recoding()
# severity_phase
# data_frame_to_write <- list(un_response, pin_table, severity_phase, big_table)
# names_data_frame_to_write <- c("dataset.csv", "pin_table.csv", "severity_phase.csv", "big_table.csv")
# names_data_frame_to_write <- paste0("output/unicef/", names_data_frame_to_write)
# 
# mapply(write.csv, x = data_frame_to_write, file = names_data_frame_to_write, row.names = F)
# file.copy(from = "output/summary_by_dependent_var_then_by_repeat_var.html", to = "output/unicef/")


### wash correction
un_response <- 
  un_response %>%
  new_recoding(target = education_humanitarian_condition) %>%
  recode_directly(ifelse(education_phywell_median > education_lvgs_median | education_phywell_median > education_coping_median, 
                         education_phywell_median,
                         lookup_table[education_lvgs_median, education_coping_median])) %>%
  new_recoding(target = education_pin) %>%
  recode_directly(ifelse(education_humanitarian_condition >= 3, "in need", "not inneed")) %>%
  new_recoding(target = education_severe_pin, source = education_humanitarian_condition) %>%
  recode_to(to = "severe pin", where.num.larger.equal = 4) %>%
  recode_to(to = "pin", where.num.equal = 3) %>%
  recode_to(to = "not pin", where.num.smaller.equal = 2) %>%
  end_recoding()

un_response$education_humanitarian_condition <- as.character(un_response$education_humanitarian_condition)
short_df <- un_response  %>%
  select(region, settlement, yes_no_host, strata, education_humanitarian_condition, education_pin, education_severe_pin)

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
  filter(dependent.var == "education_pin", 
         dependent.var.value == "in need") %>%
  spread(independent.var.value, numbers) %>%
  select(region = repeat.var.value,
         hc = yes,
         idp = no)

#severity phases
## creating table
severity_phase <- big_table %>%
  select(-c(se, min, max)) %>%
  filter(dependent.var == "education_humanitarian_condition") %>%
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
names_data_frame_to_write <- paste0("output/unicef/education/", names_data_frame_to_write)

mapply(write.csv, x = data_frame_to_write, file = names_data_frame_to_write, row.names = F)
file.copy(from = "output/summary_by_dependent_var_then_by_repeat_var.html", to = "output/unicef/education/")
