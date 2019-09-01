# horizontal aggregation
## education

#correction values for dropout and hh with no children.

response <- response %>%
  mutate(drop_out = ifelse(school_age_total == 0, NA, drop_out))

response <- 
  response %>%
  #1.1 education level
  new_recoding(target = education_level_score) %>%
  recode_to(to = 6, where = tertiary_degree %in% c("none", "dnk") & vocational_degree %in% c("none", "dnk")  & 
              secondary_degree %in% c("none", "dnk")  & primary_degree %in% c("none", "dnk")) %>%
  recode_to(to = 5, where = primary_degree == "one") %>%
  recode_to(to = 4, where = secondary_degree == "one" | primary_degree == "two_more") %>%
  recode_to(to = 3, where = vocational_degree == "one" | secondary_degree == "two_more") %>%
  recode_to(to = 2, where = vocational_degree == "two_more") %>%
  recode_to(to = 1, where = tertiary_degree == "one" | tertiary_degree == "two_more") %>%
  #2.1 enrollment rate
  new_recoding(target = enrollement_rate) %>%
  recode_directly(to_expression = enrolled_total / school_age_total) %>%
  ### family with no children?
  # recode_directly(ifelse(is.na(enrollement_rate), 0, enrollement_rate)) %>% 
  new_recoding(target = enrollement_rate_score) %>%
  recode_to(to = 1, where.num.equal = 1, source = enrollement_rate) %>%
  recode_to(to = 2, where = enrollement_rate < 1 & enrollement_rate >= 0.75) %>%
  recode_to(to = 3, where = enrollement_rate < .75 & enrollement_rate >= 0.50) %>%
  recode_to(to = 4, where = enrollement_rate < .5 & enrollement_rate >= 0.25) %>%
  recode_to(to = 5, where = enrollement_rate < .25 & enrollement_rate >= 0) %>%
  # #2.2 drop-outs
  new_recoding(target = drop_out_score) %>%
  recode_to(to = 1, where.selected.exactly = "none", source = drop_out) %>%
  recode_to(to = 3, where.selected.exactly = "some", source = drop_out) %>%
  recode_to(to = 5, where.selected.exactly = "all", source = drop_out) %>%
  #2.3 attendance rate
  new_recoding(target = attendance_rate) %>%
  recode_directly(to_expression = attend_total / school_age_total) %>%
  ### family with no children?
  # recode_directly(ifelse(is.na(attendance_rate), 0, attendance_rate)) %>%
  new_recoding(target = attendance_rate_score) %>%
  recode_to(to = 1, where.num.equal = 1, source = attendance_rate) %>%
  recode_to(to = 3, where = attendance_rate < 1 & attendance_rate >= 0.75) %>%
  recode_to(to = 4, where = attendance_rate < .75 & attendance_rate >= 0.50) %>%
  recode_to(to = 5, where = attendance_rate < .5 & attendance_rate >= 0.25) %>%
  recode_to(to = 6, where = attendance_rate < .25 & attendance_rate >= 0) %>%
  #2.4 attend previous
  new_recoding(target = attend_previous_score, source = attend_previous) %>%
  recode_to(to = 1, where.selected.exactly = "yes") %>%
  recode_to(to = 6, where.selected.exactly = "no") %>%
  #3.1 education expenditure
  ## expenditure rate
  new_recoding(target = education_expenditure_rate) %>%
  recode_directly(to_expression = spent_education_middle / income_middle) %>%
  ## expenditure score
  new_recoding(target = education_expenditure_score) %>%
  recode_to(to = 1, where = education_expenditure_rate <= .25) %>%
  recode_to(to = 3, where = education_expenditure_rate > .25 & education_expenditure_rate <= .50) %>%
  recode_to(to = 4, where = education_expenditure_rate > .50 & education_expenditure_rate <= .75) %>%
  recode_to(to = 6, where = education_expenditure_rate > .75) %>%
  # recode_to(to = 6, where = education_expenditure_rate > .75 & education_expenditure_rate <= 1) %>%
  # recode_to(to = 7, where = education_expenditure_rate > 1) %>%
  #3.2 education expenditure change
  new_recoding(target = education_expenditure_change_score, source = education_price_change) %>%
  recode_to(to = 1, where.selected.exactly = "decreased") %>%
  recode_to(to = 2, where.selected.exactly = "no_change") %>%
  recode_to(to = 4, where.selected.exactly = "increased") %>%
  #4.1 availability to school
  new_recoding(time_to_school_score) %>%
  recode_to(to = 1, where.selected.exactly = "less15", source = time_school) %>%
  recode_to(to = 2, where.selected.exactly = "16_30", source = time_school) %>%
  recode_to(to = 3, where.selected.exactly = "31_60", source = time_school) %>%
  recode_to(to = 4, where.selected.exactly = "60_180", source = time_school) %>%
  recode_to(to = 5, where = time_school == "31_60" & transport_school %in% c("walking", "bicycle")) %>%
  recode_to(to = 6, where.selected.exactly = "above180", source = time_school) %>%
  recode_to(to = 7, where = time_school == "60_180" & transport_school %in% c("walking", "bicycle")) %>%
  recode_to(to = 8, where = time_school == "above180" & transport_school %in% c("walking", "bicycle")) %>%
  #5.1 access to school
  new_recoding(target = access_to_school_score, source = access_school) %>%
  recode_to(to = 1, where.selected.exactly = "yes") %>%
  recode_to(to = 6, where.selected.exactly = "no") %>%
  end_recoding()
  
  