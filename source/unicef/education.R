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
  # not counting NAs
  recode_directly(median(education_level_score, enrollement_rate_score, attendance_rate_score, drop_out_score, na.rm = T)) %>%
  new_recoding(target = education_coping_median) %>%
  recode_directly(median(education_coping_score, na.rm = T)) %>%
  new_recoding(target = education_phywell_median) %>%
  recode_directly(1) %>%
  end_recoding()

education_lookup_table <- 
  data.frame("1" = c(1,1,2,2,3),
             "2" = c(2,2,2,3,3),
             "3" = c(2,3,3,3,4),
             "4" = c(3,3,4,4,4),
             "5" = c(3,4,4,5,5))

un_r <- 
  un_response %>%
  new_recoding(target = education_humanitarian_condition) %>%
  recode_directly(ifelse(education_phywell_median > education_lvgs_median | education_phywell_median > education_coping_median, 
                         as.character(education_phywell_median),
                         as.character(education_lookup_table[education_lvgs_median, education_coping_median]))) %>%
  end_recoding()
  
analysisplan<-make_analysisplan_all_vars(un_r,
                                         questionnaire,
                                         independent.variable = "yes_no_host",
                                         repeat.for.variable = "region",
                                         hypothesis.type = "group_difference"
) 

## while not having extra well being pillar

