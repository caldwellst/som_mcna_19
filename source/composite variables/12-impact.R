# horizontal aggregation
## impact score

# adding the secondary dataset for drought_score and number of incidents
secondary_data <- read_excel("input/impact_score_secondary_source.xlsx")

response <- response %>%
  left_join(secondary_data, by = "_uuid") %>%
  #1.1 severity_drought_score 
  #already calculated drought_score
  #2.1 conflict_environment_score 
  new_recoding(target = conflict_environment_score, source = No_Incidents) %>%
  recode_to(to = 1, where.num.equal = 0) %>%
  recode_to(to = 2, where.num.larger.equal = 1) %>%
  recode_to(to = 3, where.num.larger.equal = 3) %>%
  recode_to(to = 4, where.num.larger.equal = 7) %>%
  recode_to(to = 5, where.num.larger.equal = 11) %>%
  recode_to(to = 6, where.num.larger.equal = 21) %>%
  recode_to(to = 7, where.num.larger.equal = 31) %>%
  recode_to(to = 8, where.num.larger.equal = 51) %>%
  #3.1 reason_separation_score
  new_recoding(target = reason_separation_score) %>%
  recode_to(to = 1, where = separation_members == "no") %>%
  recode_to(to = 2, where = separation_members == "yes") %>%
  recode_to(to = 1, where.selected.any = c("left_study", "left_work"), source = separation_reasons) %>%
  recode_to(to = 5, where.selected.any = c("flood", "drought"), source = separation_reasons) %>%
  recode_to(to = 6, where.selected.any = c("conflict"), source = separation_reasons) %>%
  recode_to(to = 7, where.selected.exactly = c("flood", "conflict"), source = separation_reasons) %>%
  recode_to(to = 7, where.selected.exactly = c("drought", "conflict"), source = separation_reasons) %>%
  #4.1 reason_lost_employment_score
  new_recoding(target = reason_lost_employment_score) %>%
  recode_to(to = 1, where.num.equal = 0, source = lost_employment) %>%
  recode_to(to = 2, where.num.larger = 0, source = lost_employment) %>%
  recode_to(to = 5, where.selected.any = c("flood", "drought"), source = reason_lost_employment) %>%
  recode_to(to = 6, where.selected.any = c("conflict"), source = reason_lost_employment) %>%
  recode_to(to = 7, where.selected.exactly = c("flood", "conflict"), source = reason_lost_employment) %>%
  recode_to(to = 7, where.selected.exactly = c("drought", "conflict"), source = reason_lost_employment) %>%  
  #5.1 reason_shelter_damage_score
  new_recoding(target = reason_shelter_damage_score) %>%
  recode_to(to = 1, where = shelter_damaged_last_90_days == "no") %>%
  recode_to(to = 2, where = shelter_damaged_last_90_days == "yes") %>%
  recode_to(to = 5, where.selected.any = c("flood", "drought"), source = reason_for_shelter_damage) %>%
  recode_to(to = 6, where.selected.any = c("conflict"), source = reason_for_shelter_damage) %>%
  #6.1 reason_displacement_score 
  new_recoding(target = reason_displacement_score) %>%
  recode_to(to = 1, where = disp_why1 == "") %>%
  recode_to(to = 2, where = disp_why1 != "") %>%
  recode_to(to = 5, where = disp_why1 %in% c("flooding", "drought")) %>%
  recode_to(to = 5, where = disp_why2 %in% c("flooding", "drought")) %>%
  recode_to(to = 6, where = disp_why1 %in% c("conflict_community", "conflict_surrounding", "conflict_fear")) %>%
  recode_to(to = 6, where = disp_why2 %in% c("conflict_community", "conflict_surrounding", "conflict_fear")) %>%
  recode_to(to = 7, where = disp_why1 %in% c("conflict_community", "conflict_surrounding", "conflict_fear") & 
                            disp_why2 %in% c("flooding", "drought")) %>%
  recode_to(to = 7, where = disp_why2 %in% c("conflict_community", "conflict_surrounding", "conflict_fear") & 
                            disp_why1 %in% c("flooding", "drought")) %>%
  #7.1 barriers_hum_score
  new_recoding(target = barriers_hum_score, source = barrier_hum) %>%
  recode_to(to = 2, where.selected.any = "barrier_info") %>%
  recode_to(to = 4, where.selected.any = "barrier_excluded") %>%
  recode_to(to = 5, where.selected.any = "barrier_physical") %>%
  recode_to(to = 6, where.selected.any = c("barrier_sec_route", "barrier_sec_point")) %>%
  end_recoding() 
  #ipc_score
response$ipc_score <- NA
