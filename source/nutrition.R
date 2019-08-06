# horizontal aggregation
## nutrition

r <- 
  response %>%
  #1.1 MUAC coverage
  #1.2 visit from nutrition team
  #2.1 use of supplements
  new_recoding(target = supplements_score) %>%
  recode_to(to = 1, where = plw_using_suplements == "yes" & children_using_suplements == "yes") %>%
  recode_to(to = 1, where = plw_using_suplements == "yes" & is.na(children_using_suplements)) %>%
  recode_to(to = 1, where = plw_using_suplements == "" & children_using_suplements == "yes") %>%
  recode_to(to = 3, where = plw_using_suplements == "yes" & children_using_suplements == "no") %>%
  recode_to(to = 3, where = plw_using_suplements == "no" & children_using_suplements == "yes") %>%
  recode_to(to = 5, where = plw_using_suplements == "no" & children_using_suplements == "no") %>%
  #2.2 children enrollment
  new_recoding(target = children_enrolled_nutrition_centers_score) %>%
  recode_to(to = 1, where.selected.exactly = "yes", source = children_enrolled_nutrition_centers) %>%
  recode_to(to = 4, where.selected.exactly = "no", source = children_enrolled_nutrition_centers) %>%
  #3.1 nutrition center access
  new_recoding(time_to_nutrition_center_score) %>%
  recode_to(to = 1, where.selected.exactly = "less15", source = nearest_nutrition_center) %>%
  recode_to(to = 2, where.selected.exactly = "16_30", source = nearest_nutrition_center) %>%
  recode_to(to = 3, where.selected.exactly = "31_60", source = nearest_nutrition_center) %>%
  recode_to(to = 4, where.selected.exactly = "60_180", source = nearest_nutrition_center) %>%
  recode_to(to = 4, where = nearest_nutrition_center == "31_60" & transport_type_used %in% c("walking", "bicycle", "cart")) %>%
  recode_to(to = 5, where.selected.exactly = "above180", source = nearest_nutrition_center) %>%
  recode_to(to = 5, where = nearest_nutrition_center == "60_180" & transport_type_used %in% c("walking", "bicycle", "cart")) %>%
  recode_to(to = 6, where = nearest_nutrition_center == "above180" & transport_type_used %in% c("walking", "bicycle", "cart")) %>%
  #4.1 access to nutrition center
  new_recoding(access_to_nutrition_center_score, source = access_nearest_nutrition_center) %>%
  recode_to(to = 1, where.selected.exactly = "yes") %>%
  recode_to(to = 4, where.selected.exactly = "no") %>%
  end_recoding()
