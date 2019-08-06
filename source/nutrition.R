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
  end_recoding()
