
## 02 preexisting
## 4.2 Lost of income source
new_recoding(target = loss_income_source_score, source = lost_income_source) %>%
  recode_to(to = 2, where.selected.exactly = "no") %>%
  recode_to(to = 5, where.selected.exactly = "yes") %>%
  ## 4.3 ownership assest 
  ## 4.4 lost of assests
  new_recoding(target = lost_assest_score) %>%
  recode_to(to = 2, where = (lost_livestock == "no" & lost_land_cultivation == "no") |
              (lost_livestock == "no" & land_cultivation == "no") |
              (own_livestock == "no" & lost_land_cultivation == "no")) %>%
  recode_to(to = 5, where = lost_livestock == "yes" | lost_land_cultivation == "yes") %>%
  # 7.1 access to market

  # 8.1 food insecurity