
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
  new_recoding(target = access_to_market_score) %>%
  recode_to(to = 1, where.selected.exactly = "less15", source = time_market) %>%
  recode_to(to = 2, where.selected.exactly = "16_30", source = time_market) %>%
  recode_to(to = 3, where.selected.exactly = "31_60", source = time_market) %>%
  recode_to(to = 4, where.selected.exactly = "60_180", source = time_market) %>%
  recode_to(to = 4, where = time_market == "31_60" & transport_market %in% c("walking", "bicycle", "cart")) %>%
  recode_to(to = 5, where.selected.exactly = "above180", source = time_market) %>%
  recode_to(to = 5, where = time_market == "60_180" & transport_market %in% c("walking", "bicycle", "cart")) %>%
  recode_to(to = 6, where = time_market == "above180" & transport_market %in% c("walking", "bicycle", "cart")) %>%
  # 8.1 food insecurity