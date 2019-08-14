response_as_read %>% 
  left_join(select(clustersamplingframe, "P_CODE", "strata"), by = c("settlement" = "P_CODE")) %>% 
  dplyr::filter(is.na(strata)) %>%
  group_by(region, district, idp_settlement, settlement, settlement_other) %>%
  summarise(n = n())
