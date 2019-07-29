response %>% 
  left_join(select(clustersamplingframe, "P_CODE", "strata"), by = c("settlement" = "P_CODE")) %>% 
  dplyr::filter(is.na(strata)) %>% View()