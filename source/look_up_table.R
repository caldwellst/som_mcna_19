#look at the stratas withing buffer
match_sample <- response %>% 
  group_by(strata) %>% 
  summarise(survey_collected = n()) %>% 
  left_join(select(samplingframe, surveys, strata)) %>% 
  rename(target_surveys = surveys) %>%
  mutate(diff_perc = survey_collected/target_surveys,
         to_include = ifelse(diff_perc >= .85, "sampling_ok", "sampling_under_buffer" )) 

#decision to what to do
sample_acceptable <- c("other__idp", "protra__idp", "other__hc", "Beletweyne__hc") #higher aggregation
sample_infor <- c("Belet Xaawo__hc", "ceerigaabo__idp", "Afmadow__hc", "Garbahaarey__hc", "Ceel Barde__hc", "Buuhoodle__hc", "Dhuusamarreeb__hc")
sample_to_remove <- c("baardheere__idp", "luuq__idp", "belet_xaawo__idp")

match_sample$what <- NA
match_sample$what[match_sample$to_include == "sampling_ok"] <- "acceptable"
match_sample$what[match_sample$strata %in% sample_acceptable] <- "acceptable"
match_sample$what[match_sample$strata %in% sample_infor] <- "informative"
match_sample$what[match_sample$strata %in% sample_to_remove] <- "remove"

#creating a unique list of strata, district and population from cluster list to match everything together
unique_hc_cluster <- clustersamplingframe %>% filter(population_type == "hc") %>% select(District, strata)
unique_hc_cluster <- unique_hc_cluster[!duplicated(unique_hc_cluster$District),]
unique_hc_cluster$population_type <- "hc"
unique_idp_cluster <- clustersamplingframe %>% filter(population_type == "idp") %>% select(District, strata)
unique_idp_cluster <- unique_idp_cluster[!duplicated(unique_idp_cluster$District),]
unique_idp_cluster$population_type <- "idp"

unique_strata_district <- rbind(unique_hc_cluster, unique_idp_cluster)

match_sample <- match_sample %>% left_join(unique_strata_district)
#not sure where this typo comes from
match_sample$District[is.na(match_sample$District)] <- "afgooye"
match_sample$population_type[is.na(match_sample$population_type)] <- "idp"

match_sample %>% filter(is.na(population_type)) %>% nrow()



#super list with where IDP were sampled for.
list_districts <- read.csv("input/list_districts.csv", stringsAsFactors = F)

list_districts <- list_districts %>% 
  left_join(select(filter(match_sample, population_type == "hc"), District, what), by = c("district" = "District")) %>%
  rename(representiveness_hc = what) %>%
  left_join(select(filter(match_sample, population_type == "idp"), District, what), by = c("district" = "District")) %>%
  rename(representiveness_idp = what)


list_districts$representiveness_hc[(list_districts$statex7 == "banadir") & is.na(list_districts$representiveness_hc)] <- "acceptable"
list_districts$representiveness_idp[(list_districts$statex7 == "banadir") & is.na(list_districts$representiveness_idp)] <- "acceptable"


list_districts %>% write.csv("output/lookup_table.csv", row.names = F)
