
analysisplan <- read.csv("./input/analysisplan_map_FS.csv", stringsAsFactors = F)

strata_weight_fun <- map_to_weighting(sampling.frame = samplingframe,
                                      sampling.frame.population.column = "Population",
                                      sampling.frame.stratum.column = "strata",
                                      data.stratum.column = "strata")

response_hc_idp$general_weights <- strata_weight_fun(response_hc_idp)


results_hc_idp <- from_analysisplan_map_to_output(response_hc_idp, 
                                                  analysisplan = analysisplan,
                                                  weighting = strata_weight_fun,
                                                  cluster_variable_name = "settlement",
                                                  questionnaire,
                                                  confidence_level = 0.9)
big_table <- results_hc_idp$results %>% lapply(function(x) x[["summary.statistic"]]) %>% do.call(rbind, .)
write.csv(big_table, "output/maps/big_table.csv", row.names = F)


idp_score_maps <- big_table %>% 
  filter(independent.var.value == "IDP", 
         dependent.var.value == "in_need") %>% 
  select(dependent.var, dependent.var.value, independent.var.value, numbers, repeat.var.value) %>% 
  spread(dependent.var, numbers) %>%
  arrange(repeat.var.value) %>%
  ##we stratified on HC and IDP settlements but we define IDP and host in a different variable. therefore, 
  ## sometimes we have districts with 100% IDP in needs but it just means we came accross 1 IDP living in HC settlement. 
  ## So we are removing the information on districts without IDP
  filter(repeat.var.value %in% list_districts$admin2Pcod[list_districts$assessed_jmcna_idp == "yes"]) %>% 
  left_join(select(list_districts, admin2Pcod, admin2Name, representiveness_idp), by = c("repeat.var.value" = "admin2Pcod")) %>%
  filter(representiveness_idp %in% c("acceptable", "informative"))


idp_score_maps <- idp_score_maps[!duplicated(idp_score_maps$repeat.var.value),] ##remove duplicates row, not sure why different strata give severals duplicates row when combine
idp_score_maps[is.na(idp_score_maps)] <- 0
idp_score_maps %>% write.csv("output/maps/idp_scores_maps.csv", row.names = F)

host_score_maps <- big_table %>% 
  filter(independent.var.value == "not_displaced", 
         dependent.var.value == "in_need") %>% 
  select(dependent.var, dependent.var.value, independent.var.value, numbers, repeat.var.value) %>% 
  spread(dependent.var, numbers) %>%
  arrange(repeat.var.value) %>%
  left_join(select(list_districts, admin2Pcod, admin2Name, representiveness_hc), by = c("repeat.var.value" = "admin2Pcod")) %>%
  filter(representiveness_hc %in% c("acceptable", "informative"))
host_score_maps <- host_score_maps[!duplicated(host_score_maps$repeat.var.value),] ##remove duplicates row, not sure why different strata give severals duplicates row when combine
host_score_maps[is.na(host_score_maps)] <- 0
host_score_maps %>% write.csv("output/maps/host_score_maps.csv", row.names = F)

