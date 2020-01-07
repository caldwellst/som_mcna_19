#hno

#presentation

analysisplan <- read.csv("input/dap_hno.csv", stringsAsFactors = F)


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
write.csv(big_table, "output/big_table.csv", row.names = F)
source("source/make_large_table.R")
