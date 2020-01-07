# setup

remotes::install_github("mabafaba/hypegrammaR", ref = "custom_confidence_levels")

library(dplyr)
library(koboquest) # manage kobo questionnairs
library(kobostandards) # check inputs for inconsistencies
library(xlsformfill) # generate fake data for kobo
library(hypegrammaR) # stats 4 complex samples
library(composr) # horziontal operations
library(msni19) # tata!
library(surveyweights)

source("functions/to_alphanumeric_lowercase.R") # function to standardise column headers (like check.names)
source("functions/analysisplan_factory.R")  # generate analysis plans

library(lubridate)

# load questionnaire inputs
questions <- read.csv("input/questionnaire/SOM_JMCNA_HH_Tool_FIN_2019_settlements_questions.csv", 
                      stringsAsFactors=F, check.names=F)

choices <- read.csv("input/questionnaire/SOM_JMCNA_HH_Tool_FIN_2019_settlements_choices.csv", 
                    stringsAsFactors=F, check.names=F)

#load sampling frame
source("source/sampling.R")
# load the sampling frame into an object called samplingframe
# load the cluster sampling frame into an object called clustersamplingframe
# read data
#################################   it takes 25 minutes to compute all variables, dont run this all the time
# response <- readRDS("input/data/00-raw_data.RDS")
# 
# names(response)<-to_alphanumeric_lowercase(names(response))
# 
# response <- response %>%
#   select(-ends_with("note")) %>%
#   select(-starts_with("sv_")) %>%
#   select(-starts_with("_"), "_uuid") %>%
#   select(- c(start, end, deviceid, consensus))
# 
# response <- response %>%
#   left_join(select(clustersamplingframe, "P_CODE", "strata"), by = c("settlement" = "P_CODE"))
# response %>% filter(is.na(strata)) %>% nrow()
# ##to be removed when complete dataset and sampling frame
# response <- response %>%
#   filter(!is.na(strata))
# response %>% filter(is.na(strata)) %>% nrow()
# 
# samplingframe <- samplingframe %>% dplyr::filter(strata %in% response$strata)
# # ######
# # 
# # ##source("unicefledd thinkgs)
# # 
# # # add cluster ids
# # 
# # horizontal operations / recoding
# #
# source("source/composite variables/01-horizontal_general.R")
# source("source/composite variables/02-preexisting.R")
# source("source/composite variables/03-education.R")
# source("source/composite variables/04-nutrition.R")
# source("source/composite variables/05-health.R")
# source("source/composite variables/06-shelter_nfi.R")
# source("source/composite variables/07-fsl.R")
# source("source/composite variables/08-wash.R")
# source("source/composite variables/09-protection.R")
# source("source/composite variables/10-mcsi.R")
# source("source/composite variables/11-item_repo.R")
# source("source/composite variables/12-final.R")
# response %>% saveRDS("input/data/02-data_final_scoring25082019.RDS")
# response %>% write.csv("output/dataset_with_var.csv", row.names = F)
################################# END--  it takes 25 minutes to compute all variables, dont run this all the time
response <- readRDS("input/data/02-data_final_scoring09102019.RDS")

#
response_sl <- response %>% 
  filter(region %in% c("awdal", "sanaag", "sool", "togdheer", "woqooyi_galbeed"))

response_sl %>% select(region) %>% table()

response_sl_hc_idp <- response_sl %>%
  dplyr::filter(strata %in% samplingframe$strata) %>%
  dplyr::filter(yes_no_host == "yes" | yes_no_idp == "yes")

questionnaire <- load_questionnaire(response_sl_hc_idp,questions,choices)

#load analysisplan
analysisplan <- readr::read_delim("input/dap.csv", delim = ";")

strata_weight_fun <- map_to_weighting(sampling.frame = samplingframe,
                                      sampling.frame.population.column = "Population",
                                      sampling.frame.stratum.column = "strata",
                                      data.stratum.column = "strata")

response_sl_hc_idp$general_weights <- strata_weight_fun(response_sl_hc_idp)


results_sl_hc_idp <- from_analysisplan_map_to_output(response_sl_hc_idp, 
                                                     analysisplan = analysisplan,
                                                     weighting = strata_weight_fun,
                                                     cluster_variable_name = "settlement",
                                                     questionnaire,
                                                     confidence_level = 0.9)

# results_refugee_returnee <- from_analysisplan_map_to_output(response_refugee_returnee,
#                                                             analysisplan = analysisplan_refugee_returnee,
#                                                             questionnaire = questionnaire)


hypegrammaR:::map_to_generic_hierarchical_html(results_sl_hc_idp,
                                               render_result_with = hypegrammaR:::from_result_map_to_md_table,
                                               by_analysisplan_columns = c("dependent.var","repeat.var.value"),
                                               by_prefix =  c("",""),
                                               level = 2,
                                               questionnaire = questionnaire,
                                               label_varnames = TRUE,
                                               dir = "./output",
                                               filename = "hc_idp_state_OLD.html")

browseURL("hc_idp_test.html")

big_table <- results_sl_hc_idp$results %>% lapply(function(x) x[["summary.statistic"]]) %>% do.call(rbind, .)
write.csv(big_table, "output/big_table_state_OLD.csv", row.names = F)
# response %>% write.csv("output/dataset_with_var.csv", row.names = F)
# 
# some_results_refugee_returnee <- results_refugee_returnee[1:200]
# 
# 
# # not sure if this function should be "user facing" or have some wrappers (@Bouke thoughts?)
# # essentially it handles all the looping over different column values as hierarchies.
# # then each result is visualised by a function passed here that decides how to render each individual result
# # see ?hypegrammaR:::map_to_generic_hierarchical_html
# 
# hypegrammaR:::map_to_generic_hierarchical_html(some_results_refugee_returnee,
#                                                render_result_with = hypegrammaR:::from_result_map_to_md_table,
#                                                by_analysisplan_columns = c("dependent.var","repeat.var.value"),
#                                                by_prefix =  c("",""),
#                                                level = 2,
#                                                questionnaire = questionnaire,
#                                                label_varnames = TRUE,
#                                                dir = "./output",
#                                                filename = "refugee_returnee_test.html")
# 
# browseURL("refugee_returnee_test.html")

## REGIONAL ANALYSIS

analysisplan <- mutate(analysisplan, repeat.for.variable = "region")

results_sl_hc_idp_region <- from_analysisplan_map_to_output(response_sl_hc_idp, 
                                                     analysisplan = analysisplan,
                                                     weighting = strata_weight_fun,
                                                     cluster_variable_name = "settlement",
                                                     questionnaire,
                                                     confidence_level = 0.9)

big_table_region <- results_sl_hc_idp_region$results %>% lapply(function(x) x[["summary.statistic"]]) %>% do.call(rbind, .)
write.csv(big_table_region, "output/big_table_region_OLD.csv", row.names = F)


### DOING THE NEW ANALYSIS

df <- readxl::read_excel("input/som_mcna_data.xlsx")

df <- df %>%
  filter(!is.na(strata_new))

new_weight_fun <- map_to_weighting(sampling.frame = samplingframe,
                                   sampling.frame.population.column = "Population",
                                   sampling.frame.stratum.column = "strata",
                                   data.stratum.column = "strata_new")
## REGION LEVEL

results_region_new <- from_analysisplan_map_to_output(df, 
                                                      analysisplan = analysisplan,
                                                      weighting = new_weight_fun,
                                                      cluster_variable_name = "settlement",
                                                      questionnaire,
                                                      confidence_level = 0.9)

big_table_region_new <- results_region_new$results %>% lapply(function(x) x[["summary.statistic"]]) %>% do.call(rbind, .)
write.csv(big_table_region_new, "output/big_table_region_NEW.csv", row.names = F)

## STATE LEVEL

analysisplan <- mutate(analysisplan, repeat.for.variable = "statex7")

results_state_new <- from_analysisplan_map_to_output(df, 
                                                     analysisplan = analysisplan,
                                                     weighting = new_weight_fun,
                                                     cluster_variable_name = "settlement",
                                                     questionnaire,
                                                     confidence_level = 0.9)

big_table_state_new <- results_state_new$results %>% lapply(function(x) x[["summary.statistic"]]) %>% do.call(rbind, .)
write.csv(big_table_state_new, "output/big_table_state_NEW.csv", row.names = F)

## NATIONAL LEVEL

analysisplan <- mutate(analysisplan, repeat.for.variable = NA)

results_new <- from_analysisplan_map_to_output(df, 
                                               analysisplan = analysisplan,
                                               weighting = new_weight_fun,
                                               cluster_variable_name = "settlement",
                                               questionnaire,
                                               confidence_level = 0.9)

big_table_new <- results_new$results %>% lapply(function(x) x[["summary.statistic"]]) %>% do.call(rbind, .)
write.csv(big_table_new, "output/big_table_NEW.csv", row.names = F)

## NATIONAL LEVEL (NO DISAGGREGATION)

analysisplan <- mutate(analysisplan, independent.variable = NA, independent.variable.type = NA, hypothesis.type = "direct_reporting")

results_no_disagg_new <- from_analysisplan_map_to_output(df, 
                                               analysisplan = analysisplan,
                                               weighting = new_weight_fun,
                                               cluster_variable_name = "settlement",
                                               questionnaire,
                                               confidence_level = 0.9)

big_table_no_disagg_new <- results_no_disagg_new$results %>% lapply(function(x) x[["summary.statistic"]]) %>% do.call(rbind, .)
write.csv(big_table_no_disagg_new, "output/big_table_no_disagg_NEW.csv", row.names = F)
