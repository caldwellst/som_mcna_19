# setup

library(dplyr)
library(koboquest) # manage kobo questionnairs
library(kobostandards) # check inputs for inconsistencies
library(xlsformfill) # generate fake data for kobo
library(hypegrammaR) # stats 4 complex samples
library(composr) # horziontal operations

source("functions/to_alphanumeric_lowercase.R") # function to standardise column headers (like check.names)
source("functions/analysisplan_factory.R")  # generate analysis plans

library(lubridate)

# load questionnaire inputs
questions <- read.csv("input/questionnaire/SOM_JMCNA_HH_Tool_FIN_2019_settlements_questions.csv", 
                      stringsAsFactors=F, check.names=F)

choices <- read.csv("input/questionnaire/SOM_JMCNA_HH_Tool_FIN_2019_settlements_choices.csv", 
                    stringsAsFactors=F, check.names=F)


# read data
response <- read.csv("input/data/REACH_JMCNA_DATA_CLEANING_AMRAN.csv",
                     stringsAsFactors = F, check.names = F)
names(response)<-to_alphanumeric_lowercase(names(response))

questionnaire <- load_questionnaire(response,questions,choices)

#load sampling frame
source("source/sampling.R")
# load the sampling frame into an object called samplingframe
# load the cluster sampling frame into an object called clustersamplingframe

response <- response %>% 
  left_join(select(clustersamplingframe, "P_CODE", "strata"), by = c("settlement" = "P_CODE"))

# add cluster ids

# cluster_lookup_table <- read.csv("input/combined_sample_ids.csv", 
#                          stringsAsFactors=F, check.names=F)
# 
# response_filtered_w_clusterids <- response_filtered %>% 
#   mutate(strata = paste0(lookup_table$district[match(cluster_location_id,cluster_lookup_table$new_ID)],type_hh))
# 
# 
# horizontal operations / recoding
# 

source("source/Pre-existing vulnerability indicators.R")
# r <- response_filtered_w_clusterids %>%
#   new_recoding(source=how_much_debt, target=hh_with_debt_value) %>% 
#   recode_to(0.25,where.num.larger.equal = 505000,otherwise.to=0) %>% 

# 
#   new_recoding(target=hh_unemployed) %>%
#   recode_to(0 ,where=!(is.na(response_filtered$work) | is.na(response_filtered$actively_seek_work))) %>%
#   recode_to(0.5,where=(work == "no") & (actively_seek_work == "yes")) %>%
# 
#   new_recoding(source=reasons_for_debt, target=hh_unable_basic_needs) %>%
#   recode_to(0.25, where.selected.any = c("health","food","education","basic_hh_expenditure"), otherwise.to=0) %>%
# 
#   end_recoding
#   
# r <- r %>% mutate(score_livelihoods = hh_with_debt_value+hh_unemployed+hh_unable_basic_needs)

# vertical operations / aggregation

##to be removed when complete dataset and sampling frame
samplingframe <- samplingframe %>% dplyr::filter(strata %in% response$strata)
response <- response %>% dplyr::select(-c(`_id`, `__version__`, `_uuid`, `_submission_time`, `_index`)) %>%
  dplyr::filter(strata %in% samplingframe$strata)

# make analysisplan including all questions as dependent variable by HH type, repeated for each governorate:
analysisplan<-make_analysisplan_all_vars(response,
                                         questionnaire,
                                         independent.variable = "yes_no_host",
                                         repeat.for.variable = "region",
                                         hypothesis.type = "group_difference"
                                         ) %>%
  filter(dependent.variable == "time_market")

analysisplan2 <- data.frame(
  research.question = "",
  sub.research.question = "",
  # hypothesis = "hp 1",
  repeat.for.variable = "region",
  independent.variable = "yes_no_host",
  independent.variable.type = c("categorical"),
  # dependent.variable = c("HHEX", "access_to_market_score", "time_market"),
  # hypothesis.type = c("direct_reporting", "group_difference", "group_difference"),
  # independent.variable.type = c("categorical"),
  # dependent.variable.type = c("numerical", "categorical", "categorical"))
  dependent.variable = c("time_market"),
  dependent.variable.type = c("categorical"),
  hypothesis.type = c("group_difference"))

analysisplan3 <- data.frame(
  research.question = "RQ: not specified (automatic analysisplan)",
  sub.research.question = "sub RQ not specified (automatic analysisplan)",
  # hypothesis = "hp 1",
  repeat.for.variable = "region",
  independent.variable = "yes_no_host",
  independent.variable.type = c("categorical"),
  # dependent.variable = c("HHEX", "access_to_market_score", "time_market"),
  # hypothesis.type = c("direct_reporting", "group_difference", "group_difference"),
  # independent.variable.type = c("categorical"),
  # dependent.variable.type = c("numerical", "categorical", "categorical"))
  dependent.variable = c("time_market"),
  dependent.variable.type = c("categorical"),
  hypothesis.type = c("group_difference"))

response$general_weights <- strata_weight_fun(response)

# response$cluster_id <- paste(response$settlement,response$yes_no_idp,sep = "_")

results <- from_analysisplan_map_to_output(response, 
                                           analysisplan = analysisplan,
                                           weighting = strata_weight_fun,
                                           cluster_variable_name = "settlement",
                                           questionnaire)

results2 <- from_analysisplan_map_to_output(response, 
                                            analysisplan = analysisplan2,
                                            weighting = strata_weight_fun,
                                            cluster_variable_name = "settlement",
                                            questionnaire)

results3 <- from_analysisplan_map_to_output(response, 
                                            analysisplan = analysisplan3,
                                            weighting = strata_weight_fun,
                                            cluster_variable_name = "settlement",
                                            questionnaire)


analysisplan
analysisplan2
analysisplan3

results$analysisplan
results2$analysisplan
results3$analysisplan