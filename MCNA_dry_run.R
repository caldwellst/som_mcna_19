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


response_as_read <- response

response <- response %>%
  select(-ends_with("note")) %>%
  select(-starts_with("sv_")) %>%
  select(-starts_with("_"), "_uuid") %>%
  select(- c(start, end, deviceid, agency, consensus))


questionnaire <- load_questionnaire(response,questions,choices)

#load sampling frame
source("source/sampling.R")
# load the sampling frame into an object called samplingframe
# load the cluster sampling frame into an object called clustersamplingframe

response <- response %>% 
  left_join(select(clustersamplingframe, "P_CODE", "strata"), by = c("settlement" = "P_CODE"))

##to be removed when complete dataset and sampling frame
response <- response %>%
  filter(!is.na(strata))

samplingframe <- samplingframe %>% dplyr::filter(strata %in% response$strata)
response <- response %>% 
  dplyr::filter(strata %in% samplingframe$strata)

##to be removed when the data analysis problem is solved
response_no_added_variables <- response
######

##source("unicefledd thinkgs)

# add cluster ids

# horizontal operations / recoding
# 
source("source/composite variables/01-horizontal_general.R")
source("source/composite variables/02-preexisting.R")
source("source/composite variables/03-education.R")
source("source/composite variables/04-nutrition.R")
source("source/composite variables/05-health.R")
source("source/composite variables/06-shelter_nfi.R")
source("source/composite variables/07-fsl.R")
source("source/composite variables/08-wash.R")
source("source/composite variables/09-protection.R")
source("source/composite variables/10-mcsi.R")




# make analysisplan including all questions as dependent variable by HH type, repeated for each governorate:
analysisplan<-make_analysisplan_all_vars(response_no_added_variables,
                                         questionnaire,
                                         independent.variable = "yes_no_host",
                                         repeat.for.variable = "region",
                                         hypothesis.type = "group_difference"
                                         ) 

strata_weight_fun <- map_to_weighting(sampling.frame = samplingframe,
                                      sampling.frame.population.column = "Population",
                                      sampling.frame.stratum.column = "strata",
                                      data.stratum.column = "strata")

response$general_weights <- strata_weight_fun(response_no_added_variables)

# response$cluster_id <- paste(response$settlement,response$yes_no_idp,sep = "_")


results <- from_analysisplan_map_to_output(response_no_added_variables, analysisplan = analysisplan,
                                          weighting = strata_weight_fun,
                                          cluster_variable_name = "settlement",
                                          questionnaire)


hypegrammaR:::map_to_generic_hierarchical_html(results,
                                               render_result_with = hypegrammaR:::from_result_map_to_md_table,
                                               by_analysisplan_columns = c("dependent.var","repeat.var.value"),
                                               by_prefix =  c("",""),
                                               level = 2,
                                               questionnaire = questionnaire,
                                               label_varnames = TRUE,
                                               dir = "./output",
                                               filename = "summary_by_dependent_var_then_by_repeat_var.html"
                                               )
browseURL("summary_by_dependent_var_then_by_repeat_var.html")
