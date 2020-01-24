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

# THIS CODE DOES NOT NEED TO BE RUN ANYMORE, BUT WAS USED TO MANIPULATE THE DATA.FRAME

# # load questionnaire inputs
# questions <- read.csv("input/questionnaire/SOM_JMCNA_HH_Tool_FIN_2019_settlements_questions.csv", 
#                       stringsAsFactors=F, check.names=F)
# 
# choices <- read.csv("input/questionnaire/SOM_JMCNA_HH_Tool_FIN_2019_settlements_choices.csv", 
#                     stringsAsFactors=F, check.names=F)
# 
# #load sampling frame
# source("source/sampling.R")
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
# # LOAD SAMPLING FRAME
# source("source/sampling.R")
# response <- response %>%
#   dplyr::filter(strata %in% samplingframe$strata) %>%
#   dplyr::filter(yes_no_host == "yes" | yes_no_idp == "yes") %>%
#   select(-`_uuid`)
# strata_weight_fun <- map_to_weighting(sampling.frame = samplingframe,
#                                      sampling.frame.population.column = "Population",
#                                      sampling.frame.stratum.column = "strata",
#                                      data.stratum.column = "strata")
#
# response$general_weights <- strata_weight_fun(response)
# response %>% saveRDS("input/data/02-data_final_scoring25082019.RDS")
# response %>% write.csv("output/dataset_with_var.csv", row.names = F)
################################# END--  it takes 25 minutes to compute all variables, dont run this all the time