#####################################
### DATA AND ANALYSIS PREPARATION ###
#####################################

# NECESSARY LIBRARIES AND FUNCTIONS

library(dplyr)
library(koboquest) # manage kobo questionnairs
library(kobostandards) # check inputs for inconsistencies
library(xlsformfill) # generate fake data for kobo
library(hypegrammaR) # stats 4 complex samples
library(composr) # horziontal operations
library(msni19) # tata!
library(surveyweights)
library(lubridate)

source("functions/to_alphanumeric_lowercase.R") # function to standardise column headers (like check.names)
source("functions/analysisplan_factory.R")  # generate analysis plans

# LOAD QUESTIONNAIRE AND CHOICE OPTIONS
questions <- read.csv("input/questionnaire/SOM_JMCNA_HH_Tool_FIN_2019_settlements_questions.csv", 
                      stringsAsFactors=F, check.names=F) %>%
  select(type, name, label = `label::english`, relevant) %>%
  distinct()

choices <- read.csv("input/questionnaire/SOM_JMCNA_HH_Tool_FIN_2019_settlements_choices.csv", 
                    stringsAsFactors=F, check.names=F) %>%
  select(list_name, name, label = `label::english`) %>%
  distinct()

# LOAD AND MANIPULATE DATA
df <- readRDS("input/data/final_dataset_analysis.RDS")

questionnaire <- load_questionnaire(df, questions, choices)

# LOAD ANALYSIS PLAN
analysisplan <- readr::read_delim("input/dap.csv", delim = ";") %>%
  mutate(independent.variable = "idp_settlement")

# WEIGHTING

source("source/sampling.R")

strata_weight_fun <- map_to_weighting(sampling.frame = samplingframe,
                                     sampling.frame.population.column = "Population",
                                     sampling.frame.stratum.column = "strata",
                                     data.stratum.column = "strata")

normal_weight_fun <- function(df) {
  df$general_weights
}

#################
### FUNCTIONS ###
#################

# Getting table with p-value

table_wrangling <- function(x) {
  stats <- x[["summary.statistic"]]
  pval_table <- as.data.frame(x[["hypothesis.test"]])
  num_col = ncol(pval_table)
  if (num_col != 5) {
    if (num_col == 0) {
      pval_table <- data.frame(F = NA, p.value = NA, ndf = NA, ddf = NA, name = NA)
    } else {
      pval_table <- full_join(data.frame(F = numeric(), p.value = numeric(), ndf = numeric(), ddf = numeric(), name = character()), pval_table)
    }
  } else {
    names(pval_table) <- c("F", "p.value", "ndf", "ddf", "name")
  }
  cbind(stats, pval_table)
}

pval_table <- function(results, filename) {
  lapply(results, table_wrangling) %>%
    do.call(rbind, .) %>%
    write.csv(filename, row.names = F)
}

################
### ANALYSIS ###
################

# NATIONAL LEVEL ANALYSIS -----------------------------------------------------------------------------------------------

national_disaggregated_dap <- mutate(analysisplan, repeat.for.variable = NA)
national_overall_dap <- mutate(national_disaggregated_dap, independent.variable = NA, independent.variable.type = NA,
                               hypothesis.type = "direct_reporting")

national_disaggregated_results <- from_analysisplan_map_to_output(df, 
                                                                  analysisplan = filter(national_disaggregated_dap, dependent.variable != "separation_age_gender"),
                                                                  weighting = strata_weight_fun,
                                                                  cluster_variable_name = "settlement",
                                                                  questionnaire,
                                                                  confidence_level = 0.9)

nat_disagg_test <- from_analysisplan_map_to_output(df, 
                                                   analysisplan = national_disaggregated_dap,
                                                   weighting = normal_weight_fun,
                                                   cluster_variable_name = "settlement",
                                                   questionnaire,
                                                   confidence_level = 0.9)

national_overall_results <- from_analysisplan_map_to_output(df,
                                                            analysisplan = national_overall_dap,
                                                            weighting = strata_weight_fun,
                                                            cluster_variable_name = "settlement",
                                                            questionnaire,
                                                            confidence_level = 0.9)

# CSV tables

pval_table(national_disaggregated_results$results, "output/report_tables/national_disaggregated_table.csv")
pval_table(national_overall_results$results, "output/report_tables/national_overall_table.csv")

# HTML tables

hypegrammaR:::map_to_generic_hierarchical_html(national_disaggregated_results,
                                               render_result_with = hypegrammaR:::from_result_map_to_md_table,
                                               by_analysisplan_columns = c("dependent.var"),
                                               by_prefix =  c(""),
                                               level = 2,
                                               dir = "./output/report_tables",
                                               filename = "national_disaggregated_table.html")

map_to_generic_hierarchical_html(national_overall_results,
                                 render_result_with = hypegrammaR:::from_result_map_to_md_table,
                                 by_analysisplan_columns = c("dependent.var"),
                                 by_prefix =  c(""),
                                 level = 2,
                                 dir = "./output/report_tables",
                                 filename = "national_overall_table.html")

## STATE LEVEL ANALYSIS --------------------------------------------------------------------------------------------------

state_disaggregated_dap <- mutate(national_disaggregated_dap, repeat.for.variable = "statex7")
state_overall_dap <- mutate(national_overall_dap, repeat.for.variable = "statex7")

state_disaggregated_results <- from_analysisplan_map_to_output(df, 
                                                               analysisplan = state_disaggregated_dap,
                                                               weighting = strata_weight_fun,
                                                               cluster_variable_name = "settlement",
                                                               questionnaire,
                                                               confidence_level = 0.9)

state_overall_results <- from_analysisplan_map_to_output(df,
                                                         analysisplan = state_overall_dap,
                                                         weighting = strata_weight_fun,
                                                         cluster_variable_name = "settlement",
                                                         questionnaire,
                                                         confidence_level = 0.9)

# CSV tables

pval_table(state_disaggregated_results$results, "output/report_tables/state_disaggregated_table.csv")
pval_table(state_overall_results$results, "output/report_tables/state_overall_table.csv")

# HTML tables

map_to_generic_hierarchical_html(state_disaggregated_results,
                                 render_result_with = hypegrammaR:::from_result_map_to_md_table,
                                 by_analysisplan_columns = c("dependent.var", "repeat.var.value"),
                                 by_prefix =  c("", ""),
                                 level = 2,
                                 dir = "./output/report_tables",
                                 filename = "state_disaggregated_table.html")

map_to_generic_hierarchical_html(state_overall_results,
                                 render_result_with = hypegrammaR:::from_result_map_to_md_table,
                                 by_analysisplan_columns = c("dependent.var", "repeat.var.value"),
                                 by_prefix =  c("", ""),
                                 level = 2,
                                 dir = "./output/report_tables",
                                 filename = "state_overall_table.html")

## REGIONAL LEVEL ANALYSIS -----------------------------------------------------------------------------------------------

region_disaggregated_dap <- mutate(national_disaggregated_dap, repeat.for.variable = "region")
region_overall_dap <- mutate(national_overall_dap, repeat.for.variable = "region")

region_disaggregated_results <- from_analysisplan_map_to_output(df, 
                                                                analysisplan = region_disaggregated_dap,
                                                                weighting = strata_weight_fun,
                                                                cluster_variable_name = "settlement",
                                                                questionnaire,
                                                                confidence_level = 0.9)

region_overall_results <- from_analysisplan_map_to_output(df, 
                                                          analysisplan = region_overall_dap,
                                                          weighting = strata_weight_fun,
                                                          cluster_variable_name = "settlement",
                                                          questionnaire,
                                                          confidence_level = 0.9)

# CSV tables

pval_table(region_disaggregated_results$results, "output/report_tables/region_disaggregated_table.csv")
pval_table(region_overall_results$results, "output/report_tables/region_overall_table.csv")

# HTML tables

map_to_generic_hierarchical_html(region_disaggregated_results,
                                 render_result_with = hypegrammaR:::from_result_map_to_md_table,
                                 by_analysisplan_columns = c("dependent.var", "repeat.var.value"),
                                 by_prefix =  c("", ""),
                                 level = 2,
                                 dir = "./output/report_tables",
                                 filename = "region_disaggregated_table.html")

map_to_generic_hierarchical_html(region_overall_results,
                                 render_result_with = hypegrammaR:::from_result_map_to_md_table,
                                 by_analysisplan_columns = c("dependent.var", "repeat.var.value"),
                                 by_prefix =  c("", ""),
                                 level = 2,
                                 dir = "./output/report_tables",
                                 filename = "region_overall_table.html")

## DISTRICT LEVEL ANALYSIS -----------------------------------------------------------------------------------------------

district_disaggregated_dap <- mutate(national_disaggregated_dap, repeat.for.variable = "district_ocha")
district_overall_dap <- mutate(national_overall_dap, repeat.for.variable = "district_ocha")

district_disaggregated_results <- from_analysisplan_map_to_output(df, 
                                                                analysisplan = district_disaggregated_dap,
                                                                weighting = strata_weight_fun,
                                                                cluster_variable_name = "settlement",
                                                                questionnaire,
                                                                confidence_level = 0.9)

district_overall_results <- from_analysisplan_map_to_output(df, 
                                                          analysisplan = district_overall_dap,
                                                          weighting = strata_weight_fun,
                                                          cluster_variable_name = "settlement",
                                                          questionnaire,
                                                          confidence_level = 0.9)

# CSV tables

pval_table(district_disaggregated_results$results, "output/report_tables/district_disaggregated_table.csv")
pval_table(district_overall_results$results, "output/report_tables/district_overall_table.csv")

# HTML tables

map_to_generic_hierarchical_html(district_disaggregated_results,
                                 render_result_with = hypegrammaR:::from_result_map_to_md_table,
                                 by_analysisplan_columns = c("dependent.var", "repeat.var.value"),
                                 by_prefix =  c("", ""),
                                 level = 2,
                                 dir = "./output/report_tables",
                                 filename = "district_disaggregated_table.html")

map_to_generic_hierarchical_html(district_overall_results,
                                 render_result_with = hypegrammaR:::from_result_map_to_md_table,
                                 by_analysisplan_columns = c("dependent.var", "repeat.var.value"),
                                 by_prefix =  c("", ""),
                                 level = 2,
                                 dir = "./output/report_tables",
                                 filename = "district_overall_table.html")
