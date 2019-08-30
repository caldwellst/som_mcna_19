#make the graphs

# remotes::install_github("caldwellst/msni19")
# remotes::install_github("ellieallien/surveyweights")
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


response <- readRDS("input/data/02-data_final_scoring29082019.RDS")
source("source/sampling.R")
response_hc_idp <- response %>%
  dplyr::filter(strata %in% samplingframe$strata) %>%
  dplyr::filter(yes_no_host == "yes" | yes_no_idp == "yes")

weighting_function <- surveyweights::weighting_fun_from_samplingframe(sampling.frame = samplingframe,
                                                                      data = response_hc_idp,
                                                                      sampling.frame.population.column ="Population",
                                                                      sampling.frame.stratum.column = "strata",
                                                                      data.stratum.column = "strata")

msni19::index_chart(response_hc_idp,
                    group = "yes_no_host",
                    group_order = c("yes", "no"),
                    group_labels = c("Non-displaced","IDPs"),
                    index = "msni", 
                    index_max = 4,
                    weighting_function = weighting_function,
                    bar_graph = T,
                    print_plot = T,
                    plot_name = "msni_bar",
                    path = "output/graphs/")

msni19::index_chart(response_hc_idp,
                    group = "yes_no_host",
                    group_order = c("yes", "no"),
                    group_labels = c("Non-displaced","IDPs"),
                    index = "snfi_score", 
                    index_max = 4,
                    index_type = "lsg",
                    weighting_function = weighting_function,
                    print_plot = T,
                    plot_name = "fsl_bar",
                    path = "output/graphs/")

msni19::radar_graph(response_hc_idp, 
                    lsg = c("pev_score", 
                            "edu_score", 
                            "nut_score", 
                            "health_score", 
                            "snfi_score",
                            "fsl_score",
                            "wash_score", 
                            "prot_score",
                            "mcsi_score", 
                            "impact_score"),
                    lsg_labels = c("PEV",
                                   "Edu",
                                   "Nutri",
                                   "Health",
                                   "SNFI",
                                   "Food security", 
                                   "WASH", 
                                   "Prot", 
                                   "MCSI", 
                                   "Impact"),
                    group = "yes_no_host",
                    group_order = c("yes", "no"),
                    group_labels = c("Non-displaced","IDPs"),
                    weighting_function = weighting_function,
                    print_plot = F,
                    plot_name = "radar",
                    path = "output/graphs/")

lsg <- c("pev_score", "edu_score", "nut_score",
         "health_score", "snfi_score", "fsl_score", 
         "wash_score", "prot_score", "mcsi_score", 
         "impact_score")

# Making graph of % of households by # of indices that are >= 3
msni19::severity_lines(response_hc_idp,
                       lsg,
                       group = "yes_no_host",
                       group_order = c("yes", "no"),
                       group_labels = c("Non-displaced","IDPs"),
                       weighting_function = weighting_function,
                       print_plot = T,
                       plot_name = "severity_lines",
                       path = "output/graphs/")

msni19::sunburst_msni(response_hc_idp, msni = "msni", fsl_lsg = "fsl_score", health_lsg = "health_score", 
                      protection_lsg = "prot_score", shelter_lsg = "snfi_score",
                      wash_lsg = "wash_score", capacity_gaps = "mcsi_score", impact = "impact_score", 
                      weighting_function = weighting_function,
                      print_plot = T,
                      plot_name = "general_sunburst",
                      path = "output/graphs/")
# Add the FSL branch
msni19::sunburst_msni(response_hc_idp, msni = "msni", fsl_lsg = "fsl_score", health_lsg = "health_score", 
                      protection_lsg = "prot_score", shelter_lsg = "snfi_score",
                      wash_lsg = "wash_score", capacity_gaps = "mcsi_score", impact = "impact_score", 
                      fsl_wash_branch = T,
                      weighting_function = weighting_function,
                      print_plot = T,
                      plot_name = "full_sunburst",
                      path = "output/graphs/")
