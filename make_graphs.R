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


df_to_plot <- response_hc_idp

# sunburst page 1
msni19::sunburst_msni(df_to_plot, 
                      msni = "msni", fsl_lsg = "fsl_score", health_lsg = "health_score", 
                      protection_lsg = "prot_score", shelter_lsg = "snfi_score",
                      wash_lsg = "wash_score", capacity_gaps = "mcsi_score", impact = "impact_score", 
                      fsl_wash_branch = T,
                      weighting_function = weighting_function,
                      print_plot = T,
                      plot_name = "page1_full_sunburst",
                      path = "output/graphs/")

# line chart page 2
msni19::index_chart(df_to_plot,
                    group = "yes_no_host",
                    group_order = c("yes", "no"),
                    group_labels = c("Non-displaced","IDPs"),
                    index = "msni", 
                    index_max = 4,
                    weighting_function = weighting_function,
                    bar_graph = F,
                    print_plot = T,
                    plot_name = "page2_msni_line",
                    path = "output/graphs/")

# bar chart page 2
msni19::index_chart(df_to_plot,
                    group = "yes_no_host",
                    group_order = c("yes", "no"),
                    group_labels = c("Non-displaced","IDPs"),
                    index = "msni", 
                    index_max = 4,
                    weighting_function = weighting_function,
                    bar_graph = T,
                    print_plot = T,
                    plot_name = "page2_msni_bar",
                    path = "output/graphs/")



# sunburst page 2 group A : 
msni19::sunburst_msni(
  # dplyr::filter(response_hc_idp, yes_no_host == "no"),
                      msni = "msni", fsl_lsg = "fsl_score", health_lsg = "health_score", 
                      protection_lsg = "prot_score", shelter_lsg = "snfi_score",
                      wash_lsg = "wash_score", capacity_gaps = "mcsi_score", impact = "impact_score", 
                      fsl_wash_branch = T,
                      weighting_function = weighting_function,
                      print_plot = T,
                      plot_name = "page_2_full_sunburst_A",
                      path = "output/graphs/")
# sunburst page 2 group B :
msni19::sunburst_msni(
  # dplyr::filter(response_hc_idp, yes_no_host == "no"),
                      msni = "msni", fsl_lsg = "fsl_score", health_lsg = "health_score", 
                      protection_lsg = "prot_score", shelter_lsg = "snfi_score",
                      wash_lsg = "wash_score", capacity_gaps = "mcsi_score", impact = "impact_score", 
                      fsl_wash_branch = T,
                      weighting_function = weighting_function,
                      print_plot = T,
                      plot_name = "page_2_full_sunburst_B",
                      path = "output/graphs/")
# sunburst page 2 group C :
msni19::sunburst_msni( 
  # dplyr::filter(response_hc_idp, yes_no_host == "no"),
                      msni = "msni", fsl_lsg = "fsl_score", health_lsg = "health_score", 
                      protection_lsg = "prot_score", shelter_lsg = "snfi_score",
                      wash_lsg = "wash_score", capacity_gaps = "mcsi_score", impact = "impact_score", 
                      fsl_wash_branch = T,
                      weighting_function = weighting_function,
                      print_plot = T,
                      plot_name = "page_2_full_sunburst_C",
                      path = "output/graphs/")

# any lsg graph bar and line

make_bar_line_graph <- function(df, page, lsg_to_graph) {
  graph_name_bar <- paste0(page, "_", lsg_to_graph, "_bar")
  bar_gr <- msni19::index_chart(df,
                                group = "yes_no_host",
                                group_order = c("yes", "no"),
                                group_labels = c("Non-displaced","IDPs"),
                                index = lsg_to_graph, 
                                index_max = 4,
                                weighting_function = weighting_function,
                                bar_graph = T,
                                print_plot = T,
                                plot_name = graph_name_bar,
                                path = "output/graphs/")
  line_name_bar <- paste0(page, "_", lsg_to_graph, "_line")
  
  line_gr <- msni19::index_chart(df,
                                 group = "yes_no_host",
                                 group_order = c("yes", "no"),
                                 group_labels = c("Non-displaced","IDPs"),
                                 index = lsg_to_graph, 
                                 index_max = 4,
                                 weighting_function = weighting_function,
                                 bar_graph = F,
                                 print_plot = T,
                                 plot_name = line_name_bar,
                                 path = "output/graphs/")
  print(bar_gr)
  print(line_gr)
}


simple_chart_and_page <- data.frame(page = paste0("page", 3:10),
                                       lsg = c("wash_score", "health_score", "snfi_score", "edu_score", "prot_score",
                                            "mcsi_score", "pev_score", "impact_score"),
                                            stringsAsFactors = F)

mapply(make_bar_line_graph, page = simple_chart_and_page$page, lsg_to_graph = simple_chart_and_page$lsg, 
       MoreArgs = list(df = df_to_plot))

#page 11 radar

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

msni19::sunburst_msni(response_hc_idp, 
                      msni = "msni", fsl_lsg = "fsl_score", health_lsg = "health_score", 
                        protection_lsg = "prot_score", shelter_lsg = "snfi_score",
                        wash_lsg = "wash_score", capacity_gaps = "mcsi_score", impact = "impact_score", 
                      # weighting_function = weighting_function,
                      print_plot = T,
                      plot_name = "general_sunburst",
                      path = "output/graphs/")


# Only for households with MSNI >= 4
msni19::sunburst_msni(response_hc_idp, 
                      msni = "msni", fsl_lsg = "fsl_score", health_lsg = "health_score", 
                        protection_lsg = "prot_score", shelter_lsg = "snfi_score",
                        wash_lsg = "wash_score", capacity_gaps = "mcsi_score", impact = "impact_score", 
                      msni_filter = c(4, 5),
                      # weighting_function = weighting_function,
                      print_plot = T,
                      plot_name = "severe_msni_sunburst",
                      path = "output/graphs/")

# Only for non-displaced households
msni19::sunburst_msni(dplyr::filter(response_hc_idp, yes_no_host == "no"),
                      msni = "msni", fsl_lsg = "fsl_score", health_lsg = "health_score", 
                        protection_lsg = "prot_score", shelter_lsg = "snfi_score", 
                        wash_lsg = "wash_score", capacity_gaps = "mcsi_score", impact = "impact_score", 
                      # weighting_function = weighting_function,
                      print_plot = T,
                      plot_name = "non_displaced_sunburst",
                      path = "output/graphs/")

# venn diagram of households with any LSG >= 3 (REACH red) and those with capacity gaps >= 3 (REACH light grey)

msni19::venn_msni(response_hc_idp, 
                  lsg = c("edu_score", 
                          "snfi_score", 
                          "fsl_score", 
                          "health_score", 
                          "prot_score",
                          "wash_score"),
                  capacity_gaps = "mcsi_score",
                  weighting_function = weighting_function,
                  print_plot = T,
                  path = "output/graphs/")
