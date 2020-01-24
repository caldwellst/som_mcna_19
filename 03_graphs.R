#make the graphs

library(dplyr)
library(koboquest) # manage kobo questionnairs
library(kobostandards) # check inputs for inconsistencies
library(xlsformfill) # generate fake data for kobo
library(hypegrammaR) # stats 4 complex samples
library(composr) # horziontal operations
library(msni19) # tata!
library(surveyweights)
library(plotly)
library(lubridate)

source("functions/to_alphanumeric_lowercase.R") # function to standardise column headers (like check.names)
source("functions/analysisplan_factory.R")  # generate analysis plans

df <- readRDS("input/data/final_dataset_analysis.RDS")

weighting_function <- function(df) {
  df$general_weights
}

## Settings to make Regions Plots

# regions <- unique(df$region)
# region_name <- regions[1]
# df_to_plot <- filter(response_hc_idp, region == region_name)

#df_to_plot <- response

# sunburst page 1
msni19::sunburst_msni(df, 
                      msni = "msni", fsl_lsg = "fsl_score", health_lsg = "health_score", 
                      protection_lsg = "prot_score", shelter_lsg = "snfi_score",
                      wash_lsg = "wash_score", capacity_gaps = "mcsi_score", impact = "impact_score", 
                      fsl_wash_branch = T,
                      weighting_function = weighting_function,
                      print_plot = T,
                      plot_name = "full_sunburst",
                      path = "output/graphs/")


# line chart page 2
msni19::index_chart(df,
                    group = "yes_no_host",
                    group_order = c("yes", "no"),
                    group_labels = c("Non-displaced","IDP"),
                    index = "msni", 
                    index_max = 4,
                    weighting_function = weighting_function,
                    bar_graph = F,
                    print_plot = T,
                    plot_name = "msni_line",
                    path = "output/graphs/")

# bar chart page 2
msni19::index_chart(df,
                    group = "yes_no_host",
                    group_order = c("yes", "no"),
                    group_labels = c("Non-displaced","IDP"),
                    index = "msni", 
                    index_max = 4,
                    weighting_function = weighting_function,
                    bar_graph = T,
                    print_plot = T,
                    plot_name = "msni_bar",
                    path = "output/graphs/")

#page 11 radar
msni19::radar_graph(df, 
                    lsg = c("edu_score", 
                            "nut_score", 
                            "health_score", 
                            "snfi_score",
                            "fsl_score",
                            "wash_score", 
                            "prot_score"),
                    lsg_labels = c("Edu",
                                   "Nutri",
                                   "Health",
                                   "SNFI",
                                   "Food sec", 
                                   "WASH", 
                                   "Prot"),
                    group = "yes_no_host",
                    group_order = c("yes", "no"),
                    group_labels = c("Non-displaced","IDP"),
                    weighting_function = weighting_function,
                    print_plot = T,
                    plot_name = "radar",
                    legend_position = "bottom", 
                    path = "output/graphs/")

#page 11 line graph
lsg <- c("edu_score", "nut_score",
         "health_score", "snfi_score", "fsl_score", 
         "wash_score", "prot_score")

# Making graph of % of households by # of indices that are >= 3
msni19::severity_lines(df_to_plot,
                       lsg,
                       group = "yes_no_host",
                       group_order = c("yes", "no"),
                       group_labels = c("Non-displaced","IDP"),
                       weighting_function = weighting_function,
                       print_plot = T,
                       plot_name = paste0(regions, "page11_over3"),
                       path = "output/graphs/")

#page 11 venn
# venn diagram of households with any LSG >= 3 (REACH red) and those with capacity gaps >= 3 (REACH light grey)

msni19::venn_msni(df_to_plot, 
                  lsg = c("edu_score", 
                          "snfi_score", 
                          "fsl_score", 
                          "health_score", 
                          "prot_score",
                          "wash_score",
                          "nut_score"),
                  capacity_gaps = "mcsi_score",
                  weighting_function = weighting_function,
                  print_plot = T,
                  plot_name = paste0(regions, "page11_venn"),
                  path = "output/graphs/")


#page 11 intersection
msni19::index_intersections(df_to_plot,
                            lsg =  c("edu_score", "snfi_score", "fsl_score", "health_score",
                                     "prot_score", "wash_score", "nut_score"),
                            lsg_labels = c("Education",
                                           "Shelter",
                                           "Food",
                                           "Health",
                                           "Protection",
                                           "WASH",
                                           "Nutrition"),
                            weighting_function = weighting_function,
                            print_plot = F,
                            plot_name = paste0(regions,"page11_intersection"),
                            path = "output/graphs/")



