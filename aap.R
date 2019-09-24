#aap
analysisplan <- read.csv("input/aap/dap_aap2.csv", stringsAsFactors = F)

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

hypegrammaR:::map_to_generic_hierarchical_html(results_hc_idp,
                                               render_result_with = hypegrammaR:::from_result_map_to_md_table,
                                               by_analysisplan_columns = c("dependent.var","repeat.var.value"),
                                               by_prefix =  c("",""),
                                               level = 2,
                                               questionnaire = questionnaire,
                                               label_varnames = TRUE,
                                               dir = "./output",
                                               filename = "hc_idp_aap2.html")
#make large table
big_table_dist$repeat.var.value[is.na(big_table_dist$repeat.var.value)] <- "national"

variable_to_split_log_by <- "repeat.var.value"
repeat_var_large <- big_table_dist[[variable_to_split_log_by]]
big_table_list <- big_table_dist %>% split.data.frame(list(repeat_var_large))



make_large_table <- function(df) {
  df <- df %>%  
    dplyr::select(repeat.var.value, dependent.var, dependent.var.value, independent.var.value, numbers) %>% 
    mutate(dep.var_dep.var.value = paste(dependent.var, dependent.var.value, sep = "__")) %>% 
    dplyr::select(repeat.var.value, dep.var_dep.var.value, independent.var.value, numbers) %>%
    spread(dep.var_dep.var.value, numbers)
  return(df)
}


large_table <- lapply(big_table_list, make_large_table) %>% do.call(bind_rows, .)
large_table[is.na(large_table)] <- 0

large_table <- large_table[,order(names(large_table))]

order_analysis_plan <- analysisplan$dependent.variable %>% unique()

good_order <- lapply(X = order_analysis_plan, FUN = grep, x = names(large_table), value = T) %>% do.call(c, .)

large_table <- large_table[, c("repeat.var.value", "independent.var.value", good_order)]

large_table_aap <- large_table %>%
  dplyr::select(-c(grep("\\.1", names(large_table), value = T))) %>%
  mutate(aap_neg_outcome_no = 1 - aap_neg_outcome__yes) %>%
  new_recoding(below25) %>%
  recode_directly(sum(c(household_been_consulted_water__yes <= .25, household_been_consulted_sanitaiton__yes <= .25,
                        water_sourcces_well_developed__yes <= .25, aap_contact__yes <= .25, aap_grievance__yes <= .25))) %>%
  new_recoding(between25_50) %>%
  recode_directly(sum(c((household_been_consulted_water__yes > .25 & household_been_consulted_water__yes <= .50),
                        (household_been_consulted_sanitaiton__yes > .25 & household_been_consulted_sanitaiton__yes <= .50),
                        (water_sourcces_well_developed__yes > .25 & water_sourcces_well_developed__yes <= .50),
                        (aap_contact__yes > .25 & aap_contact__yes <= .50),
                        (aap_grievance__yes > .25 & aap_grievance__yes <= .50)))) %>%
  new_recoding(between50_75) %>%
  recode_directly(sum(c((household_been_consulted_water__yes > .50 & household_been_consulted_water__yes <= .75),
                        (household_been_consulted_sanitaiton__yes > .50 & household_been_consulted_sanitaiton__yes <= .75),
                        (water_sourcces_well_developed__yes > .50 & water_sourcces_well_developed__yes <= .75),
                        (aap_contact__yes > .50 & aap_contact__yes <= .75),
                        (aap_grievance__yes > .50 & aap_grievance__yes <= .75)))) %>%
  new_recoding(above75) %>%
  recode_directly(sum(c(household_been_consulted_water__yes > .75, household_been_consulted_sanitaiton__yes > .75,
                        water_sourcces_well_developed__yes > .75, aap_contact__yes > .75, aap_grievance__yes > .75))) %>%
  end_recoding() 
large_table_aap %>% write.csv("output/large_table.csv", row.names = F)

b50_75 <- large_table_aap %>% 
  group_by(between50_75) %>%
  summarise(n = n()) %>% 
  filter(between50_75 != 0) %>%
  summarise(n_dist_b50_75 = sum(n))

b25_50 <- large_table_aap %>% 
  group_by(between25_50) %>%
  summarise(n = n()) %>% 
  filter(between25_50 > 2) %>%
  summarise(n_dist_b25_50 = sum(n))

b_25 <- large_table_aap %>% 
  group_by(below25) %>%
  summarise(n = n()) %>% 
  filter(below25 > 2) %>%
  summarise(n_dist_b_25 = sum(n))

aap_summary <- cbind(b_25, b25_50, b50_75) %>% write.csv("aap_summary.csv", row.names = F)


#district analysis

analysisplan <- read.csv("input/aap/dap_aap4.csv", stringsAsFactors = F)

results_hc_idp <- from_analysisplan_map_to_output(response_hc_idp, 
                                                  analysisplan = analysisplan,
                                                  weighting = strata_weight_fun,
                                                  cluster_variable_name = "settlement",
                                                  questionnaire,
                                                  confidence_level = 0.9)

hypegrammaR:::map_to_generic_hierarchical_html(results_hc_idp,
                                               render_result_with = hypegrammaR:::from_result_map_to_md_table,
                                               by_analysisplan_columns = c("dependent.var","repeat.var.value"),
                                               by_prefix =  c("",""),
                                               level = 2,
                                               questionnaire = questionnaire,
                                               label_varnames = TRUE,
                                               dir = "./output",
                                               filename = "hc_idp_test.html")


browseURL("hc_idp_test.html")

big_table <- results_hc_idp$results %>% lapply(function(x) x[["summary.statistic"]]) %>% do.call(rbind, .)
write.csv(big_table, "output/big_table.csv", row.names = F)

##maps but raster hides select from dplyr.
library(rgdal)
library(leaflet)
# library(raster)
library(RColorBrewer)

big_table_dist <- big_table

big_table_dist <- big_table_dist %>% 
  filter(dependent.var.value %in% c(1, "yes")) %>%
  left_join(
    dplyr::select(list_districts[!duplicated(list_districts$admin2Pcod),], admin2Name, admin2Pcod), 
    by = c("repeat.var.value" = "admin2Name"))

big_table_dist_101 <- big_table_dist %>% filter(dependent.var == "barrier_hum.barrier_sec_point") %>% 
  dplyr::select(numbers, admin2Pcod)


district_bound <- readOGR("../shapefiles/District boundary.shp")

district_bound_102 <- merge(district_bound, big_table_dist_101, by='admin2Pcod')


pal <- colorNumeric("YlOrRd", district_bound_102$numbers)
leaflet(district_bound_102) %>% 
addPolygons( stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5, color = ~colorNumeric("YlOrRd", numbers)(numbers) ) %>%
  addLegend(pal = pal, values = ~numbers, opacity = 0.7, title = NULL,
            position = "bottomright")



