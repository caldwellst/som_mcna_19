#presentation

analysisplan <- read.csv("input/dap_presentation.csv", stringsAsFactors = F)

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
big_table$repeat.var.value[is.na(big_table$repeat.var.value)] <- "national"

big_table <- big_table 

variable_to_split_log_by <- "repeat.var.value"
repeat_var_long <- big_table[[variable_to_split_log_by]]
big_table_list <- big_table %>% split.data.frame(list(repeat_var_long))



make_long_table <- function(df) {
  df <- df %>%  
    select(repeat.var.value, dependent.var, dependent.var.value, independent.var.value, numbers) %>% 
    mutate(dep.var_dep.var.value = paste(dependent.var, dependent.var.value, sep = "__")) %>% 
    select(repeat.var.value, dep.var_dep.var.value, independent.var.value, numbers) %>%
    spread(dep.var_dep.var.value, numbers)
  return(df)
}


long_table <- lapply(big_table_list, make_long_table) %>% do.call(bind_rows, .)
long_table[is.na(long_table)] <- 0

long_table <- long_table[,order(names(long_table))]

order_analysis_plan <- analysisplan$dependent.variable %>% unique()

good_order <- lapply(X = order_analysis_plan, FUN = grep, x = names(long_table), value = T) %>% do.call(c, .)

long_table <- long_table[, c("repeat.var.value", "independent.var.value", good_order)]
long_table %>% write.csv("output/long_table.csv", row.names = F)


# make_demo_table <- function(df){
#   df <- df %>%
#     #makes category
#     mutate(female_18_59 = females_18_40 + females_41_59,
#            male_18_59 = males_18_40 + males_41_59,
#            female_6_17 = females_5_12 + females_13_15 + females_16_17,
#            male_6_17 = males_5_12 + males_13_15 + males_16_17,
#            females_05 = females_0_6m + females_6m_4y,
#            males_05 = males_0_6m + males_6m_4y,
#            total_hh = males_0_6m + males_6m_4y + females_0_6m + females_6m_4y + females_5_12 + females_13_15 +
#              females_16_17 + males_5_12+  males_13_15+  males_16_17+ females_18_40+ females_41_59 + males_18_40+
#              males_41_59 +  males_60_over + females_60_over,
#            #weigthed groups
#            female_60_w = females_60_over * general_weights,
#            male_60_w = males_60_over * general_weights,
#            female_18_59_w = female_18_59 * general_weights,
#            male_18_59w = male_18_59 * general_weights,
#            female_6_17w = female_6_17 * general_weights,
#            male_6_17_w = male_6_17 * general_weights,
#            females_05w = females_05 * general_weights,
#            males_05w = males_05 * general_weights,
#            hh_w = total_hh * general_weights) %>%
#     #sum
#     group_by(population_group) %>%
#     summarise(total_female_60_w = sum(female_60_w),
#               total_male_60_w = sum(male_60_w),
#               total_female_18_59_w = sum(female_18_59_w),
#               total_male_18_59w = sum(male_18_59w),
#               total_female_6_17w = sum(female_6_17w),
#               total_male_6_17_w = sum(male_6_17_w),
#               total_females_05w = sum(females_05w),
#               total_males_05w = sum(males_05w),
#               total_hh_w = sum(hh_w),
#               total_female_w = total_female_60_w + total_female_18_59_w + total_female_6_17w + total_females_05w,
#               total_male_w = total_male_60_w + total_male_18_59w + total_male_6_17_w + total_males_05w) %>%
#     #prop
#     mutate(perc_total_female = total_female_w / total_hh_w,
#            perc_total_male = total_male_w / total_hh_w,
#            perc_total_female_60 = total_female_60_w / total_hh_w,
#            perc_total_male_60 = total_male_60_w / total_hh_w,
#            perc_total_female_18_59 = total_female_18_59_w / total_hh_w,
#            perc_total_male_18_59 = total_male_18_59w / total_hh_w,
#            perc_total_female_6_17 = total_female_6_17w / total_hh_w,
#            perc_total_male_6_17 = total_male_6_17_w / total_hh_w,
#            perc_total_females_05 = total_females_05w / total_hh_w,
#            perc_total_males_05 = total_males_05w / total_hh_w,
#            verif1 = perc_total_female + perc_total_male,
#            verif2 = perc_total_female_60 + perc_total_male_60 + perc_total_female_18_59 + perc_total_male_18_59 +
#              perc_total_female_6_17 + perc_total_male_6_17 + perc_total_females_05 + perc_total_males_05) %>%
#     select(population_group, grep("perc_total", names(.), value = T), verif1, verif2)
#   return(df)
# }
# 
# variable_to_split_log_by <- "statex7"
# repeat_var_long <- response_hc_idp[[variable_to_split_log_by]]
# df_list <- response_hc_idp %>%
#   split.data.frame(list(repeat_var_long))
# 
# 
# national_demo <- make_demo_table(response_hc_idp)
# national_demo$repeat.var.value <- "national"
# 
# states_demo <- lapply(df_list, make_demo_table) %>% do.call(rbind, .)
# states_demo$repeat.var.value <- row.names(states_demo)
# states_demo$repeat.var.value <- sub(pattern = "\\.[1-9]", replacement = "", states_demo$repeat.var.value)
# 
# 
# all_demo <- rbind(national_demo,states_demo) 
# all_demo %>% write.csv("output/all_demographics.csv", row.names = F)
