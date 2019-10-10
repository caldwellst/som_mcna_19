analysisplan <- read.csv("input/dap_merge_FS_part1.csv", stringsAsFactors = F)

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

big_table <- results_hc_idp$results %>% lapply(function(x) x[["summary.statistic"]]) %>% do.call(rbind, .) %>%
  select(dependent.var, independent.var, dependent.var.value, independent.var.value, numbers)

rows_to_add <- big_table %>% filter(dependent.var %in% grep("_score$", big_table$dependent.var, value = T)) %>%
  group_by(dependent.var, independent.var.value) %>%
  summarise(n = n()) %>%
  filter(n < 4) %>% 
  mutate(dependent.var.value = 4,
         independent.var = ifelse(is.na(independent.var.value), NA, "population_group"),
         numbers = 0) %>% 
  select(-n) %>%
  data.frame(stringsAsFactors = F)

big_table <- rbind(big_table, rows_to_add)

no_independent_var <-  big_table %>% 
  filter(is.na(independent.var) | independent.var == "NA",
         !(dependent.var.value %in% c(FALSE, "not_in_need", "no_lsg_no_cg", "not_female_hh")))  %>%
  mutate(merge_name = paste0(dependent.var, "__", "NA__", dependent.var.value)) %>%
  select(merge_name, numbers) %>%
  arrange(merge_name) %>%
  spread(merge_name, numbers)

with_independent_var <-big_table %>% 
  filter(!is.na(independent.var), 
         dependent.var.value %in% c(1:5, TRUE, "in_need")) %>% 
  mutate(merge_name = paste0(dependent.var, "__", independent.var, "__", independent.var.value, "__", dependent.var.value)) %>%
  select(merge_name, numbers) %>% 
  arrange(merge_name) %>%
  spread(merge_name, numbers)


merge <- cbind(no_independent_var, with_independent_var)

names_big_bars_to_add <- grep("NA__[1-9]$", names(merge), value = T)
add_big_bars <- function(x) {
  score_to_add <- merge[,x] * 10 # *10 for the bar size
  return(score_to_add)
}
value_big_bars_to_add <- lapply(names_big_bars_to_add, add_big_bars) %>% do.call(cbind, .) %>% data.frame()
names(value_big_bars_to_add) <- paste0("big_bar_", names_big_bars_to_add)

merge <- cbind(merge, value_big_bars_to_add)

sum(is.na(response[, names(response)[23:38]]))

demo <- response_hc_idp %>%
  #makes category
  mutate(female_18_59 = females_18_40 + females_41_59,
         male_18_59 = males_18_40 + males_41_59,
         female_6_17 = females_5_12 + females_13_15 + females_16_17,
         male_6_17 = males_5_12 + males_13_15 + males_16_17,
         females_05 = females_0_6m + females_6m_4y,
         males_05 = males_0_6m + males_6m_4y,
         total_hh = males_0_6m + males_6m_4y + females_0_6m + females_6m_4y + females_5_12 + females_13_15 + 
           females_16_17 + males_5_12+  males_13_15+  males_16_17+ females_18_40+ females_41_59 + males_18_40+ 
           males_41_59 +  males_60_over + females_60_over,
  #weigthed groups
         female_60_w = females_60_over * general_weights,
         male_60_w = males_60_over * general_weights,
         female_18_59_w = female_18_59 * general_weights,
         male_18_59w = male_18_59 * general_weights,
         female_6_17w = female_6_17 * general_weights,
         male_6_17_w = male_6_17 * general_weights,
         females_05w = females_05 * general_weights,
         males_05w = males_05 * general_weights,
         hh_w = total_hh * general_weights) %>%
  #sum
  summarise(total_female_60_w = sum(female_60_w),
            total_male_60_w = sum(male_60_w),
            total_female_18_59_w = sum(female_18_59_w),
            total_male_18_59w = sum(male_18_59w),
            total_female_6_17w = sum(female_6_17w),
            total_male_6_17_w = sum(male_6_17_w),
            total_females_05w = sum(females_05w),
            total_males_05w = sum(males_05w),
            total_hh_w = sum(hh_w),
            total_female_w = total_female_60_w + total_female_18_59_w + total_female_6_17w + total_females_05w,
            total_male_w = total_male_60_w + total_male_18_59w + total_male_6_17_w + total_males_05w) %>%
  #prop
  mutate(perc_total_female = total_female_w / total_hh_w,
         perc_total_male = total_male_w / total_hh_w, 
         perc_total_female_60 = total_female_60_w / total_hh_w, 
         perc_total_male_60 = total_male_60_w / total_hh_w, 
         perc_total_female_18_59 = total_female_18_59_w / total_hh_w, 
         perc_total_male_18_59 = total_male_18_59w / total_hh_w, 
         perc_total_female_6_17 = total_female_6_17w / total_hh_w, 
         perc_total_male_6_17 = total_male_6_17_w / total_hh_w, 
         perc_total_females_05 = total_females_05w / total_hh_w, 
         perc_total_males_05 = total_males_05w / total_hh_w,
         verif1 = perc_total_female + perc_total_male,
         verif2 = perc_total_female_60 + perc_total_male_60 + perc_total_female_18_59 + perc_total_male_18_59 +
           perc_total_female_6_17 + perc_total_male_6_17 + perc_total_females_05 + perc_total_males_05) %>%
  select(grep("perc_total", names(.), value = T))

merge <- cbind(merge, demo)

order_lsg <- c("wash_score", "health_score", "snfi_score", "edu_score", "prot_score",
               "mcsi_score", "pev_score", "impact_score") 

page_order_fun <- function(x) {
  general_sev_3_above <- paste0(x, "_2_cat__NA")
  general_big_bar <- paste0("big_bar_", x)
  general_score_na <- paste0(x, "__NA")
  per_pop_sev_3_above <- paste0(x, "_2_cat__pop")
  order_to_add <- c(grep(general_sev_3_above, names(merge), value = T),
                    grep(general_big_bar, names(merge), value = T),
                    grep(general_score_na, names(merge), value = T),
                    grep(per_pop_sev_3_above, names(merge), value = T))
  return(order_to_add)
}

page_order <- c(
  #page1
  grep("perc_total", names(merge), value = T),
  grep("female_hh__NA__", names(merge), value = T),
  grep("total_hh__NA__", names(merge), value = T),
  grep("msni_2_cat__NA", names(merge), value = T),
  grep("big_bar_msni__NA__", names(merge), value = T),
  grep("msni__NA__", names(merge), value = T),
  #page2
  grep("msni_2_cat__po", names(merge), value = T),
  #page3 - 10
  lapply(order_lsg, page_order_fun) %>% do.call(c,.),
  #page 11
  grep("at_least_lsg_above_sev_3__NA__TRUE", names(merge), value = T),
  grep("lsg_or_cg__NA", names(merge), value = T),
  grep("lsg_cg__NA__one_lsg_no_cg", names(merge), value = T),
  grep("lsg_cg__NA__one_lsg_one_cg", names(merge), value = T),
  grep("lsg_cg__NA__no_lsg_one_cg", names(merge), value = T))

merge <- merge[, page_order] 
merge[, (names(merge) != "total_hh__NA__NA")] <- merge[, (names(merge) != "total_hh__NA__NA")] * 100

merge <- merge[, names(merge)[!(names(merge) %in%  grep("\\.[1-9]$", names(merge), value = T))]] #removing the duplicates with the big_bar

merge %>% write.csv("output/merge_FS_no_pictures.csv", row.names = F)


 