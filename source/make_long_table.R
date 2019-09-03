#make long table
big_table$repeat.var.value[is.na(big_table$repeat.var.value)] <- "national"

big_table <- big_table %>%
  filter(repeat.var.value %in% c("national", "somaliland", "banadir"))

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