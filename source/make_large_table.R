#make large table
big_table$repeat.var.value[is.na(big_table$repeat.var.value)] <- "national"

variable_to_split_log_by <- "repeat.var.value"
repeat_var_large <- big_table[[variable_to_split_log_by]]
big_table_list <- big_table %>% split.data.frame(list(repeat_var_large))



make_large_table <- function(df) {
  df <- df %>%  
    select(repeat.var.value, dependent.var, dependent.var.value, independent.var.value, numbers) %>% 
    mutate(dep.var_dep.var.value = paste(dependent.var, dependent.var.value, sep = "__")) %>% 
    select(repeat.var.value, dep.var_dep.var.value, independent.var.value, numbers) %>%
    spread(dep.var_dep.var.value, numbers)
  return(df)
}


large_table <- lapply(big_table_list, make_large_table) %>% do.call(bind_rows, .)
large_table[is.na(large_table)] <- 0

large_table <- large_table[,order(names(large_table))]

order_analysis_plan <- analysisplan$dependent.variable %>% unique()

good_order <- lapply(X = order_analysis_plan, FUN = grep, x = names(large_table), value = T) %>% do.call(c, .)

large_table <- large_table[, c("repeat.var.value", "independent.var.value", good_order)]
large_table %>% write.csv("output/large_table.csv", row.names = F)
