full_data_analysisplan <- function(x) {
  get_parameters <- function(x) {
    part1 <- x[["hypothesis.test"]] %>% unlist()
    part2 <- x[["message"]]
    all_together <- c(part1, part2)
    names(all_together)[length(names(all_together))] <- "message"
    return(all_together)
  }
  parameters_results <- lapply(x[["results"]], get_parameters) %>% do.call(bind_rows,.)
  analysiplan_parameters_results <- cbind(x[["analysisplan"]], parameters_results)
  if("percentcomplete" %in% names(analysiplan_parameters_results)) {
    analysiplan_parameters_results <- analysiplan_parameters_results %>%
      select(-percentcomplete) 
  }
  return(analysiplan_parameters_results)
}

full_data_analysisplan(results_hc_idp) %>% 
  write.csv("output/analysisplan_results.csv", row.names = F)





