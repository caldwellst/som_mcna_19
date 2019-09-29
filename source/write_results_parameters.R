get_parameters <- function(x) {
  part1 <- x[["hypothesis.test"]] %>% unlist()
  part2 <- x[["message"]]
  all_together <- c(part1, part2)
  names(all_together)[length(names(all_together))] <- "message"
  return(all_together)
}

parameters_results <- lapply(results_hc_idp$results, get_parameters) %>% do.call(rbind,.)

analysiplan_parameters_results <- cbind(analysisplan, parameters_results)

analysiplan_parameters_results %>% write.csv("output/analysisplan_results.csv", row.names = F)
