library(purrr)
library(dplyr)

# replace values for column with NAs based on values within a vector

make_na <- function(x, vals) {
  x[x %in% vals] <- NA
  x
}

#' function to get weighted borda count for any number of vectors
#'
#' @param ... vectors to be ranked
#' @param weights weight vector. Should be the same length as vectors provided for ranking
#' @param exclude vector of values to exclude from ranking analysis
#' @param ranks number of ranks to be calculated
borda_count <- function(..., weights = NULL, exclude = NULL, ranks = 3) {
  args <- list(...)
  if (!is.null(exclude)) {
    args <- map(args, make_na, exclude) # excluding values in the vector provided above
  }
  vals <- map(args, ~ unique(.x[!is.na(.x)]))
  vals <- unique(unlist(vals)) # getting unique values to rank
  ranks <- min(ranks, length(vals)) # ensuring that end result doesn't have NA values for calculating for non-existent rankings
  if (is.null(weights)) {
    weights <- rep(1, length(args[[1]]))
  }
  
  table <- map_dfc(vals, ~ tibble(!!(.x) := map_dbl(args, function(x) sum((x == .x) * weights, na.rm = T)))) %>% # counting the weighted occurences of each value
    mutate_all(~ .x * (length(args) - 1:n() + 1))  %>% # multiplying row values by their borda rank
    summarize_all(sum) # getting the total vote score for each item
  
  table <- table[,order(-table[1,])] # reordering based on score
  names(table)[1:ranks] # getting the total borda count for each
}

