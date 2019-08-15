library(purrr)
library(dplyr)

# replace values for column with NAs based on values within a vector

make_na <- function(x, vals) {
  x[x %in% vals] <- NA
  x
}

#' function to get weighted borda count for any number of vectors
#'
#' @param df data frame
#' @param columns vectors to be ranked
#' @param weights weight vector. Should be the same length as vectors provided for ranking
#' @param exclude vector of values to exclude from ranking analysis
#' @param ranks number of ranks to be calculated
borda_count <- function(df, columns, weights = NULL, exclude = NULL, ranks = 3) {
  if (is.null(weights)) {
    weights <- rep(1, length(df[[1]]))
  } else {
    weights <- df[[weights]]
  }
  
  df <- df[, columns]
  if (!is.null(exclude)) {
    df <- map(df, make_na, exclude) # excluding values in the vector provided above
  }
  vals <- map(df, ~ unique(.x[!is.na(.x)]))
  vals <- unique(unlist(vals)) # getting unique values to rank
  ranks <- min(ranks, length(vals)) # ensuring that end result doesn't have NA values for calculating for non-existent rankings
  print(vals)
  table <- map_dfc(vals, ~ tibble(!!(.x) := map_dbl(df, function(x) sum((x == .x) * weights, na.rm = T)))) %>% # counting the weighted occurences of each value
    mutate_all(~ .x * (length(vals) - 1:n() + 1))  %>% # multiplying row values by their borda rank
    summarize_all(sum) # getting the total vote score for each item

  table <- table[,order(-table[1,])] # reordering based on score
  print(table)
  names(table)[1:ranks] # getting the total borda count for each
}


