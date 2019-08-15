source("functions/borda_count.R")
df_test <- read.csv("input/data/test_set.csv", stringsAsFactors = F)

#simple borda count
## this does throw an error as some value are coded as "" instead of NA.
borda_count(df = df_test, 
            columns = c("water_access_barrier_first", "water_access_barrier_second", "water_access_barrier_third"))

## to solve this use the parameter exclude =
borda_count(df = df_test, 
            columns = c("water_access_barrier_first", "water_access_barrier_second", "water_access_barrier_third"),
            exclude = "")

## "none" is counted as an option. Use exclude to also remove them.
borda_count(df = df_test, 
            columns = c("water_access_barrier_first", "water_access_barrier_second", "water_access_barrier_third"),
            exclude = c("", "none"))

## to add weight
borda_count(df = df_test, 
            columns = c("water_access_barrier_first", "water_access_barrier_second", "water_access_barrier_third"),
            exclude = c("", "none"),
            weights = "general_weights")
