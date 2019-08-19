##unicef led clusters calculations
##needs up to strata from dry_run
un_response <- response

lookup_table <- 
  data.frame("1" = c(1,1,2,2,3),
             "2" = c(2,2,2,3,3),
             "3" = c(2,3,3,3,4),
             "4" = c(3,3,4,4,4),
             "5" = c(3,4,4,5,5))

source("source/unicef/education.R")
source("source/unicef/wash.R")
