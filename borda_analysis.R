source("MCNA_dry_run.R")
source("functions/borda_count.R")

borda_script <- readr::read_csv("input/borda/borda_analysis.csv")

borda_applier(borda_script, response_hc_idp, strata_weight_fun)
