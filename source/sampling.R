library(readxl)

#cluster sampling hc
# clustersamplingframe_hc <- read_xlsx("input/sample/master_lastupdate09072019.xlsx", sheet = "hc_sf")
clustersamplingframe_hc <- read_xlsx("input/sample/master_lastupdate14082019.xlsx", sheet = "hc_sf")


col_to_keep_hc <- c("Survey", "strata_id", "NAME", "Region", "District", "P_CODE",
                    "psu_id",	"pop_numbers", "SumDist", "proba")

clustersamplingframe_hc <- clustersamplingframe_hc[, col_to_keep_hc]
clustersamplingframe_hc$population_type <- "hc"

#cluster sampling idp
# clustersamplingframe_idp <- read_xlsx("input/sample/master_lastupdate09072019.xlsx", sheet = "idp_sf")
clustersamplingframe_idp <- read_xlsx("input/sample/master_lastupdate14082019.xlsx", sheet = "idp_sf")


col_to_keep_idp <- c("Survey", "strata_id", "ass_site", "ass_region", "ass_district", 
                     "P.Code", "psu_id", "pop_numbers", "SumDist", "proba")

clustersamplingframe_idp <- clustersamplingframe_idp[, col_to_keep_idp]
names(clustersamplingframe_idp) <- col_to_keep_hc
clustersamplingframe_idp$population_type <- "idp"

#cluster sampling
clustersamplingframe <- rbind(clustersamplingframe_hc, clustersamplingframe_idp)
clustersamplingframe$strata <- paste0(clustersamplingframe$strata_id, "__", clustersamplingframe$population_type)

#sampling frame
# samplingframe <- read_xlsx("input/sample/master_lastupdate09072019.xlsx", sheet = "summary")
samplingframe <- read_xlsx("input/sample/master_lastupdate14082019.xlsx", sheet = "summary")

samplingframe %>% names()

samplingframe$strata <- paste0(samplingframe$Stratification, "__", samplingframe$population)

samplingframe <- samplingframe %>%
  dplyr::select(- population) %>%
  as.data.frame()

#verification
sum(unique(clustersamplingframe$strata) %in% samplingframe$strata) / length(unique(clustersamplingframe$strata))
sum(unique(samplingframe$strata) %in% clustersamplingframe$strata) / length(unique(samplingframe$strata))

unique(clustersamplingframe$strata)[!(unique(clustersamplingframe$strata) %in% samplingframe$strata)]
unique(samplingframe$strata)[!(unique(samplingframe$strata) %in% clustersamplingframe$strata)]
rm(clustersamplingframe_hc, clustersamplingframe_idp)

