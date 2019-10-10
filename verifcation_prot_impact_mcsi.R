impact_names <- c("drought_score",
                  "conflict_environment_score",
                  "reason_separation_score",
                  "reason_lost_employment_score",
                  "reason_shelter_damage_score",
                  "reason_displacement_score",
                  "barriers_hum_score"
                  )

impact<- lapply(X = impact_names, FUN = function(x) table(response_hc_idp[[x]], useNA = "ifany"))
names(impact) <- impact_names
impact


protection_names <- c("freedom_movement_score",
                      "family_separation_score",
                      "safety_security_concern_score",
                      "hazardous_work_score",
                      "land_ownership_score",
                      "hlp_resolution_score",
                      "land_seizure_score",
                      "sgbv_referral_score",
                      "sgbv_justice_recourse_score",
                      "access_judicial_remedy_score",
                      "justice_recourse_score",
                      "child_injury_score",
                      "cfs_score",
                      "exploitation_score",
                      "women_committee_score",
                      "relation_hc_idp_score"
                      )

protection<- lapply(X = protection_names, FUN = function(x) table(response_hc_idp[[x]], useNA = "ifany"))
names(protection) <- protection_names
protection


mcsi_names <- c("mcsi_water_score",
                "mcsi_sanitation_score",
                "mcsi_hygienic_mat_score",
                "mcsi_food_score",
                "mcsi_shelter_score",
                "mcsi_nfi_score",
                "mcsi_education_score",
                "mcsi_health_score"
)

mcsi<- lapply(X = mcsi_names, FUN = function(x) table(response_hc_idp[[x]], useNA = "ifany"))
names(mcsi) <- mcsi_names
mcsi


impact %>% unlist() %>% write.csv("impact2.csv")
protection %>% unlist() %>% write.csv("protection2.csv")
mcsi %>% unlist() %>% write.csv("mcsi2.csv")

lvg_names <- c("pev_score",
               "edu_score",
               "nut_score",
               "health_score",
               "snfi_score",
               "fsl_score",
               "wash_score",
               "prot_score",                                             
               "mcsi_score",
               "impact_score" )


lvg<- lapply(X = lvg_names, FUN = function(x) table(response_hc_idp2[[x]], useNA = "ifany"))
names(lvg) <- lvg_names
lvg %>% unlist() %>% write.csv("lvg.csv")

response_hc_idp2 <- readRDS("input/data/02-data_final_scoring29082019.RDS")
response_hc_idp2 <- response_hc_idp2 %>%
  dplyr::filter(strata %in% samplingframe$strata) %>%
  dplyr::filter(yes_no_host == "yes" | yes_no_idp == "yes")
