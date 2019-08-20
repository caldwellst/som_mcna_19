# horizontal aggregation
## protection

response <- 
  response %>%
  #1.1 freedom of movement
  new_recoding(target = freedom_movement_score) %>%
  recode_to(to = 1, where = free_movement == "yes") %>%
  recode_to(to = 3, where = free_movement == "yes" & unsafe_male == "yes") %>%
  recode_to(to = 4, where = free_movement == "yes" & unsafe_female == "yes") %>%
  recode_to(to = 5, where = free_movement == "no") %>%
  recode_to(to = 6, where = free_movement == "no" & unsafe_male == "yes" & unsafe_female == "yes") %>%
  #2.1 family separation
  new_recoding(target = family_separation_score) %>%
  recode_directly(to_expression = 1) %>%
  #3.1 safety and security concern
  new_recoding(target = safety_security_concern_score) %>%
  recode_to(to = 8, where = sgbv == "always" | grave_injury == "always" | abductions == "always" | 
              uxo == "always" | death == "always") %>%
  recode_to(to = 7, where = uxo == "sometimes" | abductions == "sometimes" | death == "sometimes") %>%
  recode_to(to = 6, where = sgbv == "sometimes" | grave_injury == "sometimes") %>%
  recode_to(to = 5, where = theft_harassment == "always" | light_injury == "always") %>%
  recode_to(to = 4, where = light_injury == "sometimes") %>%
  recode_to(to = 3, where = theft_harassment == "sometimes") %>%
  recode_to(to = 1, where = sgbv == "never" & grave_injury == "never" & abductions == "never" & 
              uxo == "never" & death == "never" & theft_harassment == "never" & light_injury == "never") %>%
  #4.1 hazardous or exploitive work 
  new_recoding(target = hazardous_work_score) %>%
  recode_directly(to_expression = 1) %>%
  #5.1 land ownership and documentation
  new_recoding(target = land_ownership_score) %>%
  recode_to(to = 8, where = own_land == "no" & doc_land_tenure == "no" & obtain_title == "no") %>%
  recode_to(to = 7, where = own_land == "no" & doc_land_tenure == "no" & obtain_title == "yes") %>%
  recode_to(to = 6, where = own_land == "no" & doc_land_tenure == "yes" & obtain_title == "no") %>%
  recode_to(to = 5, where = own_land == "no" & doc_land_tenure == "yes" & obtain_title == "yes") %>%
  recode_to(to = 4, where = own_land == "yes" & doc_land_tenure == "no" & obtain_title == "no") %>%
  recode_to(to = 3, where = own_land == "yes" & doc_land_tenure == "no" & obtain_title == "yes") %>%
  recode_to(to = 2, where = own_land == "yes" & doc_land_tenure == "yes" & obtain_title == "no") %>%
  recode_to(to = 1, where = own_land == "yes" & doc_land_tenure == "yes" & obtain_title == "yes") %>%
  #5.2 hlp dispute and resolution
  new_recoding(target = hlp_resolution_score) %>%
  recode_to(to = 1, where = hlp_dispute == "no") %>%
  recode_to(to = 2, where = hlp_dispute == "yes" & hlp_dispute_mech_use == "yes" & hlp_dispute_mech_satisfaction == "yes") %>%
  recode_to(to = 3, where = hlp_dispute == "yes" & hlp_dispute_mech_use == "no") %>%
  recode_to(to = 4, where = hlp_dispute == "yes" & hlp_dispute_mech_use == "yes" & hlp_dispute_mech_satisfaction == "no") %>%
  #5.3 land seizure
  new_recoding(target = land_seizure_score, source =land_grab) %>%
  recode_to(to = 1, where.selected.any = "no") %>%
  recode_to(to = 2, where.selected.any = "yes") %>%
  #6.1 GBV referral
  new_recoding(target = sgbv_referral_score) %>%
  recode_to(to = 1, where = sgbv_recourse_awareness == "yes") %>%
  recode_to(to = 3, where = sgbv_recourse_awareness == "yes" & sgbv_recourse_use == "yes" & sgbv_recourse_use_satisfaction == "yes") %>%
  recode_to(to = 5, where = sgbv_recourse_awareness == "no") %>%
  recode_to(to = 7, where = sgbv_recourse_use == "yes" & sgbv_recourse_use_satisfaction == "no") %>%
  #6.2 GBV recourse to justice
  new_recoding(target = sgbv_justice_recourse_score) %>%
  recode_to(to = 1, where = sgbv_recourse_women.police == 1) %>%
  recode_to(to = 2, where = sgbv_recourse_women.comm_elders == 1 | sgbv_recourse_women.comm_leader == 1) %>%
  recode_to(to = 3, where = sgbv_recourse_women.un_ngo == 1 | sgbv_recourse_women.health_centre == 1) %>%
  recode_to(to = 5, where = sgbv_recourse_women.armed_group == 1) %>%
  recode_to(to = 7, where = sgbv_recourse_women.none == 1 | sgbv_recourse_women.no_report == 1) %>%
  #7.1 access to judicial remedy
  new_recoding(target = access_judicial_remedy_score) %>%
  recode_to(to = 1, where = judicial_remedy == "yes" & judicial_remedy_effective == "yes") %>%
  recode_to(to = 3, where = judicial_remedy == "yes" & judicial_remedy_effective == "no") %>%
  recode_to(to = 5, where = judicial_remedy == "no" & judicial_remedy_effective == "yes") %>%
  recode_to(to = 7, where = judicial_remedy == "no" & judicial_remedy_effective == "no") %>%
  #7.2 recourse to justice
  new_recoding(target = justice_recourse_score) %>%
  recode_to(to = 1, where = crime_recourse_hh.police == 1) %>%
  recode_to(to = 2, where = crime_recourse_hh.comm_elders == 1 | crime_recourse_hh.comm_leader == 1) %>%
  recode_to(to = 3, where = crime_recourse_hh.un_ngo == 1 | crime_recourse_hh.health_centre == 1) %>%
  recode_to(to = 5, where = crime_recourse_hh.armed_group == 1) %>%
  recode_to(to = 7, where = crime_recourse_hh.none == 1 | crime_recourse_hh.no_report == 1) %>%
  #8.1 injuries to children
  new_recoding(target = child_injury_score, source = child_injury) %>%
  recode_to(to = 1, where.selected.exactly = "no") %>%
  recode_to(to = 5, where.selected.exactly = "yes") %>%
  #8.2 cfs 
  new_recoding(target = cfs_score) %>%
  recode_to(to = 1, where = child_space == "yes" & child_prot_service == "yes" & child_prot_service_satisfaction == "yes") %>%
  recode_to(to = 4, where = child_space == "yes" & child_prot_service == "yes" & child_prot_service_satisfaction == "no") %>%
  recode_to(to = 5, where = child_space == "yes" & child_prot_service == "no") %>%
  recode_to(to = 6, where = child_space == "no" & child_prot_service == "yes" & child_prot_service_satisfaction == "no") %>%
  recode_to(to = 7, where = child_space == "no" & child_prot_service == "no") %>%
  recode_to(to = 7, where = child_space == "no" & child_prot_service == "no" & child_prot_service_satisfaction == "no") %>%
  #9.1 exploitation
  new_recoding(target = exploitation_score) %>%
  recode_to(to = 1, where = exploit_hum_fee == "no" & exploit_hum_favour == "no") %>%
  recode_to(to = 4, where = exploit_hum_fee == "yes" & exploit_hum_favour == "no") %>%
  recode_to(to = 5, where = exploit_hum_fee == "no" & exploit_hum_favour == "yes") %>%
  recode_to(to = 6, where = exploit_hum_fee == "yes" & exploit_hum_favour == "yes") %>%
  #10.1 representation of women
  new_recoding(target = women_committee_score, source = women_committee) %>%
  recode_to(to = 1, where.selected.exactly = "yes") %>%
  recode_to(to = 3, where.selected.exactly = "no") %>%
  #11.1 relation between host/idp
  new_recoding(target = relation_hc_idp_score, source = idp_hc_relations) %>%
  recode_to(to = 1, where.selected.exactly = "v_good") %>%
  recode_to(to = 2, where.selected.exactly = "good") %>%
  recode_to(to = 3, where.selected.exactly = "bad") %>%
  recode_to(to = 4, where.selected.exactly = "v_bad") %>%
  end_recoding()
