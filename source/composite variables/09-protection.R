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
  recode_to(to = 1, where = separation_members == "no") %>%
  recode_to(to = 4, where.selected.any = c("m_a_18", "f_a_18"), source = separation_age_gender) %>%
  recode_to(to = 5, where.selected.any = c("m_u_18", "f_u_18"), source = separation_age_gender) %>%
  recode_to(to = 6, where = (separation_age_gender.m_a_18 == 1 | separation_age_gender.f_a_18 == 1) &
                            (separation_age_gender.m_u_18 == 1 | separation_age_gender.f_u_18 == 1)) %>%
  recode_to(to = 2, where.selected.any = c("left_study", "left_work"), source = separation_reasons) %>%
  #3.1 safety and security concern
  new_recoding(target = safety_security_concern_score) %>%
  recode_to(to = 1, where = sgbv == "never" & grave_injury == "never" & abductions == "never" & 
              uxo == "never" & death == "never" & theft_harassment == "never" & light_injury == "never") %>%
  recode_to(to = 3, where = theft_harassment == "sometimes") %>%
  recode_to(to = 4, where = light_injury == "sometimes") %>%
  recode_to(to = 5, where = theft_harassment == "always" | light_injury == "always") %>%
  recode_to(to = 6, where = sgbv == "sometimes" | grave_injury == "sometimes") %>%
  recode_to(to = 7, where = uxo == "sometimes" | abductions == "sometimes" | death == "sometimes") %>%
  recode_to(to = 8, where = sgbv == "always" | grave_injury == "always" | abductions == "always" | 
              uxo == "always" | death == "always") %>%
  #4.1 hazardous or exploitive work 
  new_recoding(target = hazardous_work_score) %>%
  recode_to(to = 1, where = hazardous_work == "no") %>%
  recode_to(to = 4, where.selected.any = c("m_a_18", "f_a_18"), source = hazardous_work_age_gender) %>%
  recode_to(to = 5, where.selected.any = c("m_u_18", "f_u_18"), source = hazardous_work_age_gender) %>%
  recode_to(to = 6, where = (hazardous_work_age_gender.m_a_18 == 1 | hazardous_work_age_gender.f_a_18 == 1) &
              (hazardous_work_age_gender.m_u_18 == 1 | hazardous_work_age_gender.f_u_18 == 1)) %>%
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
  new_recoding(target = sgbv_referral_sum) %>%
  recode_directly(to_expression = 
                    (sgbv_recourse_awareness == "yes") * 2 +
                    (sgbv_recourse_awareness == "dontknow") * 1 +
                    (sgbv_recourse_use == "yes") * 2 +
                    (sgbv_recourse_use == "dontknow") * 1 +
                    (sgbv_recourse_use_satisfaction == "yes") * 2 +
                    (sgbv_recourse_use_satisfaction == "dontknow") * 1) %>%
  new_recoding(target = sgbv_referral_score, source = sgbv_referral_sum) %>%
  recode_to(to = 1, where.num.equal = 6) %>%
  recode_to(to = 2, where.num.smaller.equal = 5) %>%
  recode_to(to = 3, where.num.smaller.equal = 4) %>%
  recode_to(to = 4, where.num.smaller.equal = 3) %>%
  recode_to(to = 5, where.num.smaller.equal = 2) %>%
  recode_to(to = 6, where.num.smaller.equal = 1) %>%
  recode_to(to = 7, where.num.equal = 0) %>%
  #6.2 GBV recourse to justice
  new_recoding(target = sgbv_justice_recourse_score) %>%
  recode_to(to = 1, where = sgbv_recourse_women.police == 1) %>%
  recode_to(to = 2, where = sgbv_recourse_women.comm_elders == 1 | sgbv_recourse_women.comm_leader == 1) %>%
  recode_to(to = 3, where = sgbv_recourse_women.un_ngo == 1 | sgbv_recourse_women.health_centre == 1) %>%
  recode_to(to = 5, where = sgbv_recourse_women.armed_group == 1) %>%
  recode_to(to = 7, where = sgbv_recourse_women.none == 1 | sgbv_recourse_women.no_report == 1) %>%
  #7.1 access to judicial remedy
  new_recoding(target = access_judicial_remedy_sum) %>%
  recode_directly(to_expression = 
                    (judicial_remedy == "yes") * 2 +
                    (judicial_remedy == "dontknow") * 1 +
                    (judicial_remedy_effective == "yes") * 2 +
                    (judicial_remedy_effective == "dontknow") * 1)%>%
  new_recoding(target = access_judicial_remedy_score, source = access_judicial_remedy_sum) %>%
  recode_to(to = 1, where.num.equal = 4) %>%
  recode_to(to = 3, where.num.smaller.equal = 3) %>%
  recode_to(to = 4, where.num.smaller.equal = 2) %>%
  recode_to(to = 5, where.num.smaller.equal = 1) %>%
  recode_to(to = 7, where.num.equal = 0) %>%
  #7.2 recourse to justice
  new_recoding(target = justice_recourse_score) %>%
  recode_to(to = 1, where = crime_recourse_hh.police == 1) %>%
  recode_to(to = 2, where = crime_recourse_hh.comm_elders == 1 | crime_recourse_hh.comm_leader == 1) %>%
  recode_to(to = 3, where = crime_recourse_hh.un_ngo == 1 | crime_recourse_hh.health_centre == 1) %>%
  recode_to(to = 5, where = crime_recourse_hh.armed_group == 1) %>%
  recode_to(to = 7, where = crime_recourse_hh.none == 1 | crime_recourse_hh.no_report == 1) %>%
  #8.1 injuries to children
  new_recoding(target = child_injury_score, source = child_injury) %>%
  recode_to(to = 1, where = child_injury == "no") %>%
  recode_to(to = 5, where = child_injury == "yes") %>%
  recode_to(to = 7, where = child_injury == "yes" & child_health_access == "no") %>%
  #8.2 cfs 
  new_recoding(target = cfs_sum) %>%
  recode_directly(to_expression = 
                    (child_space == "yes") * 2 +
                    (child_space == "dontknow") * 1 +
                    (child_prot_service == "yes") * 2 +
                    (child_prot_service == "dontknow") * 1 +
                    (child_prot_service_satisfaction == "yes") * 2 +
                    (child_prot_service_satisfaction == "dontknow") * 1) %>%
  new_recoding(target = cfs_score, source = cfs_sum) %>%
  recode_to(to = 1, where.num.equal = 6) %>%
  recode_to(to = 2, where.num.smaller.equal = 5) %>%
  recode_to(to = 3, where.num.smaller.equal = 4) %>%
  recode_to(to = 4, where.num.smaller.equal = 3) %>%
  recode_to(to = 5, where.num.smaller.equal = 2) %>%
  recode_to(to = 6, where.num.smaller.equal = 1) %>%
  recode_to(to = 7, where.num.equal = 0) %>%
  #9.1 exploitation
  new_recoding(target = exploitation_sum) %>%
  recode_directly(to_expression = 
                    (exploit_hum_fee == "yes") * 2 +
                    (exploit_hum_fee == "dontknow") * 1 +
                    (exploit_hum_favour == "yes") * 2 +
                    (exploit_hum_favour == "dontknow") * 1)%>%
  new_recoding(target = exploitation_score) %>%
  recode_to(to = 1, where.num.equal = 4, source = exploitation_sum) %>%
  recode_to(to = 3, where.num.smaller.equal = 3, source = exploitation_sum) %>%
  recode_to(to = 4, where.num.smaller.equal = 2, source = exploitation_sum) %>%
  recode_to(to = 5, where.num.smaller.equal = 1, source = exploitation_sum) %>%
  recode_to(to = 7, where.num.equal = 0, source = exploitation_sum) %>%
  recode_to(to = 8, where = coping_water.exploit_hum == 1 | 
              coping_sanitation.exploit_hum == 1 |
              coping_hygiene.exploit_hum == 1 | 
              coping_food.exploit_hum == 1 | 
              coping_shelter.exploit_hum == 1 |
              coping_nfi.exploit_hum == 1 |
              coping_education.exploit_hum == 1 |
              coping_health.exploit_hum == 1 |
              coping_general.exploit_hum == 1) %>%
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

