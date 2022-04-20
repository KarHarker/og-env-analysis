source('packages.R')
conflict_prefer("filter", "dplyr")

tar_load(c(priority_og_du9, priority_og_pa_du9, du9omp_area, priority_og_des_du9, pa_du9,
           priority_og_des_vis_du9, og_du9, og_pa_du9, og_des_lands_du9, og_des_lands_vis_du9,
           du9_ch, du9_ch_pa, des_lands_du9,
           du9_ch_des_vis, du9, des_lands_du9_vis))

#priority og in area
du9_priority_og <- priority_og_du9 %>%
  mutate(og_area = st_area(.),
         og_area = as.numeric(set_units(og_area, ha))) %>%
  st_drop_geometry() %>%
  group_by(herd, critical_habitat_type, priority_zone_type) %>%
  summarise(og_priority_area = sum(og_area))

#priority og in protected area
du9_priority_og_pa <- priority_og_pa_du9 %>%
  mutate(og_pa_area = st_area(.),
         og_pa_area = as.numeric(set_units(og_pa_area, ha))) %>%
  st_drop_geometry() %>%
  group_by(herd, critical_habitat_type, priority_zone_type) %>%
  summarise(og_priority_pa_area = sum(og_pa_area))

# #priority og in uwr area
# du9_priority_og_uwr<- priority_og_des_vis_du9 %>%
#   mutate(og_des_area = st_area(.),
#          og_des_area = as.numeric(set_units(og_des_area, ha))) %>%
#   st_drop_geometry() %>%
#   filter(designation == "uwr_conditional_harvest"|designation == "uwr_no_harvest") %>%
#   summarise(og_priority_uwr_area = sum(og_des_area))

#priority og is all des area
du9_priority_og_des<- priority_og_des_du9 %>%
  mutate(og_des_area = st_area(.),
         og_des_area = as.numeric(set_units(og_des_area, ha))) %>%
  st_drop_geometry() %>%
  group_by(herd, critical_habitat_type, priority_zone_type) %>%
  summarise(og_priority_des_area = sum(og_des_area))

#all og sums
du9_all_og <- og_du9 %>%
  mutate(og_area = st_area(.),
         og_area = as.numeric(set_units(og_area, ha))) %>%
  st_drop_geometry() %>%
  group_by(herd, critical_habitat_type, priority_zone_type) %>%
  summarise(all_og_area = sum(og_area))

du9_all_og_pa <- og_pa_du9 %>%
  mutate(og_pa_area = st_area(.),
         og_pa_area = as.numeric(set_units(og_pa_area, ha))) %>%
  st_drop_geometry() %>%
  group_by(herd, critical_habitat_type, priority_zone_type) %>%
  summarise(all_og_pa_area = sum(og_pa_area))

# du9_all_og_uwr <- og_des_lands_vis_du9 %>%
#   mutate(og_uwr_area = st_area(.),
#          og_uwr_area = as.numeric(set_units(og_uwr_area, ha))) %>%
#   st_drop_geometry() %>%
#   filter(designation == "uwr_conditional_harvest"|designation == "uwr_no_harvest") %>%
#   summarise(all_uwr_area = sum(og_uwr_area))

du9_all_og_des <- og_des_lands_du9 %>%
  mutate(og_des_area = st_area(.),
         og_des_area = as.numeric(set_units(og_des_area, ha))) %>%
  st_drop_geometry() %>%
  group_by(herd, critical_habitat_type, priority_zone_type) %>%
  summarise(all_og_des_area = sum(og_des_area))

#du9 all area
du9_area <- du9 %>%
  mutate(caribou_area = st_area(.),
         caribou_area = as.numeric(set_units(caribou_area, ha))) %>%
  st_drop_geometry() %>%
  group_by(herd, critical_habitat_type, priority_zone_type) %>%
  summarise(du9_area = sum(caribou_area))

du9_pa <- pa_du9 %>%
  mutate(caribou_pa_area = st_area(.),
         caribou_pa_area = as.numeric(set_units(caribou_pa_area, ha))) %>%
  st_drop_geometry() %>%
  group_by(pa_type, herd, critical_habitat_type, priority_zone_type) %>%
  summarise(caribou_pa_area = sum(caribou_pa_area)) %>%
  pivot_wider(names_from=pa_type, values_from=caribou_pa_area)

# du9_uwr <- des_lands_du9_vis %>%
#   mutate(proposed_cons_area= st_area(.),
#          proposed_cons_area = as.numeric(set_units(proposed_cons_area, ha))) %>%
#   st_drop_geometry() %>%
#   group_by(designation) %>%
#   summarise(des_area_type = sum(proposed_cons_area)) %>%
#   filter(designation == "uwr_conditional_harvest"|designation == "uwr_no_harvest")%>%
#   pivot_wider(names_from=designation, values_from=des_area_type)
# #write_sf(des_lands_du9_vis, "out/des_lands_du9_vis.gpkg")

du9_des <- des_lands_du9 %>%
  mutate(caribou_area_des = st_area(.),
         caribou_area_des = as.numeric(set_units(caribou_area_des, ha))) %>%
  st_drop_geometry() %>%
  group_by(herd, critical_habitat_type, priority_zone_type) %>%
  summarise(total_area_des = sum(caribou_area_des))

du9_sums <- du9_area %>%
  left_join(du9_pa, by = c('herd', 'critical_habitat_type', 'priority_zone_type')) %>%
  #left_join(du9_uwr, by = c('herd', 'critical_habitat_type', 'priority_zone_type')) %>%
  left_join(du9_des, by = c('herd', 'critical_habitat_type', 'priority_zone_type')) %>%
  left_join(du9_all_og, by = c('herd', 'critical_habitat_type', 'priority_zone_type')) %>%
  left_join(du9_all_og_pa, by = c('herd', 'critical_habitat_type', 'priority_zone_type')) %>%
  left_join(du9_all_og_des, by = c('herd', 'critical_habitat_type', 'priority_zone_type')) %>%
  left_join(du9_priority_og, by = c('herd', 'critical_habitat_type', 'priority_zone_type')) %>%
  left_join(du9_priority_og_pa, by = c('herd', 'critical_habitat_type', 'priority_zone_type')) %>%
  left_join(du9_priority_og_des, by = c('herd', 'critical_habitat_type', 'priority_zone_type'))

#area sums
du9_sum <- du9_sums %>%
  mutate(area_no_conservation = du9_area - total_area_des,
         area_outside_parks = du9_area - ppa)

du9_des <- des_lands_du9 %>%
  mutate(caribou_area_des = st_area(.),
         caribou_area_des = as.numeric(set_units(caribou_area_des, ha))) %>%
  summarise(total_area_des = sum(caribou_area_des))
write_sf(du9_des, "out/des_lands_du9_all.gpkg")

des_info_flat <- read_csv("designations-unique-flat.csv") %>%
  rename(designations = designation)

#Table 2 - summarize the type and area of conservation area (individually)
du9_des_summary_flat <- des_lands_du9 %>%
  mutate(proposed_cons_area= st_area(.),
         proposed_cons_area = as.numeric(set_units(proposed_cons_area, ha))) %>%
  st_drop_geometry() %>%
  group_by(designations, herd, critical_habitat_type, priority_zone_type, forest_restriction_max,
           mine_restriction_max, og_restriction_max) %>%
  summarise(des_area_type = sum(proposed_cons_area)) %>%
  ungroup() %>%
  group_by(herd, critical_habitat_type) %>%
  mutate(des_area_hab_type = sum(des_area_type)) %>%
  ungroup() %>%
  group_by(herd) %>%
  mutate(des_area_herd = sum(des_area_type)) %>%
  arrange(designations, herd, desc(des_area_type)) %>%
  left_join(des_info_flat, by = "designations")
#write_sf(des_lands_du9_vis, "out/des_lands_du9_vis.gpkg")
write_csv(du9_des_summary_flat, "out/des_lands_flat_du9.csv")

#Table 2 - summarize the type and area of conservation area (with overlaps)
du9_des_summary_overlapping <- des_lands_du9_vis %>%
  mutate(proposed_cons_area= st_area(.),
         proposed_cons_area = as.numeric(set_units(proposed_cons_area, ha))) %>%
  st_drop_geometry() %>%
  group_by(designation, herd, critical_habitat_type, priority_zone_type) %>%
  summarise(des_area_type = sum(proposed_cons_area)) %>%
  ungroup() %>%
  group_by(herd, critical_habitat_type) %>%
  mutate(des_area_hab_type = sum(des_area_type)) %>%
  ungroup() %>%
  group_by(herd) %>%
  mutate(des_area_herd = sum(des_area_type)) %>%
  arrange(designation, herd, desc(des_area_type))
write_csv(du9_des_summary_overlapping, "out/des_lands_overlapping_du9.csv")


du9_og <- du9_area %>%
  left_join(du9_pa, by = c('herd', 'critical_habitat_type', 'priority_zone_type')) %>%
  #left_join(du9_uwr, by = c('herd', 'critical_habitat_type', 'priority_zone_type')) %>%
  left_join(du9_des, by = c('herd', 'critical_habitat_type', 'priority_zone_type')) %>%
  left_join(du9_all_og, by = c('herd', 'critical_habitat_type', 'priority_zone_type')) %>%
  left_join(du9_all_og_pa, by = c('herd', 'critical_habitat_type', 'priority_zone_type')) %>%
  left_join(du9_all_og_des, by = c('herd', 'critical_habitat_type', 'priority_zone_type')) %>%
  left_join(du9_priority_og, by = c('herd', 'critical_habitat_type', 'priority_zone_type')) %>%
  left_join(du9_priority_og_pa, by = c('herd', 'critical_habitat_type', 'priority_zone_type')) %>%
  left_join(du9_priority_og_des, by = c('herd', 'critical_habitat_type', 'priority_zone_type'))

du9_summary_og <- du9_og %>%
  mutate(priority_unprotected_og = og_priority_area - og_priority_pa_area - (og_priority_des_area-og_priority_pa_area),
         all_unprotected_og = all_og_area-all_og_pa_area-(all_og_des_area-all_og_pa_area),
         perc_priority_unprot = round(priority_unprotected_og/og_priority_area*100, digits=0),
         perc_all_unprot = round(all_unprotected_og/all_og_area*100, digits=1))
#unprot_area = caribou_area - ppa - oecm,
#priority_no_des = og_priority_area - og_des_area,
#non_des_area = caribou_area - ppa - oecm -caribou_area_des,
#perc_unprot = non_des_area/caribou_area*100)

order = c("High", "Medium", "Low", "Other")

du9_summary_og_output <- du9_summary_og %>%
  select(-c(oecm, ppa, total_area_des, all_og_des_area, og_priority_des_area)) %>%
  relocate(all_unprotected_og, .after=all_og_pa_area) %>%
  relocate(perc_all_unprot,  .after=all_unprotected_og) %>%
  mutate(across(where(is.numeric), round, 0),
    priority_zone_type = factor(priority_zone_type, levels=order)) %>%
  arrange(herd, critical_habitat_type, priority_zone_type) %>%
  select(-c(all_og_pa_area, og_priority_pa_area)) %>%
  mutate(across(where(is.numeric), ~replace_na(.x, 0)))

write_csv(du9_summary_og_output, "out/old-growth-summary_du9.csv")

########################################################################################################


pa_du9 <- pa_du9 %>%
  mutate(pa_area= st_area(.),
         pa_area = as.numeric(set_units(pa_area, ha))) %>%
  group_by(pa_type) %>%
  summarise(area_type = sum(pa_area))
write_sf(pa_du9, "out/pa_du9.gpkg")

ppa <- pa_du9 %>%
  filter(pa_type == "ppa")
write_sf(ppa, "out/ppa_du9.gpkg")

oecm <- pa_du9 %>%
  filter(pa_type == "oecm")
write_sf(oecm, "out/oecm_du9.gpkg")

tar_load(c(du9_ch, du9_ch_des))

ch <-du9_ch %>%
  mutate(ch_area = st_area(.),
         ch_area = as.numeric(set_units(ch_area, ha))) %>%
  group_by(common_name_english) %>%
  summarise(ch_du9ca = sum(ch_area))

write_sf(ch, "out/du9_ch.gpkg")

ch <- ch %>%
  st_drop_geometry()

ch_des <-du9_ch_des %>%
  mutate(ch_pa_area = st_area(.),
         ch_pa_area = as.numeric(set_units(ch_pa_area, ha))) %>%
  group_by(common_name_english) %>%
  summarise(ch_du9ca_pa = sum(ch_pa_area))

write_sf(ch_des, "out/ch_des.gpkg")

ch_des <- ch_des %>%
  st_drop_geometry()

ch_comb <- ch %>%
  left_join(ch_des) %>%
  mutate(perc_cons = ch_du9ca_pa/ch_du9ca*100)

write_csv(ch_comb, "du9_ch.csv")

ch_overall <- ch %>%
  left_join(ch_pa, by='common_name_english') %>%
  left_join(ch_des, by='common_name_english') %>%
  mutate(unprot = ch_du9ca - ch_du9ca_pa - ch_du9ca_uwr)

write_csv(ch_overall, "out/ch_du9_overall.csv")


caribou_ch <-du9_ch %>%
  mutate(ch_area = st_area(.),
         ch_area = as.numeric(set_units(ch_area, ha))) %>%
  group_by(common_name_english) %>%
  summarise(sum_ch_area = sum(ch_area)) %>%
  filter(common_name_english == "Caribou - Southern Mountain population")
write_sf(caribou_ch, "out/caribou_ch.gpkg")

whitebark_pine <-du9_ch %>%
  mutate(ch_area = st_area(.),
         ch_area = as.numeric(set_units(ch_area, ha))) %>%
  group_by(common_name_english) %>%
  summarise(sum_ch_area = sum(ch_area)) %>%
  filter(common_name_english == "Whitebark Pine")

write_sf(whitebark_pine, "out/whitebark_pine.gpkg")

ch_des_sf <-du9_ch_des_vis %>%
  mutate(ch_uwr_area = st_area(.),
         ch_uwr_area = as.numeric(set_units(ch_uwr_area, ha))) %>%
  group_by(designation, common_name_english) %>%
  summarise(ch_uwr_area = sum(ch_uwr_area)) %>%
  filter(designation == "uwr_conditional_harvest"|designation == "uwr_no_harvest")


caribou_area <- c_area %>%
  mutate(ch_area = st_area(.),
         ch_area = as.numeric(set_units(ch_area, km^2))) %>%
  st_drop_geometry() %>%
  group_by(herd) %>%
  summarise(sum_caribou_area = sum(ch_area))

caribou_area_pa <-du9_caribou_pa %>%
  mutate(pa_area = st_area(.),
         pa_area = as.numeric(set_units(pa_area, km^2))) %>%
  st_drop_geometry() %>%
  group_by(herd) %>%
  summarise(sum_pa_area = sum(pa_area))

caribou_area_pa_des <- du9_caribou_pa_des %>%
  mutate(pa_uwr_area = st_area(.),
         pa_uwr_area = as.numeric(set_units(pa_uwr_area, ha))) %>%
  st_drop_geometry() %>%
  filter(designation == "uwr_conditional_harvest"|designation == "uwr_no_harvest") %>%
  group_by(herd) %>%
  summarise(sum_pa_uwr_area = sum(pa_uwr_area))

sum_caribou_area <- caribou_area %>%
  left_join(caribou_area_pa) %>%
  left_join(caribou_area_pa_des) %>%
  mutate()


tar_load(c(du9_forest, du9_tenures, du9_harv, du9_mine_potential, du9_land))

du9_cutblocks <- du9_forest %>%
  mutate(for_area = st_area(.),
         for_area = as.numeric(set_units(for_area, ha))) %>%
  st_drop_geometry() %>%
  group_by(LIFE_CYCLE_STATUS_CODE) %>%
  summarise(for_area_sum = sum(for_area),
            n_cutblock_total = n())

du9_cutblock_licensees <- du9_forest %>%
  mutate(for_area = st_area(.),
         for_area = as.numeric(set_units(for_area, ha))) %>%
  st_drop_geometry() %>%
  group_by(CLIENT_NAME, LIFE_CYCLE_STATUS_CODE) %>%
  summarise(for_licensee_sum = sum(for_area),
            n_cutblocks = n()) %>%
  left_join(du9_cutblocks) %>%
  mutate(perc_of_total = for_licensee_sum/for_area_sum*100)

write_csv(du9_cutblock_licensees, "out/du9_licensee_list.csv")


du9_tenure_sum <- du9_tenures %>%
  mutate(for_area = st_area(.),
         for_area = as.numeric(set_units(for_area, ha))) %>%
  st_drop_geometry() %>%
  group_by(CLIENT_NAME, LIFE_CYCLE_STATUS_CODE) %>%
  summarise(for_area_sum = sum(for_area),
            n_cutblocks = n())

write_csv(du9_tenure_sum, "out/du9_managed_license_list.csv")

du9_harvest_sum <- du9_harv %>%
  mutate(for_area = st_area(.),
         for_area = as.numeric(set_units(for_area, ha))) %>%
  st_drop_geometry() %>%
  filter(LIFE_CYCLE_STATUS_CODE != "RETIRED") %>%
  group_by(CLIENT_NAME, LIFE_CYCLE_STATUS_CODE) %>%
  summarise(harv_area_sum = sum(for_area),
            n_cutblocks = n()) %>%
  left_join(du9_cutblocks) %>%
  mutate(perc_of_total = harv_area_sum/for_area_sum*100)

write_csv(du9_harvest_sum, "out/du9_harv_auth_list.csv")


du9_land_sum <- du9_land %>%
  mutate(land_area = st_area(.),
         land_area = as.numeric(set_units(land_area, ha))) %>%
  st_drop_geometry() %>%
  group_by(TENURE_STATUS, TENURE_TYPE, TENURE_PURPOSE, TENURE_SUBPURPOSE) %>%
  summarise(land_area_sum = sum(land_area),
            n_tenure_total = n()) %>%
  ungroup() %>%
  mutate(n_tenure_overall=sum(n_tenure_total))

write_csv(du9_land_sum, "out/du9_land_tenure.csv")


du9_mineral <- du9_mine_potential %>%
  mutate(mine_area = st_area(.),
         mine_area = as.numeric(set_units(mine_area, ha))) %>%
  st_drop_geometry() %>%
  group_by(OWNER_NAME) %>%
  summarise(mine_area_sum = sum(mine_area),
            n_claim = n()) %>%
  ungroup() %>%
  mutate(overall_area = sum(mine_area_sum),
         n_claims_overall=sum(n_claim))

write_csv(du9_mineral, "out/du9_mineral_claims.csv")


# PIP areas

tar_load(c(pip_du9))

du9_pip_summary <- pip_du9 %>%
  mutate(pip_area = st_area(.),
         pip_area = as.numeric(set_units(pip_area, ha))) %>%
  st_drop_geometry() %>%
  group_by(CONTACT_ORGANIZATION_NAME, CNSLTN_AREA_NAME) %>%
  summarise(area = sum(pip_area))
