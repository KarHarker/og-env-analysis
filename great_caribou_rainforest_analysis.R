tar_load(c(priority_og_smc, priority_og_pa_smc, caribou_cons_area, priority_og_des, pa_smc,
           priority_og_des_vis, og_smc, og_pa_smc, og_des_lands, og_des_lands_vis, smc_ch, smc_ch_pa, des_lands_smc,
           smc_ch_des_vis, smc_caribou_pa, smc_caribou_pa_des, c_area, des_lands_smc_vis))

#priority og in area
smc_priority_og <- priority_og_smc %>%
  mutate(og_area = as.numeric(st_area(.)),
         og_area = as.numeric(set_units(og_area, ha))) %>%
  st_drop_geometry() %>%
  summarise(og_priority_area = sum(og_area)/10000)

#priority og in protected area
smc_priority_og_pa <- priority_og_pa_smc %>%
  mutate(og_pa_area = as.numeric(st_area(.)),
         og_pa_area = as.numeric(set_units(og_pa_area, ha))) %>%
  st_drop_geometry() %>%
  summarise(og_priority_pa_area = sum(og_pa_area)/10000)

#priority og in uwr area
smc_priority_og_uwr<- priority_og_des_vis %>%
  mutate(og_des_area = as.numeric(st_area(.)),
         og_des_area = as.numeric(set_units(og_des_area, ha))) %>%
  st_drop_geometry() %>%
  filter(designation == "uwr_conditional_harvest"|designation == "uwr_no_harvest") %>%
  summarise(og_priority_uwr_area = sum(og_des_area)/10000)

#priority og is all des area
smc_priority_og_des<- priority_og_des %>%
  mutate(og_des_area = as.numeric(st_area(.)),
         og_des_area = as.numeric(set_units(og_des_area, ha))) %>%
  st_drop_geometry() %>%
  summarise(og_priority_des_area = sum(og_des_area)/10000)

#all og sums
smc_all_og <- og_smc %>%
  mutate(og_area = as.numeric(st_area(.)),
         og_area = as.numeric(set_units(og_area, ha))) %>%
  st_drop_geometry() %>%
  summarise(all_og_area = sum(og_area)/10000)

smc_all_og_pa <- og_pa_smc %>%
  mutate(og_pa_area = as.numeric(st_area(.)),
         og_pa_area = as.numeric(set_units(og_pa_area, ha))) %>%
  st_drop_geometry() %>%
  summarise(all_og_pa_area = sum(og_pa_area)/10000)

smc_all_og_uwr <- og_des_lands_vis %>%
  mutate(og_uwr_area = as.numeric(st_area(.)),
         og_uwr_area = as.numeric(set_units(og_uwr_area, ha))) %>%
  st_drop_geometry() %>%
  filter(designation == "uwr_conditional_harvest"|designation == "uwr_no_harvest") %>%
  summarise(all_uwr_area = sum(og_uwr_area)/10000)

smc_all_og_des <- og_des_lands %>%
  mutate(og_des_area = as.numeric(st_area(.)),
         og_des_area = as.numeric(set_units(og_des_area, ha))) %>%
  st_drop_geometry() %>%
  summarise(all_des_area = sum(og_des_area)/10000)

#smc all area
smc_area <- caribou_cons_area %>%
  mutate(caribou_area = as.numeric(st_area(.)),
         caribou_area = as.numeric(set_units(caribou_area, ha))) %>%
  st_drop_geometry() %>%
  summarise(caribou_area = sum(caribou_area)/10000)

smc_pa <- pa_smc %>%
  mutate(caribou_pa_area = as.numeric(st_area(.)),
         caribou_pa_area = as.numeric(set_units(caribou_pa_area, ha))) %>%
  st_drop_geometry() %>%
  group_by(pa_type) %>%
  summarise(caribou_pa_area = sum(caribou_pa_area)/10000) %>%
  pivot_wider(names_from=pa_type, values_from=caribou_pa_area)

smc_uwr <- des_lands_smc_vis %>%
  mutate(proposed_cons_area= as.numeric(st_area(.)),
         proposed_cons_area = as.numeric(set_units(proposed_cons_area, ha))) %>%
  st_drop_geometry() %>%
  group_by(designation) %>%
  summarise(des_area_type = sum(proposed_cons_area)/10000) %>%
  filter(designation == "uwr_conditional_harvest"|designation == "uwr_no_harvest")%>%
  pivot_wider(names_from=designation, values_from=des_area_type)
#write_sf(des_lands_smc_vis, "out/des_lands_smc_vis.gpkg")

smc_des <- des_lands_smc %>%
  mutate(caribou_area_des = as.numeric(st_area(.)),
         caribou_area_des = as.numeric(set_units(caribou_area_des, ha))) %>%
  st_drop_geometry() %>%
  summarise(total_area_des = sum(caribou_area_des)/10000)

smc_sums <- cbind(smc_area, smc_pa, smc_uwr, smc_des, smc_all_og, smc_all_og_pa, smc_all_og_uwr, smc_all_og_des,
                  smc_priority_og, smc_priority_og_pa, smc_priority_og_uwr, smc_priority_og_des)

#area sums
smc_sum <- smc_sums %>%
  mutate(area_no_conservation = caribou_area - total_area_des,
         area_outside_parks = caribou_area - ppa - oecm,
         area_harmony = caribou_area - ppa - oecm - uwr_conditional_harvest - uwr_no_harvest,
         area_des = total_area_des - ppa - oecm - uwr_conditional_harvest - uwr_no_harvest,
         uwr_area = uwr_conditional_harvest + uwr_no_harvest)

smc_des <- des_lands_smc %>%
  mutate(caribou_area_des = as.numeric(st_area(.)),
         caribou_area_des = as.numeric(set_units(caribou_area_des, ha))) %>%
  summarise(total_area_des = sum(caribou_area_des)/10000)
write_sf(smc_des, "out/des_lands_smc_all.gpkg")

#Table 2 - summarize the type and area of conservation area (individually)
smc_des_summary_flat <- des_lands_smc %>%
  mutate(proposed_cons_area= as.numeric(st_area(.)),
         proposed_cons_area = as.numeric(set_units(proposed_cons_area, ha))) %>%
  st_drop_geometry() %>%
  group_by(designations) %>%
  summarise(des_area_type = sum(proposed_cons_area)/10000)
#write_sf(des_lands_smc_vis, "out/des_lands_smc_vis.gpkg")
write_csv(smc_des_summary_flat, "out/des_lands_flat_smc.csv")

#Table 2 - summarize the type and area of conservation area (with overlaps)
smc_des_summary_overlapping <- des_lands_smc_vis %>%
  mutate(proposed_cons_area= as.numeric(st_area(.)),
         proposed_cons_area = as.numeric(set_units(proposed_cons_area, ha))) %>%
  st_drop_geometry() %>%
  group_by(designation) %>%
  summarise(des_area_type = sum(proposed_cons_area)/10000)
write_csv(smc_des_summary_overlapping, "out/des_lands_overlapping_smc.csv")



smc_og<-cbind(smc_priority_og, smc_priority_og_pa, smc_priority_og_des, smc_priority_og_uwr, smc_all_og, smc_all_og_pa,
              smc_all_og_uwr, smc_all_og_uwr, caribou_area, caribou_area_pa, caribou_area_des)

smc_summary <- smc_og %>%
  mutate(priority_unprotected_og = og_priority_area - og_priority_pa_area - og_priority_uwr_area,
         all_unprotected_og = all_og_area-all_og_pa_area-all_uwr_area,
         perc_priority_unprot = priority_unprotected_og/og_priority_area*100,
         perc_all_unprot = all_unprotected_og/all_og_area*100,
         #unprot_area = caribou_area - ppa - oecm,
         #priority_no_des = og_priority_area - og_des_area,
         non_des_area = caribou_area - ppa - oecm -caribou_area_des,
         perc_unprot = non_des_area/caribou_area*100)

pa_smc <- pa_smc %>%
  mutate(pa_area= as.numeric(st_area(.)),
         pa_area = as.numeric(set_units(pa_area, ha))) %>%
  group_by(pa_type) %>%
  summarise(area_type = sum(pa_area))
write_sf(pa_smc, "out/pa_smc.gpkg")

ppa <- pa_smc %>%
  filter(pa_type == "ppa")
write_sf(ppa, "out/ppa_smc.gpkg")

oecm <- pa_smc %>%
  filter(pa_type == "oecm")
write_sf(oecm, "out/oecm_smc.gpkg")



ch <-smc_ch %>%
  mutate(ch_area = as.numeric(st_area(.)),
         ch_area = as.numeric(set_units(ch_area, ha))) %>%
  st_drop_geometry() %>%
  group_by(common_name_english) %>%
  summarise(ch_gcrca = sum(ch_area)/10000)

ch_pa <-smc_ch_pa %>%
  mutate(ch_pa_area = as.numeric(st_area(.)),
         ch_pa_area = as.numeric(set_units(ch_pa_area, ha))) %>%
  st_drop_geometry() %>%
  group_by(common_name_english) %>%
  summarise(ch_gcrca_pa = sum(ch_pa_area)/10000)

ch_des <-smc_ch_des_vis %>%
  mutate(ch_uwr_area = as.numeric(st_area(.)),
         ch_uwr_area = as.numeric(set_units(ch_uwr_area, ha))) %>%
  st_drop_geometry() %>%
  group_by(designation, common_name_english) %>%
  summarise(ch_uwr_area = sum(ch_uwr_area)) %>%
  filter(designation == "uwr_conditional_harvest"|designation == "uwr_no_harvest") %>%
  ungroup() %>%
  group_by(common_name_english) %>%
  summarise(ch_gcrca_uwr = sum(ch_uwr_area)/10000)

ch_overall <- ch %>%
  left_join(ch_pa, by='common_name_english') %>%
  left_join(ch_des, by='common_name_english') %>%
  mutate(unprot = ch_gcrca - ch_gcrca_pa - ch_gcrca_uwr)

write_csv(ch_overall, "out/ch_overall.csv")


caribou_ch <-smc_ch %>%
  mutate(ch_area = as.numeric(st_area(.)),
         ch_area = as.numeric(set_units(ch_area, ha))) %>%
  group_by(common_name_english) %>%
  summarise(sum_ch_area = sum(ch_area)) %>%
  filter(common_name_english == "Caribou - Southern Mountain population")
write_sf(caribou_ch, "out/caribou_ch.gpkg")

whitebark_pine <-smc_ch %>%
  mutate(ch_area = as.numeric(st_area(.)),
         ch_area = as.numeric(set_units(ch_area, ha))) %>%
  group_by(common_name_english) %>%
  summarise(sum_ch_area = sum(ch_area)) %>%
  filter(common_name_english == "Whitebark Pine")

write_sf(whitebark_pine, "out/whitebark_pine.gpkg")

ch_des_sf <-smc_ch_des_vis %>%
  mutate(ch_uwr_area = as.numeric(st_area(.)),
         ch_uwr_area = as.numeric(set_units(ch_uwr_area, ha))) %>%
  group_by(designation, common_name_english) %>%
  summarise(ch_uwr_area = sum(ch_uwr_area)) %>%
  filter(designation == "uwr_conditional_harvest"|designation == "uwr_no_harvest")


caribou_area <- c_area %>%
  mutate(ch_area = as.numeric(st_area(.)),
         ch_area = as.numeric(set_units(ch_area, km^2))) %>%
  st_drop_geometry() %>%
  group_by(herd) %>%
  summarise(sum_caribou_area = sum(ch_area))

caribou_area_pa <-smc_caribou_pa %>%
  mutate(pa_area = as.numeric(st_area(.)),
         pa_area = as.numeric(set_units(pa_area, km^2))) %>%
  st_drop_geometry() %>%
  group_by(herd) %>%
  summarise(sum_pa_area = sum(pa_area))

caribou_area_pa_des <- smc_caribou_pa_des %>%
  mutate(pa_uwr_area = as.numeric(st_area(.)),
         pa_uwr_area = as.numeric(set_units(pa_uwr_area, ha))) %>%
  st_drop_geometry() %>%
  filter(designation == "uwr_conditional_harvest"|designation == "uwr_no_harvest") %>%
  group_by(herd) %>%
  summarise(sum_pa_uwr_area = sum(pa_uwr_area))

sum_caribou_area <- caribou_area %>%
  left_join(caribou_area_pa) %>%
  left_join(caribou_area_pa_des) %>%
  mutate()


tar_load(c(smc_forest, smc_tenures, smc_harv, smc_mine_potential, smc_land))

smc_cutblocks <- smc_forest %>%
  mutate(for_area = as.numeric(st_area(.)),
         for_area = as.numeric(set_units(for_area, ha))) %>%
  st_drop_geometry() %>%
  group_by(LIFE_CYCLE_STATUS_CODE) %>%
  summarise(for_area_sum = sum(for_area)/10000,
            n_cutblock_total = n())

smc_cutblock_licensees <- smc_forest %>%
  mutate(for_area = as.numeric(st_area(.)),
         for_area = as.numeric(set_units(for_area, ha))) %>%
  st_drop_geometry() %>%
  filter(LIFE_CYCLE_STATUS_CODE != "RETIRED") %>%
  group_by(CLIENT_NAME, LIFE_CYCLE_STATUS_CODE) %>%
  summarise(for_licensee_sum = sum(for_area)/10000,
            n_cutblocks = n()) %>%
  left_join(smc_cutblocks) %>%
  mutate(perc_of_total = for_licensee_sum/for_area_sum*100)

write_csv(smc_cutblock_licensees, "out/grc_licensee_list.csv")


smc_tenure_sum <- smc_tenures %>%
  mutate(for_area = as.numeric(st_area(.)),
         for_area = as.numeric(set_units(for_area, ha))) %>%
  st_drop_geometry() %>%
  group_by(CLIENT_NAME, LIFE_CYCLE_STATUS_CODE) %>%
  summarise(for_area_sum = sum(for_area)/10000,
            n_cutblocks = n())

write_csv(smc_tenure_sum, "out/grc_managed_license_list.csv")

smc_harvest_sum <- smc_harv %>%
  mutate(for_area = as.numeric(st_area(.)),
         for_area = as.numeric(set_units(for_area, ha))) %>%
  st_drop_geometry() %>%
  filter(LIFE_CYCLE_STATUS_CODE != "RETIRED") %>%
  group_by(CLIENT_NAME, LIFE_CYCLE_STATUS_CODE) %>%
  summarise(harv_area_sum = sum(for_area)/10000,
            n_cutblocks = n()) %>%
  left_join(smc_cutblocks) %>%
  mutate(perc_of_total = harv_area_sum/for_area_sum*100)

write_csv(smc_harvest_sum, "out/grc_harv_auth_list.csv")


smc_land_sum <- smc_land %>%
  mutate(land_area = as.numeric(st_area(.)),
         land_area = as.numeric(set_units(land_area, ha))) %>%
  st_drop_geometry() %>%
  group_by(TENURE_STATUS, TENURE_TYPE, TENURE_PURPOSE, TENURE_SUBPURPOSE) %>%
  summarise(land_area_sum = sum(land_area)/10000,
            n_tenure_total = n()) %>%
  ungroup() %>%
  mutate(n_tenure_overall=sum(n_tenure_total))

write_csv(smc_land_sum, "out/grc_land_tenure.csv")


smc_mineral <- smc_mine_potential %>%
  mutate(mine_area = as.numeric(st_area(.)),
         mine_area = as.numeric(set_units(mine_area, ha))) %>%
  st_drop_geometry() %>%
  group_by(OWNER_NAME) %>%
  summarise(mine_area_sum = sum(mine_area)/10000,
            n_claim = n()) %>%
  ungroup() %>%
  mutate(overall_area = sum(mine_area_sum),
         n_claims_overall=sum(n_claim))

write_csv(smc_mineral, "out/mineral_claims.csv")

# CARIBOU FOOTPRINT AREA ANALYSIS

tar_load(c(caribou_full_herd, caribou_herd_pa, caribou_gcr_herd, smc_caribou_pa,
           smc_caribou_pa_des, smc_caribou_pa_uwr, caribou_herd_pa))

herd_full <- caribou_full_herd %>%
  mutate(herd_area = as.numeric(st_area(.)),
         herd_area = as.numeric(set_units(herd_area, ha))) %>%
  st_drop_geometry() %>%
  filter(priority_zone_type == "High" | priority_zone_type == "Medium" | priority_zone_type == "Low") %>%
  group_by(herd) %>%
  summarise(herd_area_sum = sum(herd_area)/10000)

herd_full_by_zone <- caribou_full_herd %>%
  mutate(herd_area = as.numeric(st_area(.)),
         herd_area = as.numeric(set_units(herd_area, ha))) %>%
  st_drop_geometry() %>%
  filter(priority_zone_type == "High" | priority_zone_type == "Medium" | priority_zone_type == "Low") %>%
  group_by(herd, priority_zone_type) %>%
  summarise(herd_area_zone_type = sum(herd_area)/10000) %>%
  pivot_wider(names_from = priority_zone_type, values_from= herd_area_zone_type)

herd_full_pa <- caribou_herd_pa %>%
  mutate(herd_area = as.numeric(st_area(.)),
         herd_area = as.numeric(set_units(herd_area, ha))) %>%
  st_drop_geometry() %>%
  filter(priority_zone_type == "High" | priority_zone_type == "Medium" | priority_zone_type == "Low") %>%
  group_by(herd) %>%
  summarise(herd_pa_sum = sum(herd_area)/10000)

herd_full_pa_by_zone <- caribou_herd_pa %>%
  mutate(herd_area = as.numeric(st_area(.)),
         herd_area = as.numeric(set_units(herd_area, ha))) %>%
  st_drop_geometry() %>%
  group_by(herd, priority_zone_type) %>%
  summarise(herd_pa_zone_type = sum(herd_area)/10000) %>%
  filter(priority_zone_type == "High" | priority_zone_type == "Medium" | priority_zone_type == "Low") %>%
  pivot_wider(names_from = priority_zone_type, values_from= herd_pa_zone_type) %>%
  rename(High_Protected = High) %>%
  rename(Medium_Protected = Medium) %>%
  rename(Low_Protected = Low)

gcr_caribou_pa <- smc_caribou_pa %>%
  mutate(herd_area = st_area(.),
         herd_area = as.numeric(set_units(herd_area, ha))) %>%
  st_drop_geometry() %>%
  filter(priority_zone_type == "High" | priority_zone_type == "Medium" | priority_zone_type == "Low") %>%
  group_by(herd) %>%
  summarise(herd_pa_gcr = sum(herd_area))

gcr_caribou_des <- smc_caribou_pa_des %>%
  mutate(herd_area = as.numeric(st_area(.)),
         herd_area = as.numeric(set_units(herd_area, ha))) %>%
  st_drop_geometry() %>%
  filter(priority_zone_type == "High" | priority_zone_type == "Medium" | priority_zone_type == "Low") %>%
  group_by(herd) %>%
  summarise(herd_des_gcr = sum(herd_area)/10000)

gcr_caribou_uwr <- smc_caribou_pa_uwr %>%
  mutate(herd_area = as.numeric(st_area(.)),
         herd_area = as.numeric(set_units(herd_area, ha))) %>%
  st_drop_geometry() %>%
  filter(designation == "uwr_conditional_harvest"|designation == "uwr_no_harvest") %>%
  filter(priority_zone_type == "High" | priority_zone_type == "Medium" | priority_zone_type == "Low") %>%
  group_by(herd) %>%
  summarise(herd_uwr_gcr = sum(herd_area)/10000)

gcr_caribou <- caribou_gcr_herd %>%
  mutate(herd_area = as.numeric(st_area(.)),
         herd_area = as.numeric(set_units(herd_area, ha))) %>%
  st_drop_geometry() %>%
  filter(priority_zone_type == "High" | priority_zone_type == "Medium" | priority_zone_type == "Low") %>%
  group_by(herd) %>%
  summarise(herd_area_gcr = sum(herd_area)/10000)

herd_summary <- herd_full %>%
  left_join(herd_full_by_zone) %>%
  left_join(herd_full_pa) %>%
  left_join(herd_full_pa_by_zone) %>%
  left_join(gcr_caribou) %>%
  left_join(gcr_caribou_pa) %>%
  mutate_if(is.numeric, ~replace(., is.na(.),0)) %>%
  mutate(perc_prot_area_gcr = herd_pa_gcr/herd_area_gcr*100,
         perc_area_in_gcr = herd_area_gcr/herd_area_sum*100,
         unprot_area_totals = herd_area_sum - herd_pa_sum,
         unprot_area_remaining_ppa_oecm_gcr = herd_area_gcr - herd_pa_gcr,
         perc_unprot_area_gcr_ppa_oecm = unprot_area_remaining_ppa_oecm_gcr/herd_area_gcr*100,
         gcr_caribou_area = sum(herd_area_gcr),
            gcr_caribou_area_pa = sum(herd_pa_gcr),
            total_caribou_area = sum(herd_area_sum),
            total_caribou_area_pa = sum(herd_pa_sum),
         perc_gcr_unprot = (gcr_caribou_area-gcr_caribou_area_pa)/gcr_caribou_area*100,
         perc_gcr_of_total = gcr_caribou_area/total_caribou_area*100,
         total_caribou_prot = total_caribou_area_pa/total_caribou_area*100,
         gcr_scenario = (gcr_caribou_area-gcr_caribou_area_pa+total_caribou_area_pa)/total_caribou_area*100)

write_csv(herd_summary, "out/herd_summary_Proposed.csv")

gcr_caribou <- caribou_gcr_herd %>%
  mutate(herd_area = as.numeric(st_area(.)),
         herd_area = as.numeric(set_units(herd_area, ha))) %>%
  st_drop_geometry() %>%
  filter(priority_zone_type == "High" | priority_zone_type == "Medium" | priority_zone_type == "Low") %>%
  group_by(herd) %>%
  summarise(herd_area_gcr = sum(herd_area)/10000) %>%
  left_join(herd_full) %>%
  left_join(herd_full_pa) %>%
  left_join(gcr_caribou_pa) %>%
  left_join(gcr_caribou_des) %>%
  left_join(gcr_caribou_uwr) %>%
  mutate(prot_area_gcr = herd_pa_gcr/herd_area_gcr*100,
         area_in_gcr = herd_area_gcr/herd_area_sum*100,
         herd_area_remaining_uwr = herd_area_gcr - herd_pa_gcr - herd_uwr_gcr,
         herd_area_remaining_des = herd_area_gcr - herd_des_gcr,
         herd_area_remaining_ppa_oecm = herd_area_gcr - herd_pa_gcr,
         prot_area_gcr_uwr = herd_area_remaining_uwr/herd_area_gcr*100)

write_csv(gcr_caribou, "out/gcr_caribou_herd_summary_Proposed.csv")


# PIP areas

tar_load(c(pip_gcr, pip_full_herd, pip))

gcr_pip_summary <- pip_gcr %>%
  mutate(pip_area = st_area(.),
         pip_area = as.numeric(set_units(pip_area, ha))) %>%
  st_drop_geometry() %>%
  group_by(CONTACT_ORGANIZATION_NAME, CNSLTN_AREA_NAME) %>%
  summarise(area = sum(pip_area))

du9_pip_summary <- pip_full_herd %>%
  mutate(pip_area = st_area(.),
         pip_area = as.numeric(set_units(pip_area, ha))) %>%
  st_drop_geometry() %>%
  group_by(CONTACT_ORGANIZATION_NAME, CNSLTN_AREA_NAME) %>%
  summarise(area = sum(pip_area))

all_pip_summary <- pip %>%
  mutate(pip_area = st_area(.),
         pip_area = as.numeric(set_units(pip_area, ha))) %>%
  st_drop_geometry() %>%
  group_by(CONTACT_ORGANIZATION_NAME, CNSLTN_AREA_NAME) %>%
  summarise(all_pip_area = sum(pip_area))

pip_areas <- st_read("out/pip_gcr.gpkg", crs=3005) %>%
    st_make_valid() %>%
    st_cast(to = "POLYGON", warn = FALSE) %>%
  mutate(pip_area_total = st_area(.),
         pip_area_total = as.numeric(set_units(pip_area_total, ha))) %>%
  st_drop_geometry() %>%
  group_by(CONTACT_ORGANIZATION_NAME, CNSLTN_AREA_NAME) %>%
  summarise(total_pip_area = sum(pip_area_total)) %>%

gcr_pip_sum <- gcr_pip_summary %>%
  left_join(all_pip_summary) %>%
  mutate(perc_in_gcr = round(area/all_pip_area*100, digits=2))

write_csv(gcr_pip_sum, "out/gcr_pip_summary.csv")

tar_load(c(cut_og, cut_og_pa))

cut_og_sum <- cut_og %>%
  mutate(og_area = st_area(.),
         og_area = as.numeric(set_units(og_area, ha))) %>%
  st_drop_geometry() %>%
  filter(LIFE_CYCLE_STATUS_CODE != "RETIRED") %>%
  group_by(LIFE_CYCLE_STATUS_CODE) %>%
  summarise(all_og_area = sum(og_area))

cut_og_pa_sum <- cut_og_pa %>%
  mutate(og_pa_area = st_area(.),
         og_pa_area = as.numeric(set_units(og_pa_area, ha))) %>%
  st_drop_geometry() %>%
  filter(LIFE_CYCLE_STATUS_CODE != "RETIRED") %>%
  group_by(LIFE_CYCLE_STATUS_CODE) %>%
  summarise(all_og_pa_area = sum(og_pa_area)) %>%
  left_join(cut_og_sum, by = "LIFE_CYCLE_STATUS_CODE") %>%
  mutate(unprot = all_og_area - all_og_pa_area)
