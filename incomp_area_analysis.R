tar_load(c(priority_og_inc, priority_og_pa_inc, incomp_area, priority_og_des_inc, pa_inc,
           priority_og_des_vis_inc, og_inc, og_pa_inc, og_des_lands_inc, og_des_lands_vis_inc,
           inc_ch, inc_ch_pa, des_lands_inc,
           inc_ch_des_vis, incomp_area, des_lands_inc_vis))

#priority og in area
inc_priority_og <- priority_og_inc %>%
  mutate(og_area = as.numeric(st_area(.)),
         og_area = as.numeric(set_units(og_area, ha))) %>%
  st_drop_geometry() %>%
  summarise(og_priority_area = sum(og_area)/10000)

#priority og in protected area
inc_priority_og_pa <- priority_og_pa_inc %>%
  mutate(og_pa_area = as.numeric(st_area(.)),
         og_pa_area = as.numeric(set_units(og_pa_area, ha))) %>%
  st_drop_geometry() %>%
  summarise(og_priority_pa_area = sum(og_pa_area)/10000)

#priority og in uwr area
inc_priority_og_uwr<- priority_og_des_vis_inc %>%
  mutate(og_des_area = as.numeric(st_area(.)),
         og_des_area = as.numeric(set_units(og_des_area, ha))) %>%
  st_drop_geometry() %>%
  filter(designation == "uwr_conditional_harvest"|designation == "uwr_no_harvest") %>%
  summarise(og_priority_uwr_area = sum(og_des_area)/10000)

#priority og is all des area
inc_priority_og_des<- priority_og_des_inc %>%
  mutate(og_des_area = as.numeric(st_area(.)),
         og_des_area = as.numeric(set_units(og_des_area, ha))) %>%
  st_drop_geometry() %>%
  summarise(og_priority_des_area = sum(og_des_area)/10000)

#all og sums
inc_all_og <- og_inc %>%
  mutate(og_area = as.numeric(st_area(.)),
         og_area = as.numeric(set_units(og_area, ha))) %>%
  st_drop_geometry() %>%
  summarise(all_og_area = sum(og_area)/10000)

inc_all_og_pa <- og_pa_inc %>%
  mutate(og_pa_area = as.numeric(st_area(.)),
         og_pa_area = as.numeric(set_units(og_pa_area, ha))) %>%
  st_drop_geometry() %>%
  summarise(all_og_pa_area = sum(og_pa_area)/10000)

inc_all_og_uwr <- og_des_lands_vis_inc %>%
  mutate(og_uwr_area = as.numeric(st_area(.)),
         og_uwr_area = as.numeric(set_units(og_uwr_area, ha))) %>%
  st_drop_geometry() %>%
  filter(designation == "uwr_conditional_harvest"|designation == "uwr_no_harvest") %>%
  summarise(all_uwr_area = sum(og_uwr_area)/10000)

inc_all_og_des <- og_des_lands_inc %>%
  mutate(og_des_area = as.numeric(st_area(.)),
         og_des_area = as.numeric(set_units(og_des_area, ha))) %>%
  st_drop_geometry() %>%
  summarise(all_og_des_area = sum(og_des_area)/10000)

#inc all area
inc_area <- incomp_area %>%
  mutate(caribou_area = as.numeric(st_area(.)),
         caribou_area = as.numeric(set_units(caribou_area, ha))) %>%
  st_drop_geometry() %>%
  summarise(incomp_area = sum(caribou_area)/10000)

inc_pa <- pa_inc %>%
  mutate(caribou_pa_area = as.numeric(st_area(.)),
         caribou_pa_area = as.numeric(set_units(caribou_pa_area, ha))) %>%
  st_drop_geometry() %>%
  group_by(pa_type) %>%
  summarise(caribou_pa_area = sum(caribou_pa_area)/10000) %>%
  pivot_wider(names_from=pa_type, values_from=caribou_pa_area)

inc_uwr <- des_lands_inc_vis %>%
  mutate(proposed_cons_area= as.numeric(st_area(.)),
         proposed_cons_area = as.numeric(set_units(proposed_cons_area, ha))) %>%
  st_drop_geometry() %>%
  group_by(designation) %>%
  summarise(des_area_type = sum(proposed_cons_area)/10000) %>%
  filter(designation == "uwr_conditional_harvest"|designation == "uwr_no_harvest")%>%
  pivot_wider(names_from=designation, values_from=des_area_type)
#write_sf(des_lands_inc_vis, "out/des_lands_inc_vis.gpkg")

inc_des <- des_lands_inc %>%
  mutate(caribou_area_des = as.numeric(st_area(.)),
         caribou_area_des = as.numeric(set_units(caribou_area_des, ha))) %>%
  st_drop_geometry() %>%
  summarise(total_area_des = sum(caribou_area_des)/10000)

inc_sums <- cbind(inc_area, inc_pa, inc_uwr, inc_des, inc_all_og, inc_all_og_pa, inc_all_og_uwr, inc_all_og_des,
                  inc_priority_og, inc_priority_og_pa, inc_priority_og_uwr, inc_priority_og_des)

#area sums
inc_sum <- inc_sums %>%
  mutate(area_no_conservation = incomp_area - total_area_des,
         area_outside_parks = incomp_area - ppa)

inc_des <- des_lands_inc %>%
  mutate(caribou_area_des = as.numeric(st_area(.)),
         caribou_area_des = as.numeric(set_units(caribou_area_des, ha))) %>%
  summarise(total_area_des = sum(caribou_area_des)/10000)
write_sf(inc_des, "out/des_lands_inc_all.gpkg")

#Table 2 - summarize the type and area of conservation area (individually)
inc_des_summary_flat <- des_lands_inc %>%
  mutate(proposed_cons_area= as.numeric(st_area(.)),
         proposed_cons_area = as.numeric(set_units(proposed_cons_area, ha))) %>%
  st_drop_geometry() %>%
  group_by(designations) %>%
  summarise(des_area_type = sum(proposed_cons_area)/10000) %>%
  ungroup() %>%
  mutate(des_area_all = sum(des_area_type))
#write_sf(des_lands_inc_vis, "out/des_lands_inc_vis.gpkg")
write_csv(inc_des_summary_flat, "out/des_lands_flat_inc.csv")

#Table 2 - summarize the type and area of conservation area (with overlaps)
inc_des_summary_overlapping <- des_lands_inc_vis %>%
  mutate(proposed_cons_area= as.numeric(st_area(.)),
         proposed_cons_area = as.numeric(set_units(proposed_cons_area, ha))) %>%
  st_drop_geometry() %>%
  group_by(designation) %>%
  summarise(des_area_type = sum(proposed_cons_area)/10000)
write_csv(inc_des_summary_overlapping, "out/des_lands_overlapping_inc.csv")



inc_og<-cbind(inc_priority_og, inc_priority_og_pa, inc_priority_og_des, inc_priority_og_uwr, inc_all_og, inc_all_og_pa,
              inc_all_og_uwr, inc_all_og_uwr, inc_area)

inc_summary_og <- inc_og %>%
  mutate(priority_unprotected_og = og_priority_area - og_priority_pa_area - og_priority_uwr_area,
         all_unprotected_og = all_og_area-all_og_pa_area-all_uwr_area,
         perc_priority_unprot = priority_unprotected_og/og_priority_area*100,
         perc_all_unprot = all_unprotected_og/all_og_area*100)
         #unprot_area = caribou_area - ppa - oecm,
         #priority_no_des = og_priority_area - og_des_area,
         #non_des_area = caribou_area - ppa - oecm -caribou_area_des,
         #perc_unprot = non_des_area/caribou_area*100)

pa_inc <- pa_inc %>%
  mutate(pa_area= as.numeric(st_area(.)),
         pa_area = as.numeric(set_units(pa_area, ha))) %>%
  group_by(pa_type) %>%
  summarise(area_type = sum(pa_area))
write_sf(pa_inc, "out/pa_inc.gpkg")

ppa <- pa_inc %>%
  filter(pa_type == "ppa")
write_sf(ppa, "out/ppa_inc.gpkg")

oecm <- pa_inc %>%
  filter(pa_type == "oecm")
write_sf(oecm, "out/oecm_inc.gpkg")

tar_load(c(inc_ch, inc_ch_des))

ch <-inc_ch %>%
  mutate(ch_area = as.numeric(st_area(.)),
         ch_area = as.numeric(set_units(ch_area, ha))) %>%
  group_by(common_name_english) %>%
  summarise(ch_incca = sum(ch_area)/10000)

write_sf(ch, "out/inc_ch.gpkg")

ch <- ch %>%
  st_drop_geometry()

ch_des <-inc_ch_des %>%
  mutate(ch_pa_area = as.numeric(st_area(.)),
         ch_pa_area = as.numeric(set_units(ch_pa_area, ha))) %>%
  group_by(common_name_english) %>%
  summarise(ch_incca_pa = sum(ch_pa_area)/10000)

write_sf(ch_des, "out/ch_des.gpkg")

ch_des <- ch_des %>%
  st_drop_geometry()

ch_comb <- ch %>%
  left_join(ch_des) %>%
  mutate(perc_cons = ch_incca_pa/ch_incca*100)

write_csv(ch_comb, "inc_ch.csv")

ch_overall <- ch %>%
  left_join(ch_pa, by='common_name_english') %>%
  left_join(ch_des, by='common_name_english') %>%
  mutate(unprot = ch_incca - ch_incca_pa - ch_incca_uwr)

write_csv(ch_overall, "out/ch_inc_overall.csv")


caribou_ch <-inc_ch %>%
  mutate(ch_area = as.numeric(st_area(.)),
         ch_area = as.numeric(set_units(ch_area, ha))) %>%
  group_by(common_name_english) %>%
  summarise(sum_ch_area = sum(ch_area)) %>%
  filter(common_name_english == "Caribou - Southern Mountain population")
write_sf(caribou_ch, "out/caribou_ch.gpkg")

whitebark_pine <-inc_ch %>%
  mutate(ch_area = as.numeric(st_area(.)),
         ch_area = as.numeric(set_units(ch_area, ha))) %>%
  group_by(common_name_english) %>%
  summarise(sum_ch_area = sum(ch_area)) %>%
  filter(common_name_english == "Whitebark Pine")

write_sf(whitebark_pine, "out/whitebark_pine.gpkg")

ch_des_sf <-inc_ch_des_vis %>%
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

caribou_area_pa <-inc_caribou_pa %>%
  mutate(pa_area = as.numeric(st_area(.)),
         pa_area = as.numeric(set_units(pa_area, km^2))) %>%
  st_drop_geometry() %>%
  group_by(herd) %>%
  summarise(sum_pa_area = sum(pa_area))

caribou_area_pa_des <- inc_caribou_pa_des %>%
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


tar_load(c(inc_forest, inc_tenures, inc_harv, inc_mine_potential, inc_land))

inc_cutblocks <- inc_forest %>%
  mutate(for_area = as.numeric(st_area(.)),
         for_area = as.numeric(set_units(for_area, ha))) %>%
  st_drop_geometry() %>%
  group_by(LIFE_CYCLE_STATUS_CODE) %>%
  summarise(for_area_sum = sum(for_area)/10000,
            n_cutblock_total = n())

inc_cutblock_licensees <- inc_forest %>%
  mutate(for_area = as.numeric(st_area(.)),
         for_area = as.numeric(set_units(for_area, ha))) %>%
  st_drop_geometry() %>%
  group_by(CLIENT_NAME, LIFE_CYCLE_STATUS_CODE) %>%
  summarise(for_licensee_sum = sum(for_area)/10000,
            n_cutblocks = n()) %>%
  left_join(inc_cutblocks) %>%
  mutate(perc_of_total = for_licensee_sum/for_area_sum*100)

write_csv(inc_cutblock_licensees, "out/inc_licensee_list.csv")


inc_tenure_sum <- inc_tenures %>%
  mutate(for_area = as.numeric(st_area(.)),
         for_area = as.numeric(set_units(for_area, ha))) %>%
  st_drop_geometry() %>%
  group_by(CLIENT_NAME, LIFE_CYCLE_STATUS_CODE) %>%
  summarise(for_area_sum = sum(for_area)/10000,
            n_cutblocks = n())

write_csv(inc_tenure_sum, "out/inc_managed_license_list.csv")

inc_harvest_sum <- inc_harv %>%
  mutate(for_area = as.numeric(st_area(.)),
         for_area = as.numeric(set_units(for_area, ha))) %>%
  st_drop_geometry() %>%
  filter(LIFE_CYCLE_STATUS_CODE != "RETIRED") %>%
  group_by(CLIENT_NAME, LIFE_CYCLE_STATUS_CODE) %>%
  summarise(harv_area_sum = sum(for_area)/10000,
            n_cutblocks = n()) %>%
  left_join(inc_cutblocks) %>%
  mutate(perc_of_total = harv_area_sum/for_area_sum*100)

write_csv(inc_harvest_sum, "out/inc_harv_auth_list.csv")


inc_land_sum <- inc_land %>%
  mutate(land_area = as.numeric(st_area(.)),
         land_area = as.numeric(set_units(land_area, ha))) %>%
  st_drop_geometry() %>%
  group_by(TENURE_STATUS, TENURE_TYPE, TENURE_PURPOSE, TENURE_SUBPURPOSE) %>%
  summarise(land_area_sum = sum(land_area)/10000,
            n_tenure_total = n()) %>%
  ungroup() %>%
  mutate(n_tenure_overall=sum(n_tenure_total))

write_csv(inc_land_sum, "out/inc_land_tenure.csv")


inc_mineral <- inc_mine_potential %>%
  mutate(mine_area = as.numeric(st_area(.)),
         mine_area = as.numeric(set_units(mine_area, ha))) %>%
  st_drop_geometry() %>%
  group_by(OWNER_NAME) %>%
  summarise(mine_area_sum = sum(mine_area)/10000,
            n_claim = n()) %>%
  ungroup() %>%
  mutate(overall_area = sum(mine_area_sum),
         n_claims_overall=sum(n_claim))

write_csv(inc_mineral, "out/inc_mineral_claims.csv")


# PIP areas

tar_load(c(pip_inc))

inc_pip_summary <- pip_inc %>%
  mutate(pip_area = st_area(.),
         pip_area = as.numeric(set_units(pip_area, ha))) %>%
  st_drop_geometry() %>%
  group_by(CONTACT_ORGANIZATION_NAME, CNSLTN_AREA_NAME) %>%
  summarise(area = sum(pip_area))

