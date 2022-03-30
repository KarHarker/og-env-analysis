tar_load(c(priority_og_fish, priority_og_pa_fish, fish_area, priority_og_des_fish, pa_fish,
           priority_og_des_vis_fish, og_fish, og_pa_fish, og_des_lands_fish, og_des_lands_vis_fish,
           fish_ch, fish_ch_pa, des_lands_fish,
           fish_ch_des_vis_fish, fish_area, des_lands_fish_vis))

#priority og in area
fish_priority_og <- priority_og_fish %>%
  mutate(og_area = as.numeric(st_area(.)),
         og_area = as.numeric(set_units(og_area, ha))) %>%
  st_drop_geometry() %>%
  summarise(og_priority_area = sum(og_area)/10000)

#priority og in protected area
fish_priority_og_pa <- priority_og_pa_fish %>%
  mutate(og_pa_area = as.numeric(st_area(.)),
         og_pa_area = as.numeric(set_units(og_pa_area, ha))) %>%
  st_drop_geometry() %>%
  summarise(og_priority_pa_area = sum(og_pa_area)/10000)

#priority og in uwr area
fish_priority_og_uwr<- priority_og_des_vis_fish %>%
  mutate(og_des_area = as.numeric(st_area(.)),
         og_des_area = as.numeric(set_units(og_des_area, ha))) %>%
  st_drop_geometry() %>%
  filter(designation == "uwr_conditional_harvest"|designation == "uwr_no_harvest") %>%
  summarise(og_priority_uwr_area = sum(og_des_area)/10000)

#priority og is all des area
fish_priority_og_des<- priority_og_des_fish %>%
  mutate(og_des_area = as.numeric(st_area(.)),
         og_des_area = as.numeric(set_units(og_des_area, ha))) %>%
  st_drop_geometry() %>%
  summarise(og_priority_des_area = sum(og_des_area)/10000)

#all og sums
fish_all_og <- og_fish %>%
  mutate(og_area = as.numeric(st_area(.)),
         og_area = as.numeric(set_units(og_area, ha))) %>%
  st_drop_geometry() %>%
  summarise(all_og_area = sum(og_area)/10000)

fish_all_og_pa <- og_pa_fish %>%
  mutate(og_pa_area = as.numeric(st_area(.)),
         og_pa_area = as.numeric(set_units(og_pa_area, ha))) %>%
  st_drop_geometry() %>%
  summarise(all_og_pa_area = sum(og_pa_area)/10000)

fish_all_og_uwr <- og_des_lands_vis_fish %>%
  mutate(og_uwr_area = as.numeric(st_area(.)),
         og_uwr_area = as.numeric(set_units(og_uwr_area, ha))) %>%
  st_drop_geometry() %>%
  filter(designation == "uwr_conditional_harvest"|designation == "uwr_no_harvest") %>%
  summarise(all_uwr_area = sum(og_uwr_area)/10000)

fish_all_og_des <- og_des_lands_fish %>%
  mutate(og_des_area = as.numeric(st_area(.)),
         og_des_area = as.numeric(set_units(og_des_area, ha))) %>%
  st_drop_geometry() %>%
  summarise(all_des_og_area = sum(og_des_area)/10000)

#fish all area
fish_area <- fish_area %>%
  mutate(caribou_area = as.numeric(st_area(.)),
         caribou_area = as.numeric(set_units(caribou_area, ha))) %>%
  st_drop_geometry() %>%
  summarise(caribou_area = sum(caribou_area)/10000)

fish_pa <- pa_fish %>%
  mutate(caribou_pa_area = as.numeric(st_area(.)),
         caribou_pa_area = as.numeric(set_units(caribou_pa_area, ha))) %>%
  st_drop_geometry() %>%
  group_by(pa_type) %>%
  summarise(caribou_pa_area = sum(caribou_pa_area)/10000) %>%
  pivot_wider(names_from=pa_type, values_from=caribou_pa_area)

fish_uwr <- des_lands_fish_vis %>%
  mutate(proposed_cons_area= as.numeric(st_area(.)),
         proposed_cons_area = as.numeric(set_units(proposed_cons_area, ha))) %>%
  st_drop_geometry() %>%
  group_by(designation) %>%
  summarise(des_area_type = sum(proposed_cons_area)/10000) %>%
  filter(designation == "uwr_conditional_harvest"|designation == "uwr_no_harvest")%>%
  pivot_wider(names_from=designation, values_from=des_area_type)
#write_sf(des_lands_fish_vis, "out/des_lands_fish_vis.gpkg")

fish_des <- des_lands_fish %>%
  mutate(caribou_area_des = as.numeric(st_area(.)),
         caribou_area_des = as.numeric(set_units(caribou_area_des, ha))) %>%
  st_drop_geometry() %>%
  summarise(total_area_des = sum(caribou_area_des)/10000)

fish_sums <- cbind(fish_area, fish_pa, fish_uwr, fish_des, fish_all_og, fish_all_og_pa, fish_all_og_uwr, fish_all_og_des,
                  fish_priority_og, fish_priority_og_pa, fish_priority_og_uwr, fish_priority_og_des)

#area sums
fish_sum <- fish_sums %>%
  mutate(area_no_conservation = caribou_area - total_area_des,
         area_outside_parks = caribou_area - ppa)

fish_des <- des_lands_fish %>%
  mutate(caribou_area_des = as.numeric(st_area(.)),
         caribou_area_des = as.numeric(set_units(caribou_area_des, ha))) %>%
  summarise(total_area_des = sum(caribou_area_des)/10000)
write_sf(fish_des, "out/des_lands_fish_all.gpkg")

#Table 2 - summarize the type and area of conservation area (individually)
fish_des_summary_flat <- des_lands_fish %>%
  mutate(proposed_cons_area= as.numeric(st_area(.)),
         proposed_cons_area = as.numeric(set_units(proposed_cons_area, ha))) %>%
  st_drop_geometry() %>%
  group_by(designations) %>%
  summarise(des_area_type = sum(proposed_cons_area)/10000)
#write_sf(des_lands_fish_vis, "out/des_lands_fish_vis.gpkg")
write_csv(fish_des_summary_flat, "out/des_lands_flat_fish.csv")

#Table 2 - summarize the type and area of conservation area (with overlaps)
fish_des_summary_overlapping <- des_lands_fish_vis %>%
  mutate(proposed_cons_area= as.numeric(st_area(.)),
         proposed_cons_area = as.numeric(set_units(proposed_cons_area, ha))) %>%
  st_drop_geometry() %>%
  group_by(designation) %>%
  summarise(des_area_type = sum(proposed_cons_area)/10000)
write_csv(fish_des_summary_overlapping, "out/des_lands_overlapping_fish.csv")

## designated lands - surrounding area

tar_load(c(fish_des_10, fish_des_vis_10))

fish_des_flat <- fish_des_10 %>%
  mutate(caribou_area_des = as.numeric(st_area(.)),
         des_area = as.numeric(set_units(caribou_area_des, ha)))
write_sf(fish_des_flat, "out/des_lands_fish_10.gpkg")

fish_des_vis <- fish_des_vis_10 %>%
  mutate(caribou_area_des = as.numeric(st_area(.)),
         des_area = as.numeric(set_units(caribou_area_des, ha)))
write_sf(fish_des_vis, "out/fish_des_vis_10.gpkg")

#Table 2 - summarize the type and area of conservation area (individually)
fish_des_summary_flat <- des_lands_fish %>%
  mutate(proposed_cons_area= as.numeric(st_area(.)),
         proposed_cons_area = as.numeric(set_units(proposed_cons_area, ha))) %>%
  st_drop_geometry() %>%
  group_by(designations) %>%
  summarise(des_area_type = sum(proposed_cons_area)/10000)
#write_sf(des_lands_fish_vis, "out/des_lands_fish_vis.gpkg")
write_csv(fish_des_summary_flat, "out/des_lands_flat_fish.csv")

#Table 2 - summarize the type and area of conservation area (with overlaps)
fish_des_summary_overlapping <- des_lands_fish_vis %>%
  mutate(proposed_cons_area= as.numeric(st_area(.)),
         proposed_cons_area = as.numeric(set_units(proposed_cons_area, ha))) %>%
  st_drop_geometry() %>%
  group_by(designation) %>%
  summarise(des_area_type = sum(proposed_cons_area)/10000)
write_csv(fish_des_summary_overlapping, "out/des_lands_overlapping_fish.csv")


fish_og<-cbind(fish_priority_og, fish_priority_og_pa, fish_priority_og_des, fish_priority_og_uwr, fish_all_og, fish_all_og_pa,
              fish_all_og_uwr, fish_all_og_uwr, fish_area)

fish_summary <- fish_og %>%
  mutate(priority_unprotected_og = og_priority_area - og_priority_pa_area - og_priority_uwr_area,
         all_unprotected_og = all_og_area-all_og_pa_area-all_uwr_area,
         perc_priority_unprot = priority_unprotected_og/og_priority_area*100,
         perc_all_unprot = all_unprotected_og/all_og_area*100,
         #unprot_area = caribou_area - ppa - oecm,
         #priority_no_des = og_priority_area - og_des_area,
         non_des_area = caribou_area - ppa - oecm -caribou_area_des,
         perc_unprot = non_des_area/caribou_area*100)

pa_fish <- pa_fish %>%
  mutate(pa_area= as.numeric(st_area(.)),
         pa_area = as.numeric(set_units(pa_area, ha))) %>%
  group_by(pa_type) %>%
  summarise(area_type = sum(pa_area))
write_sf(pa_fish, "out/pa_fish.gpkg")

ppa <- pa_fish %>%
  filter(pa_type == "ppa")
write_sf(ppa, "out/ppa_fish.gpkg")

oecm <- pa_fish %>%
  filter(pa_type == "oecm")
write_sf(oecm, "out/oecm_fish.gpkg")


tar_load(c(fish_ch, fish_ch_des))

ch <-fish_ch %>%
  mutate(ch_area = as.numeric(st_area(.)),
         ch_area = as.numeric(set_units(ch_area, ha))) %>%
  group_by(common_name_english) %>%
  summarise(ch_fishca = sum(ch_area)/10000)

write_sf(ch, "out/fish_ch.gpkg")

ch <- ch %>%
  st_drop_geometry()

ch_des <-fish_ch_des %>%
  mutate(ch_pa_area = as.numeric(st_area(.)),
         ch_pa_area = as.numeric(set_units(ch_pa_area, ha))) %>%
  group_by(common_name_english) %>%
  summarise(ch_fishca_pa = sum(ch_pa_area)/10000)

write_sf(ch_des, "out/ch_des.gpkg")

ch_des <- ch_des %>%
  st_drop_geometry()

ch_comb <- ch %>%
  left_join(ch_des) %>%
  mutate(perc_cons = ch_fishca_pa/ch_fishca*100)

write_csv(ch_comb, "fish_ch.csv")

ch_des <-fish_ch_des %>%
  mutate(ch_pa_area = as.numeric(st_area(.)),
         ch_pa_area = as.numeric(set_units(ch_pa_area, ha))) %>%
  group_by(common_name_english) %>%
  summarise(ch_fishca_pa = sum(ch_pa_area)/10000)

write_sf(fish_pa_10, "out/fish_pa_10.gpkg")

ch <-fish_ch %>%
  mutate(ch_area = as.numeric(st_area(.)),
         ch_area = as.numeric(set_units(ch_area, ha))) %>%
  st_drop_geometry() %>%
  group_by(common_name_english) %>%
  summarise(ch_fishca = sum(ch_area)/10000)

ch_pa <-fish_ch_pa %>%
  mutate(ch_pa_area = as.numeric(st_area(.)),
         ch_pa_area = as.numeric(set_units(ch_pa_area, ha))) %>%
  st_drop_geometry() %>%
  group_by(common_name_english) %>%
  summarise(ch_fishca_pa = sum(ch_pa_area)/10000)

ch_des <-fish_ch_des_ %>%
  mutate(ch_uwr_area = as.numeric(st_area(.)),
         ch_uwr_area = as.numeric(set_units(ch_uwr_area, ha))) %>%
  st_drop_geometry() %>%
  group_by(designation, common_name_english) %>%
  summarise(ch_uwr_area = sum(ch_uwr_area)) %>%
  filter(designation == "uwr_conditional_harvest"|designation == "uwr_no_harvest") %>%
  ungroup() %>%
  group_by(common_name_english) %>%
  summarise(ch_fishca_uwr = sum(ch_uwr_area)/10000)

ch_overall <- ch %>%
  left_join(ch_pa, by='common_name_english') %>%
  left_join(ch_des, by='common_name_english') %>%
  mutate(unprot = ch_fishca - ch_fishca_pa - ch_fishca_uwr)

write_csv(ch_overall, "out/ch_overall.csv")

ch_10 <- fish_ch_10 %>%
  mutate(ch_pa_area = st_area(.),
         ch_pa_area = as.numeric(set_units(ch_pa_area, ha))) %>%
  st_drop_geometry() %>%
  group_by(common_name_english) %>%
  summarise(ch_fishca_pa = sum(ch_pa_area))

write_sf(fish_ch_10, "out/fish_ch_10.gpkg")


fish_ch_area <-fish_ch %>%
  mutate(ch_area = as.numeric(st_area(.)),
         ch_area = as.numeric(set_units(ch_area, ha))) %>%
  group_by(common_name_english) %>%
  summarise(sum_ch_area = sum(ch_area))
write_sf(fish_ch_area , "out/fish_ch.gpkg")

whitebark_pine <-fish_ch %>%
  mutate(ch_area = as.numeric(st_area(.)),
         ch_area = as.numeric(set_units(ch_area, ha))) %>%
  group_by(common_name_english) %>%
  summarise(sum_ch_area = sum(ch_area)) %>%
  filter(common_name_english == "Whitebark Pine")

write_sf(whitebark_pine, "out/whitebark_pine.gpkg")

ch_des_sf <-fish_ch_des_vis %>%
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

caribou_area_pa <-fish_caribou_pa %>%
  mutate(pa_area = as.numeric(st_area(.)),
         pa_area = as.numeric(set_units(pa_area, km^2))) %>%
  st_drop_geometry() %>%
  group_by(herd) %>%
  summarise(sum_pa_area = sum(pa_area))

caribou_area_pa_des <- fish_caribou_pa_des %>%
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


tar_load(c(fish_forest, fish_tenures, fish_harv, fish_mine_potential, fish_land))

fish_cutblocks <- fish_forest %>%
  mutate(for_area = as.numeric(st_area(.)),
         for_area = as.numeric(set_units(for_area, ha))) %>%
  st_drop_geometry() %>%
  group_by(LIFE_CYCLE_STATUS_CODE) %>%
  summarise(for_area_sum = sum(for_area)/10000,
            n_cutblock_total = n())

fish_cutblock_licensees <- fish_forest %>%
  mutate(for_area = as.numeric(st_area(.)),
         for_area = as.numeric(set_units(for_area, ha))) %>%
  st_drop_geometry() %>%
  group_by(CLIENT_NAME, LIFE_CYCLE_STATUS_CODE) %>%
  summarise(for_licensee_sum = sum(for_area)/10000,
            n_cutblocks = n()) %>%
  left_join(fish_cutblocks) %>%
  mutate(perc_of_total = for_licensee_sum/for_area_sum*100)

write_csv(fish_cutblock_licensees, "out/fish_licensee_list.csv")


fish_tenure_sum <- fish_tenures %>%
  mutate(for_area = as.numeric(st_area(.)),
         for_area = as.numeric(set_units(for_area, ha))) %>%
  st_drop_geometry() %>%
  group_by(CLIENT_NAME, LIFE_CYCLE_STATUS_CODE) %>%
  summarise(for_area_sum = sum(for_area)/10000,
            n_cutblocks = n())

write_csv(fish_tenure_sum, "out/fish_managed_license_list.csv")

fish_harvest_sum <- fish_harv %>%
  mutate(for_area = as.numeric(st_area(.)),
         for_area = as.numeric(set_units(for_area, ha))) %>%
  st_drop_geometry() %>%
  filter(LIFE_CYCLE_STATUS_CODE != "RETIRED") %>%
  group_by(CLIENT_NAME, LIFE_CYCLE_STATUS_CODE) %>%
  summarise(harv_area_sum = sum(for_area)/10000,
            n_cutblocks = n()) %>%
  left_join(fish_cutblocks) %>%
  mutate(perc_of_total = harv_area_sum/for_area_sum*100)

write_csv(fish_harvest_sum, "out/fish_harv_auth_list.csv")

#Tree Farm License

tar_load(c(fish_tfl, fish_tfl_10))

fish_tfl_summary <- fish_tfl %>%
  mutate(pip_area = st_area(.),
         tfl_area = as.numeric(set_units(pip_area, ha))) %>%
  st_drop_geometry() %>%
  select(FOREST_FILE_ID, LICENCEE, tfl_area)

write_sf(fish_tfl, "out/fish_tfl.gpkg")


fish_tfl_summary_10 <- fish_tfl_10 %>%
  mutate(pip_area = st_area(.),
         tfl_area = as.numeric(set_units(pip_area, ha))) %>%
  st_drop_geometry() %>%
  group_by(FOREST_FILE_ID, LICENCEE) %>%
  summarise(tfl_area = sum(tfl_area))

write_sf(fish_tfl_10, "out/fish_tfl_10.gpkg")


fish_land_sum <- fish_land %>%
  mutate(land_area = as.numeric(st_area(.)),
         land_area = as.numeric(set_units(land_area, ha))) %>%
  st_drop_geometry() %>%
  group_by(TENURE_STATUS, TENURE_TYPE, TENURE_PURPOSE, TENURE_SUBPURPOSE) %>%
  summarise(land_area_sum = sum(land_area)/10000,
            n_tenure_total = n()) %>%
  ungroup() %>%
  mutate(n_tenure_overall=sum(n_tenure_total))

write_csv(fish_land_sum, "out/fish_land_tenure.csv")


fish_mineral <- fish_mine_potential %>%
  mutate(mine_area = as.numeric(st_area(.)),
         mine_area = as.numeric(set_units(mine_area, ha))) %>%
  st_drop_geometry() %>%
  group_by(OWNER_NAME) %>%
  summarise(mine_area_sum = sum(mine_area)/10000,
            n_claim = n()) %>%
  ungroup() %>%
  mutate(overall_area = sum(mine_area_sum),
         n_claims_overall=sum(n_claim))

write_csv(fish_mineral, "out/fish_mineral_claims.csv")


# PIP areas

tar_load(c(pip_fish))

fish_pip_summary <- pip_fish %>%
  mutate(pip_area = st_area(.),
         pip_area = as.numeric(set_units(pip_area, ha))) %>%
  #st_drop_geometry() %>%
  group_by(CONTACT_ORGANIZATION_NAME, CNSLTN_AREA_NAME) %>%
  summarise(area = sum(pip_area))

write_sf(fish_pip_summary, "out/pip_fish.gpkg")

fish_land_10_mod <- fish_land_10 %>%
  mutate(pip_area = st_area(.),
         pip_area = as.numeric(set_units(pip_area, ha))) %>%
  group_by(TENURE_PURPOSE, TENURE_SUBPURPOSE) %>%
  filter(TENURE_PURPOSE != "ENVIRONMENT, CONSERVATION, & RECR") %>%
  summarise(area = sum(pip_area))

fish_land_sum <- fish_land_10_mod %>%
  st_drop_geometry()

tar_load(c(fish_land_10, fish_forest_10, fish_mine_active_10, fish_mine_potential_10, fish_eao_10,
           fish_tenures_10, fish_harv_10))

write_sf(fish_land_10_mod, "out/fish_land_10_mod.gpkg")
write_sf(fish_forest_10, "out/fish_forest_10.gpkg")
write_sf(fish_mine_potential_10, "out/fish_mine_potential_10.gpkg")
write_sf(fish_eao_10, "out/fish_eao_10.gpkg")
write_sf(fish_tenures_10, "out/fish_tenures_10.gpkg")
write_sf(fish_harv_10, "out/fish_harv_10.gpkg")
