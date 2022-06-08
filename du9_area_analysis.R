source('packages.R')
conflict_prefer("filter", "dplyr")

tar_load(c(priority_og_du9, priority_og_pa_du9, du9omp_area, priority_og_des_du9, pa_du9,
           priority_og_des_vis_du9, og_du9, og_pa_du9, og_des_lands_du9, og_des_lands_vis_du9,
           du9_ch, du9_ch_pa, des_lands_du9,
           du9_ch_des_vis, du9, des_lands_du9_vis))


# ******************************
# Old Growth Analysis -----------------------------------------------------
# ******************************

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


priority_herds <- c("Barkerville", "Hart Ranges", "North Cariboo", "Wells Gray North", "Wells Gray South",
                    "Groundhog", "Columbia North", "Central Selkirks")


#priority og in protected area
du9_priority_og_pa_recovery <- priority_og_pa_du9 %>%
  mutate(og_pa_area = st_area(.),
         og_pa_area = as.numeric(set_units(og_pa_area, ha))) %>%
  st_drop_geometry() %>%
  filter(herd %in% priority_herds) %>%
  group_by(critical_habitat_type, priority_zone_type) %>%
  summarise(og_priority_pa_area = sum(og_pa_area))

#priority og in area
du9_priority_og_recovery <- priority_og_du9 %>%
  mutate(og_area = st_area(.),
         og_area = as.numeric(set_units(og_area, ha))) %>%
  st_drop_geometry() %>%
  filter(herd %in% priority_herds) %>%
  group_by(critical_habitat_type, priority_zone_type) %>%
  summarise(og_priority_area = sum(og_area)) %>%
  left_join(du9_priority_og_pa_recovery, by = c('critical_habitat_type', 'priority_zone_type')) %>%
  mutate(unprotected_area_recovery = og_priority_area - og_priority_pa_area) %>%
  group_by(critical_habitat_type) %>%
  mutate(unprotected_by_hab_type = sum(unprotected_area_recovery))

write_csv(du9_priority_og_recovery, 'out/du9/du9-priority-og_habitat-type_recovery-herds.csv')

du9_priority_og_pa_all <- priority_og_pa_du9 %>%
  mutate(og_pa_area = st_area(.),
         og_pa_area = as.numeric(set_units(og_pa_area, ha))) %>%
  st_drop_geometry() %>%
  #filter(herd %in% priority_herds) %>%
  group_by(critical_habitat_type, priority_zone_type) %>%
  summarise(og_priority_pa_area = sum(og_pa_area))

#priority og in area
du9_priority_og_all <- priority_og_du9 %>%
  mutate(og_area = st_area(.),
         og_area = as.numeric(set_units(og_area, ha))) %>%
  st_drop_geometry() %>%
  #filter(herd %in% priority_herds) %>%
  group_by(critical_habitat_type, priority_zone_type) %>%
  summarise(og_priority_area = sum(og_area)) %>%
  left_join(du9_priority_og_pa_all, by = c('critical_habitat_type', 'priority_zone_type')) %>%
  mutate(unprotected_area_all = og_priority_area - og_priority_pa_area) %>%
  group_by(critical_habitat_type) %>%
  mutate(unprotected_by_hab_type = sum(unprotected_area_all))

write_csv(du9_priority_og_recovery, 'out/du9/du9-priority-og_habitat-type_all-herds.csv')


#priority og in protected area
du9_og_pa_recovery <- og_pa_du9 %>%
  mutate(og_pa_area = st_area(.),
         og_pa_area = as.numeric(set_units(og_pa_area, ha))) %>%
  st_drop_geometry() %>%
  filter(herd %in% priority_herds) %>%
  group_by(critical_habitat_type, priority_zone_type) %>%
  summarise(og_pa_area = sum(og_pa_area))

#priority og in area
du9_og_recovery <- og_du9 %>%
  mutate(og_area = st_area(.),
         og_area = as.numeric(set_units(og_area, ha))) %>%
  st_drop_geometry() %>%
  filter(herd %in% priority_herds) %>%
  group_by(critical_habitat_type, priority_zone_type) %>%
  summarise(og_area = sum(og_area)) %>%
  left_join(du9_og_pa_recovery, by = c('critical_habitat_type', 'priority_zone_type')) %>%
  mutate(unprotected_area_recovery = og_area - og_pa_area) %>%
  group_by(critical_habitat_type) %>%
  mutate(unprotected_by_hab_type = sum(unprotected_area_recovery))

write_csv(du9_og_recovery, 'out/du9/du9-og_habitat-type_recovery-herds.csv')

du9_og_pa_all <- og_pa_du9 %>%
  mutate(og_pa_area = st_area(.),
         og_pa_area = as.numeric(set_units(og_pa_area, ha))) %>%
  st_drop_geometry() %>%
  #filter(herd %in% priority_herds) %>%
  group_by(critical_habitat_type, priority_zone_type) %>%
  summarise(og_pa_area = sum(og_pa_area))

#priority og in area
du9_og_all <- og_du9 %>%
  mutate(og_area = st_area(.),
         og_area = as.numeric(set_units(og_area, ha))) %>%
  st_drop_geometry() %>%
  #filter(herd %in% priority_herds) %>%
  group_by(critical_habitat_type, priority_zone_type) %>%
  summarise(og_area = sum(og_area)) %>%
  left_join(du9_og_pa_all, by = c('critical_habitat_type', 'priority_zone_type')) %>%
  mutate(unprotected_area_all = og_area - og_pa_area) %>%
  group_by(critical_habitat_type) %>%
  mutate(unprotected_by_hab_type = sum(unprotected_area_all))

write_csv(du9_og_all, 'out/du9/du9-og_habitat-type_all-herds.csv')


tar_load(c(og_5_du9, og_5_pa_du9))

priority_herds <- c("Barkerville", "Hart Ranges", "North Cariboo", "Wells Gray North", "Wells Gray South",
                    "Groundhog", "Columbia North", "Central Selkirks")

du9_og_5_pa_recovery <- og_5_pa_du9 %>%
  mutate(og_pa_area = st_area(.),
         og_pa_area = as.numeric(set_units(og_pa_area, ha))) %>%
  st_drop_geometry() %>%
  filter(herd %in% priority_herds) %>%
  group_by(critical_habitat_type, priority_zone_type) %>%
  summarise(og_5_pa_area = sum(og_pa_area))

#priority og in area
du9_og_5_recovery <- og_5_du9 %>%
  mutate(og_area = st_area(.),
         og_area = as.numeric(set_units(og_area, ha))) %>%
  st_drop_geometry() %>%
  filter(herd %in% priority_herds) %>%
  group_by(critical_habitat_type, priority_zone_type) %>%
  summarise(og_5_area = sum(og_area)) %>%
  left_join(du9_og_5_pa_recovery, by = c('critical_habitat_type', 'priority_zone_type')) %>%
  mutate(unprotected_area_5_recovery = og_5_area - og_5_pa_area) %>%
  group_by(critical_habitat_type) %>%
  mutate(unprotected_by_hab_type = sum(unprotected_area_5_recovery))

write_csv(du9_og_5_recovery, 'out/du9/du9-5-og_habitat-type_recovery-herds.csv')

du9_og_5_pa <- og_5_pa_du9 %>%
  mutate(og_pa_area = st_area(.),
         og_pa_area = as.numeric(set_units(og_pa_area, ha))) %>%
  st_drop_geometry() %>%
  #filter(herd %in% priority_herds) %>%
  group_by(critical_habitat_type, priority_zone_type) %>%
  summarise(og_5_pa_area = sum(og_pa_area))

#priority og in area
du9_og_5 <- og_5_du9 %>%
  mutate(og_area = st_area(.),
         og_area = as.numeric(set_units(og_area, ha))) %>%
  st_drop_geometry() %>%
  #filter(herd %in% priority_herds) %>%
  group_by(critical_habitat_type, priority_zone_type) %>%
  summarise(og_5_area = sum(og_area)) %>%
  left_join(du9_og_5_pa, by = c('critical_habitat_type', 'priority_zone_type')) %>%
  mutate(unprotected_area_5 = og_5_area - og_5_pa_area) %>%
  group_by(critical_habitat_type) %>%
  mutate(unprotected_by_hab_type = sum(unprotected_area_5))

write_csv(du9_og_5, 'out/du9/du9-5-og_habitat-type_all-herds.csv')


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

du9_pa <- pa_du9 %>%
  mutate(caribou_pa_area = st_area(.),
         caribou_pa_area = as.numeric(set_units(caribou_pa_area, ha))) %>%
  st_drop_geometry() %>%
  group_by(pa_type, herd, critical_habitat_type, priority_zone_type) %>%
  summarise(caribou_pa_area = sum(caribou_pa_area)) %>%
  pivot_wider(names_from=pa_type, values_from=caribou_pa_area)





order = c("High", "Medium", "Low", "Other")


#du9 all area
du9_area <- du9 %>%
  mutate(caribou_area = st_area(.),
         caribou_area = as.numeric(set_units(caribou_area, ha))) %>%
  st_drop_geometry() %>%
  group_by(herd, critical_habitat_type, priority_zone_type) %>%
  summarise(du9_area = sum(caribou_area)) %>%
  left_join(du9_pa, by= c('herd', 'critical_habitat_type', 'priority_zone_type')) %>%
  mutate(across(where(is.numeric), ~round(.x, digits=1))) %>%
  mutate(across(where(is.numeric), ~replace_na(.x, 0)),
         priority_zone_type = factor(priority_zone_type, levels=order)) %>%
  arrange(herd, critical_habitat_type, priority_zone_type) %>%
  relocate(ppa, .before=oecm)

write_csv(du9_area, 'out/du9/ppa-oecm-totals-du9.csv')






# du9_uwr <- des_lands_du9_vis %>%
#   mutate(proposed_cons_area= st_area(.),
#          proposed_cons_area = as.numeric(set_units(proposed_cons_area, ha))) %>%
#   st_drop_geometry() %>%
#   group_by(designation) %>%
#   summarise(des_area_type = sum(proposed_cons_area)) %>%
#   filter(designation == "uwr_conditional_harvest"|designation == "uwr_no_harvest")%>%
#   pivot_wider(names_from=designation, values_from=des_area_type)
# #write_sf(des_lands_du9_vis, "out/des_lands_du9_vis.gpkg")
##


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


## Designated Lands --------------------------------------------------------


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


# ******************************
# PPA & OECM Analysis -----------------------------------------------------
# ******************************

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

# ******************************
# CH Analysis -----------------------------------------------------
# ******************************


tar_load(c(du9_ch, du9_ch_des, du9_ch_pa, du9_ch_flat_pa, du9_ch_flat))


order = c("High", "Medium", "Low", "Other")

ch_pa <- du9_ch_pa %>% mutate(ch_pa_area = st_area(.),
                              ch_pa_area = as.numeric(set_units(ch_pa_area, ha))) %>%
  st_drop_geometry() %>%
  filter(common_name_english != "Caribou - Southern Mountain population") %>%
  group_by(herd, critical_habitat_type, priority_zone_type, common_name_english) %>%
  summarise(ch_du9_pa_area = sum(ch_pa_area))

ch <-du9_ch %>%
  mutate(ch_area = st_area(.),
         ch_area = as.numeric(set_units(ch_area, ha))) %>%
  st_drop_geometry() %>%
  filter(common_name_english != "Caribou - Southern Mountain population") %>%
  group_by(herd, critical_habitat_type, priority_zone_type, common_name_english) %>%
  summarise(ch_du9_area = sum(ch_area)) %>%
  left_join(ch_pa, by = c('herd', 'critical_habitat_type', 'priority_zone_type',
                          "common_name_english")) %>%
  mutate(ch_du9_unprotected = ch_du9_area - ch_du9_pa_area,
         percent_unprotected = round(ch_du9_unprotected/ch_du9_area*100),
          priority_zone_type = factor(priority_zone_type, levels=order)) %>%
  arrange(herd, critical_habitat_type, priority_zone_type) %>%
  mutate(across(where(is.numeric), ~replace_na(.x, 0))) %>%
  mutate(across(where(is.numeric), ~round(.x, digits=1)))

write_csv(ch, 'out/du9/ch-summary-du9.csv')

order = c("High", "Medium", "Low", "Other")

ch_pa_zone_type <- du9_ch_pa %>% mutate(ch_pa_area = st_area(.),
                              ch_pa_area = as.numeric(set_units(ch_pa_area, ha))) %>%
  st_drop_geometry() %>%
  filter(common_name_english != "Caribou - Southern Mountain population") %>%
  group_by(critical_habitat_type, priority_zone_type) %>%
  mutate(pa_species = paste0(unique(common_name_english), collapse = ", ")) %>%
  group_by(critical_habitat_type, priority_zone_type, pa_species) %>%
  #group_by(ScientificName, EnglishName, BCList, CDCMappedLocationsPublic, CDCMappedLocationsConfidential, locations) %>%
  summarise(ch_du9_pa_area = sum(ch_pa_area)) %>%
  group_by(critical_habitat_type) %>%
  mutate(ch_du9_pa_total_hab_area = sum(ch_du9_pa_area))

ch_zone_type <-du9_ch %>%
  mutate(ch_area = st_area(.),
         ch_area = as.numeric(set_units(ch_area, ha))) %>%
  st_drop_geometry() %>%
  filter(common_name_english != "Caribou - Southern Mountain population") %>%
  group_by(critical_habitat_type, priority_zone_type) %>%
  mutate(species = paste0(unique(common_name_english), collapse = ", ")) %>%
  group_by(critical_habitat_type, priority_zone_type, species) %>%
  #group_by(ScientificName, EnglishName, BCList, CDCMappedLocationsPublic, CDCMappedLocationsConfidential, locations) %>%
  summarise(ch_du9_area = sum(ch_area)) %>%
  group_by(critical_habitat_type) %>%
  mutate(ch_du9_total_hab_area = sum(ch_du9_area)) %>%
  left_join(ch_pa_zone_type, by = c('critical_habitat_type', 'priority_zone_type')) %>%
  mutate(ch_du9_unprotected = ch_du9_area - ch_du9_pa_area,
         percent_unprotected = round(ch_du9_unprotected/ch_du9_area*100),
         priority_zone_type = factor(priority_zone_type, levels=order)) %>%
  arrange(critical_habitat_type, priority_zone_type) %>%
  mutate(across(where(is.numeric), ~replace_na(.x, 0))) %>%
  mutate(across(where(is.numeric), ~round(.x, digits=1))) %>%
  group_by(critical_habitat_type) %>%
  mutate(ch_du9_total_unprotected_hab_area = sum(ch_du9_unprotected))

write_csv(ch_zone_type, 'out/du9/ch-habitat-type-summary-du9.csv')


ch_pa_zone_type <- du9_ch_pa %>% mutate(ch_pa_area = st_area(.),
                                        ch_pa_area = as.numeric(set_units(ch_pa_area, ha))) %>%
  st_drop_geometry() %>%
  filter(common_name_english != "Caribou - Southern Mountain population") %>%
  group_by(critical_habitat_type, priority_zone_type) %>%
  mutate(pa_species = paste0(unique(common_name_english), collapse = ", ")) %>%
  group_by(critical_habitat_type, priority_zone_type, pa_species) %>%
  #group_by(ScientificName, EnglishName, BCList, CDCMappedLocationsPublic, CDCMappedLocationsConfidential, locations) %>%
  summarise(ch_du9_pa_area = sum(ch_pa_area)) %>%
  group_by(critical_habitat_type) %>%
  mutate(ch_du9_pa_total_hab_area = sum(ch_du9_pa_area))

priority_herds <- c("Barkerville", "Hart Ranges", "North Cariboo", "Wells Gray North", "Wells Gray South",
                    "Groundhog", "Columbia North", "Central Selkirks")

ch_pa_zone_type_recovery_herds <- du9_ch_pa %>% mutate(ch_pa_area = st_area(.),
                                        ch_pa_area = as.numeric(set_units(ch_pa_area, ha))) %>%
  st_drop_geometry() %>%
  filter(herd %in% priority_herds) %>%
  filter(common_name_english != "Caribou - Southern Mountain population") %>%
  group_by(critical_habitat_type, priority_zone_type) %>%
  mutate(pa_species = paste0(unique(common_name_english), collapse = ", ")) %>%
  group_by(critical_habitat_type, priority_zone_type, pa_species) %>%
  #group_by(ScientificName, EnglishName, BCList, CDCMappedLocationsPublic, CDCMappedLocationsConfidential, locations) %>%
  summarise(ch_du9_pa_area = sum(ch_pa_area)) %>%
  group_by(critical_habitat_type) %>%
  mutate(ch_du9_pa_total_hab_area = sum(ch_du9_pa_area))

ch_zone_type_recovery_herds <-du9_ch %>%
  mutate(ch_area = st_area(.),
         ch_area = as.numeric(set_units(ch_area, ha))) %>%
  st_drop_geometry() %>%
  filter(herd %in% priority_herds) %>%
  filter(common_name_english != "Caribou - Southern Mountain population") %>%
  group_by(critical_habitat_type, priority_zone_type) %>%
  mutate(species = paste0(unique(common_name_english), collapse = ", ")) %>%
  group_by(critical_habitat_type, priority_zone_type, species) %>%
  #group_by(ScientificName, EnglishName, BCList, CDCMappedLocationsPublic, CDCMappedLocationsConfidential, locations) %>%
  summarise(ch_du9_area = sum(ch_area)) %>%
  group_by(critical_habitat_type) %>%
  mutate(ch_du9_total_hab_area = sum(ch_du9_area)) %>%
  left_join(ch_pa_zone_type_recovery_herds, by = c('critical_habitat_type', 'priority_zone_type')) %>%
  mutate(ch_du9_unprotected = ch_du9_area - ch_du9_pa_area,
         percent_unprotected = round(ch_du9_unprotected/ch_du9_area*100),
         priority_zone_type = factor(priority_zone_type, levels=order)) %>%
  arrange(critical_habitat_type, priority_zone_type) %>%
  mutate(across(where(is.numeric), ~replace_na(.x, 0))) %>%
  mutate(across(where(is.numeric), ~round(.x, digits=1))) %>%
  group_by(critical_habitat_type) %>%
  mutate(ch_du9_total_unprotected_hab_area = sum(ch_du9_unprotected))

write_csv(ch_zone_type_recovery_herds, 'out/du9/ch-habitat-type-summary_recovery-herds_du9.csv')

write_sf(du9_ch, "out/du9/du9_ch.gpkg")
# ******************************
# CDC Analysis -----------------------------------------------------
# ******************************

tar_load(c(du9_cdc, du9_cdc_pa))

order = c("High", "Medium", "Low", "Other")

du9_cdc_sum <- du9_cdc %>%
  mutate(cdc_area = st_area(.),
         cdc_area = as.numeric(set_units(cdc_area, ha))) %>%
  st_drop_geometry() %>%
  group_by(critical_habitat_type, priority_zone_type, BC_LIST) %>%
  mutate(species = paste0(unique(ENG_NAME), collapse = ", ")) %>%
  ungroup() %>%
  group_by(critical_habitat_type, priority_zone_type, species) %>%
  summarise(cdc_sum = sum(cdc_area),
            n = n()) %>%
  mutate(priority_zone_type = factor(priority_zone_type, levels=order)) %>%
  arrange(critical_habitat_type, priority_zone_type)



# ******************************
# Industrial Activities Summary -----------------------------------------------------
# ******************************

tar_load(c(du9_forest, du9_tenures, du9_harv, du9_mine_potential, du9_land))

order = c("High", "Medium", "Low", "Other")

du9_cutblocks <- du9_forest %>%
  mutate(for_area = st_area(.),
         for_area = as.numeric(set_units(for_area, ha))) %>%
  st_drop_geometry() %>%
  group_by(herd, critical_habitat_type, priority_zone_type, LIFE_CYCLE_STATUS_CODE) %>%
  summarise(for_area_sum = sum(for_area),
            n_cutblock_total = n()) %>%
  mutate(priority_zone_type = factor(priority_zone_type, levels=order)) %>%
  arrange(herd, critical_habitat_type, priority_zone_type)

write_csv(du9_cutblocks, "out/du9_cutblocks.csv")

du9_cutblocks_hab_type <- du9_forest %>%
  mutate(for_area = st_area(.),
         for_area = as.numeric(set_units(for_area, ha))) %>%
  st_drop_geometry() %>%
  group_by(critical_habitat_type, priority_zone_type, LIFE_CYCLE_STATUS_CODE) %>%
  summarise(for_area_sum = sum(for_area),
            n_cutblock_total = n()) %>%
  #mutate(priority_zone_type = factor(priority_zone_type, levels=order)) %>%
  arrange(critical_habitat_type, priority_zone_type)

write_csv(du9_cutblocks_hab_type, "out/du9_cutblocks_habitat-type_all-herds.csv")

du9_cutblocks_hab_type_recovery <- du9_forest %>%
  mutate(for_area = st_area(.),
         for_area = as.numeric(set_units(for_area, ha))) %>%
  st_drop_geometry() %>%
  filter(herd %in% priority_herds) %>%
  group_by(critical_habitat_type, priority_zone_type, LIFE_CYCLE_STATUS_CODE) %>%
  summarise(for_area_sum = sum(for_area),
            n_cutblock_total = n()) %>%
  #mutate(priority_zone_type = factor(priority_zone_type, levels=order)) %>%
  arrange(critical_habitat_type, priority_zone_type)

write_csv(du9_cutblocks_hab_type_recovery, "out/du9_cutblocks_habitat-type_recovery-herds.csv")


du9_cutblock_licensees <- du9_forest %>%
  mutate(for_area = st_area(.),
         for_area = as.numeric(set_units(for_area, ha))) %>%
  st_drop_geometry() %>%
  group_by(herd, critical_habitat_type, priority_zone_type,
           CLIENT_NAME, LIFE_CYCLE_STATUS_CODE) %>%
  summarise(for_licensee_sum = sum(for_area),
            n_cutblocks = n()) %>%
  left_join(du9_cutblocks) %>%
  mutate(perc_of_total = for_licensee_sum/for_area_sum*100) %>%
  mutate(priority_zone_type = factor(priority_zone_type, levels=order)) %>%
  arrange(herd, critical_habitat_type, priority_zone_type)

write_csv(du9_cutblock_licensees, "out/du9_licensee_list.csv")


du9_tenure_sum <- du9_tenures %>%
  mutate(for_area = st_area(.),
         for_area = as.numeric(set_units(for_area, ha))) %>%
  st_drop_geometry() %>%
  group_by(herd, critical_habitat_type, priority_zone_type, CLIENT_NAME, LIFE_CYCLE_STATUS_CODE) %>%
  summarise(for_area_sum = sum(for_area),
            n_cutblocks = n())%>%
  mutate(priority_zone_type = factor(priority_zone_type, levels=order)) %>%
  arrange(herd, critical_habitat_type, priority_zone_type)

write_csv(du9_tenure_sum, "out/du9_managed_license_list.csv")

du9_tenure_summary <- du9_tenures %>%
  mutate(for_area = st_area(.),
         for_area = as.numeric(set_units(for_area, ha))) %>%
  st_drop_geometry() %>%
  group_by(herd, critical_habitat_type, priority_zone_type, LIFE_CYCLE_STATUS_CODE) %>%
  summarise(for_area_sum = sum(for_area),
            n_cutblocks = n())%>%
  mutate(priority_zone_type = factor(priority_zone_type, levels=order)) %>%
  arrange(herd, critical_habitat_type, priority_zone_type)

write_csv(du9_tenure_summary, "out/du9_managed_license_list_summary.csv")


du9_harvest_sum <- du9_harv %>%
  mutate(for_area = st_area(.),
         for_area = as.numeric(set_units(for_area, ha))) %>%
  st_drop_geometry() %>%
  filter(LIFE_CYCLE_STATUS_CODE != "RETIRED") %>%
  group_by(herd, critical_habitat_type, priority_zone_type, CLIENT_NAME, LIFE_CYCLE_STATUS_CODE) %>%
  summarise(harv_area_sum = sum(for_area),
            n_cutblocks = n()) %>%
  left_join(du9_cutblocks) %>%
  mutate(perc_of_total = harv_area_sum/for_area_sum*100) %>%
  mutate(priority_zone_type = factor(priority_zone_type, levels=order)) %>%
  arrange(herd, critical_habitat_type, priority_zone_type)


write_csv(du9_harvest_sum, "out/du9_harv_auth_list.csv")

du9_harvest_all_herd <- du9_harv %>%
  mutate(for_area = st_area(.),
         for_area = as.numeric(set_units(for_area, ha))) %>%
  st_drop_geometry() %>%
  filter(LIFE_CYCLE_STATUS_CODE != "RETIRED") %>%
  group_by(critical_habitat_type, priority_zone_type, LIFE_CYCLE_STATUS_CODE) %>%
  summarise(harv_area_sum = sum(for_area),
            n_cutblocks = n()) %>%
  #left_join(du9_cutblocks_hab_type) %>%
  #mutate(perc_of_total = harv_area_sum/for_area_sum*100) %>%
  #mutate(priority_zone_type = factor(priority_zone_type, levels=order)) %>%
  arrange(critical_habitat_type, priority_zone_type)


write_csv(du9_harvest_all_herd, "out/du9-harv-auth-list_by-habitat-type_all-herds.csv")


du9_harvest_recovery <- du9_harv %>%
  mutate(for_area = st_area(.),
         for_area = as.numeric(set_units(for_area, ha))) %>%
  st_drop_geometry() %>%
  filter(herd %in% priority_herds) %>%
  filter(LIFE_CYCLE_STATUS_CODE != "RETIRED") %>%
  group_by(critical_habitat_type, priority_zone_type, LIFE_CYCLE_STATUS_CODE) %>%
  summarise(harv_area_sum = sum(for_area),
            n_cutblocks = n()) %>%
  #left_join(du9_cutblocks_hab_type) %>%
  #mutate(perc_of_total = harv_area_sum/for_area_sum*100) %>%
  #mutate(priority_zone_type = factor(priority_zone_type, levels=order)) %>%
  arrange(critical_habitat_type, priority_zone_type)


write_csv(du9_harvest_recovery, "out/du9-harv-auth-list_by-habitat-type_recovery.csv")


du9_land_sum <- du9_land %>%
  mutate(land_area = st_area(.),
         land_area = as.numeric(set_units(land_area, ha))) %>%
  st_drop_geometry() %>%
  filter(TENURE_PURPOSE == "COMMERCIAL RECREATION") %>%
  group_by(critical_habitat_type, priority_zone_type, TENURE_STATUS, TENURE_TYPE, TENURE_PURPOSE, TENURE_SUBPURPOSE) %>%
  summarise(land_area_sum = sum(land_area),
            n_tenure_total = n()) %>%
  ungroup() %>%
  group_by(critical_habitat_type, priority_zone_type) %>%
  mutate(n_tenure_by_priority=sum(n_tenure_total))%>%
  #mutate(priority_zone_type = factor(priority_zone_type, levels=order)) %>%
  arrange(critical_habitat_type, priority_zone_type)

write_csv(du9_land_sum, "out/du9-land-tenure_all-herds.csv")

du9_land_sum_recovery <- du9_land %>%
  mutate(land_area = st_area(.),
         land_area = as.numeric(set_units(land_area, ha))) %>%
  st_drop_geometry() %>%
  filter(herd %in% priority_herds) %>%
  filter(TENURE_PURPOSE == "COMMERCIAL RECREATION") %>%
  group_by(critical_habitat_type, priority_zone_type, TENURE_STATUS, TENURE_TYPE, TENURE_PURPOSE, TENURE_SUBPURPOSE) %>%
  summarise(land_area_sum = sum(land_area),
            n_tenure_total = n()) %>%
  ungroup() %>%
  group_by(critical_habitat_type, priority_zone_type) %>%
  mutate(n_tenure_by_priority=sum(n_tenure_total))%>%
  #mutate(priority_zone_type = factor(priority_zone_type, levels=order)) %>%
  arrange(critical_habitat_type, priority_zone_type)

write_csv(du9_land_sum_recovery, "out/du9-land-tenure_recovery-herds.csv")


du9_mineral <- du9_mine_potential %>%
  mutate(mine_area = st_area(.),
         mine_area = as.numeric(set_units(mine_area, ha))) %>%
  st_drop_geometry() %>%
  group_by(herd, critical_habitat_type, priority_zone_type, OWNER_NAME) %>%
  summarise(mine_area_sum = sum(mine_area),
            n_claim = n()) %>%
  ungroup() %>%
  group_by(herd, critical_habitat_type, priority_zone_type) %>%
  mutate(overall_area = sum(mine_area_sum),
         n_claims_overall=sum(n_claim)) %>%
    mutate(priority_zone_type = factor(priority_zone_type, levels=order)) %>%
    arrange(herd, critical_habitat_type, priority_zone_type)

write_csv(du9_mineral, "out/du9_mineral_claims.csv")


# PIP areas

tar_load(c(pip_du9))

du9_pip_summary <- pip_du9 %>%
  mutate(pip_area = st_area(.),
         pip_area = as.numeric(set_units(pip_area, ha))) %>%
  st_drop_geometry() %>%
  group_by(CONTACT_ORGANIZATION_NAME, CNSLTN_AREA_NAME) %>%
  summarise(area = sum(pip_area))

# ******************************
# Watershed Assessment  -----------------------------------------------------
# ******************************

tar_load(c(du9_cef, du9_tfl, du9_tsa))


priority_herds <- c("Barkerville", "Hart Ranges", "North Cariboo", "Wells Gray North", "Wells Gray South",
                    "Groundhog", "Columbia North", "Central Selkirks")

#priority og in protected area
cef_recovery <- du9_cef %>%
  mutate(watershed_area = st_area(.),
         watershed_area = as.numeric(set_units(watershed_area, ha))) %>%
  st_drop_geometry() %>%
  filter(herd %in% priority_herds) %>%
  filter(priority_zone_type == "High") %>%
  group_by(assessment_unit_source_id, .drop=FALSE) %>%
  mutate(wshed_caribou_overlap_area = sum(watershed_area))

write_csv(cef_recovery, "out/du9/cef_watersheds_du9-high-priority.csv")

wshed_list <- unique(du9_cef$assessment_unit_source_id)

saveRDS(wshed_list, "wshed_list.RDS")


cef_tfl <- du9_tfl %>%
  mutate(tfl_area = st_area(.),
         tfl_area = as.numeric(set_units(tfl_area, ha))) %>%
  st_drop_geometry() %>%
  #filter(herd %in% priority_herds) %>%
  #filter(priority_zone_type == "High")
  group_by(assessment_unit_source_id, FOREST_FILE_ID,
           LICENCEE) %>%
  summarise(tfl_area = sum(tfl_area))


cef_tsa <- du9_tsa %>%
  mutate(tsa_area = st_area(.),
         tsa_area = as.numeric(set_units(tsa_area, ha))) %>%
  st_drop_geometry() %>%
  #filter(herd %in% priority_herds) %>%
  #filter(priority_zone_type == "High")
  group_by(assessment_unit_source_id, TSA_NUMBER, TSA_NUMBER_DESCRIPTION) %>%
  summarise(tsa_area = sum(tsa_area))

cef_recovery_combined <- cef_recovery %>%
  left_join(cef_tfl, by='assessment_unit_source_id') %>%
  left_join(cef_tsa, by='assessment_unit_source_id') %>%
  mutate(across(where(is.numeric), ~replace_na(.x, 0))) %>%
  mutate(across(where(is.numeric), ~round(.x, digits=1)))

write_csv(cef_recovery_combined, "out/du9/cef_watersheds_du9-high-priority_recovery-herds.csv")

# ******************************
# Old-growth by TSA/TFL  -----------------------------------------------------
# ******************************


tar_load(c(priority_du9_tfl, priority_du9_tfl_pa, priority_du9_tsa, priority_du9_tsa_pa))

#priority og in protected area
priority_pa_du9_tfl_recovery <- priority_du9_tfl_pa %>%
  mutate(og_pa_area = st_area(.),
         og_pa_area = as.numeric(set_units(og_pa_area, ha))) %>%
  st_drop_geometry() %>%
  filter(herd %in% priority_herds) %>%
  group_by(herd, critical_habitat_type, priority_zone_type,  FOREST_FILE_ID,
           LICENCEE) %>%
  summarise(og_priority_area_pa_by_tfl = sum(og_pa_area))

#priority og in area
priority_du9_tfl_recovery <- priority_du9_tfl %>%
  mutate(og_area = st_area(.),
         og_area = as.numeric(set_units(og_area, ha))) %>%
  st_drop_geometry() %>%
  filter(herd %in% priority_herds) %>%
  group_by(herd, critical_habitat_type, priority_zone_type, FOREST_FILE_ID,
           LICENCEE) %>%
  summarise(og_priority_area_by_tfl = sum(og_area)) %>%
  left_join(priority_pa_du9_tfl_recovery, by = c('herd','critical_habitat_type', 'priority_zone_type',
                                                 'FOREST_FILE_ID', 'LICENCEE')) %>%
  mutate(across(where(is.numeric), ~replace_na(.x, 0))) %>%
  mutate(across(where(is.numeric), ~round(.x, digits=1))) %>%
  mutate(og_unprotected_tfl = og_priority_area_by_tfl - og_priority_area_pa_by_tfl)

write_csv(priority_du9_tfl_recovery, 'out/du9/du9-priority-og_habitat-type_recovery-herds_by-tfl.csv')

high_priority <- priority_du9_tfl_recovery %>%
  filter(priority_zone_type == "High")

write_csv(high_priority, 'out/du9/du9-high-priority-og_habitat-type_recovery-herds_by-tfl.csv')

priority_pa_du9_tfl <- priority_du9_tfl_pa %>%
  mutate(og_pa_area = st_area(.),
         og_pa_area = as.numeric(set_units(og_pa_area, ha))) %>%
  st_drop_geometry() %>%
  #filter(herd %in% priority_herds) %>%
  group_by(herd, critical_habitat_type, priority_zone_type, FOREST_FILE_ID,
           LICENCEE) %>%
  summarise(og_priority_area_pa_by_tfl = sum(og_pa_area))

#priority og in area
priority_du9_tfl_all <- priority_du9_tfl %>%
  mutate(og_area = st_area(.),
         og_area = as.numeric(set_units(og_area, ha))) %>%
  st_drop_geometry() %>%
  #filter(herd %in% priority_herds) %>%
  group_by(herd, critical_habitat_type, priority_zone_type, FOREST_FILE_ID,
           LICENCEE) %>%
  summarise(og_priority_area_by_tfl = sum(og_area)) %>%
  left_join(priority_pa_du9_tfl, by = c('herd','critical_habitat_type', 'priority_zone_type',
                                                 'FOREST_FILE_ID', 'LICENCEE')) %>%
  mutate(across(where(is.numeric), ~replace_na(.x, 0))) %>%
  mutate(across(where(is.numeric), ~round(.x, digits=1))) %>%
  mutate(og_unprotected_tfl = og_priority_area_by_tfl - og_priority_area_pa_by_tfl)

write_csv(priority_du9_tfl, 'out/du9/du9-priority-og_habitat-type_all-herds_by-tfl.csv')

high_priority_all <- priority_du9_tfl %>%
  filter(priority_zone_type == "High")

write_csv(high_priority_all, 'out/du9/du9-high-priority-og_habitat-type_all-herds_by-tfl.csv')


## TSA ---------------------------------------------------------------------



priority_pa_du9_tsa_recovery <- priority_du9_tsa_pa %>%
  mutate(og_pa_area = st_area(.),
         og_pa_area = as.numeric(set_units(og_pa_area, ha))) %>%
  st_drop_geometry() %>%
  filter(herd %in% priority_herds) %>%
  group_by(herd, critical_habitat_type, priority_zone_type) %>%
  mutate(TSA_Name = paste0(unique(TSA_NUMBER_DESCRIPTION), collapse = ", ")) %>%
  group_by(herd, critical_habitat_type, priority_zone_type, TSA_Name) %>%
  summarise(og_priority_area_pa_by_tsa = sum(og_pa_area))

#priority og in area
priority_du9_tsa_recovery <- priority_du9_tsa %>%
  mutate(og_area = st_area(.),
         og_area = as.numeric(set_units(og_area, ha))) %>%
  st_drop_geometry() %>%
  filter(herd %in% priority_herds) %>%
  group_by(herd, critical_habitat_type, priority_zone_type) %>%
  mutate(TSA_Name = paste0(unique(TSA_NUMBER_DESCRIPTION), collapse = ", ")) %>%
  group_by(herd, critical_habitat_type, priority_zone_type, TSA_Name) %>%
  summarise(og_priority_area_by_tsa = sum(og_area)) %>%
  left_join(priority_pa_du9_tsa_recovery, by = c('herd', 'critical_habitat_type', 'priority_zone_type', 'TSA_Name')) %>%
  mutate(across(where(is.numeric), ~replace_na(.x, 0))) %>%
  mutate(across(where(is.numeric), ~round(.x, digits=1))) %>%
  mutate(og_unprotected_tsa = og_priority_area_by_tsa - og_priority_area_pa_by_tsa) %>%
  left_join(priority_du9_tfl_recovery, by = c('herd', 'critical_habitat_type', 'priority_zone_type')) %>%
  mutate(across(where(is.numeric), ~replace_na(.x, 0))) %>%
  mutate(across(where(is.numeric), ~round(.x, digits=1)))

write_csv(priority_du9_tsa_recovery, 'out/du9/du9-priority-og_habitat-type_recovery-herds_by-tsa-and-tfl.csv')

high_priority_tsa <- priority_du9_tsa_recovery %>%
  filter(priority_zone_type == "High")

write_csv(high_priority_tsa, 'out/du9/du9-high-priority-og_habitat-type_recovery-herds_by-tsa.csv')

priority_pa_du9_tsa <- priority_du9_tsa_pa %>%
  mutate(og_pa_area = st_area(.),
         og_pa_area = as.numeric(set_units(og_pa_area, ha))) %>%
  st_drop_geometry() %>%
  #filter(herd %in% priority_herds) %>%
  group_by(herd, critical_habitat_type, priority_zone_type) %>%
  mutate(TSA_Name = paste0(unique(TSA_NUMBER_DESCRIPTION), collapse = ", ")) %>%
  group_by(herd, critical_habitat_type, priority_zone_type, TSA_Name) %>%
  summarise(og_priority_area_pa_by_tsa = sum(og_pa_area))

#priority og in area
priority_du9_tsa_all <- priority_du9_tsa %>%
  mutate(og_area = st_area(.),
         og_area = as.numeric(set_units(og_area, ha))) %>%
  st_drop_geometry() %>%
  #filter(herd %in% priority_herds) %>%
  group_by(herd, critical_habitat_type, priority_zone_type) %>%
  mutate(TSA_Name = paste0(unique(TSA_NUMBER_DESCRIPTION), collapse = ", ")) %>%
  group_by(herd, critical_habitat_type, priority_zone_type, TSA_Name) %>%
  summarise(og_priority_area_by_tsa = sum(og_area)) %>%
  left_join(priority_pa_du9_tsa, by = c('herd', 'critical_habitat_type', 'priority_zone_type',
                                                 'TSA_Name')) %>%
  left_join(priority_du9_tfl_all, by = c('herd', 'critical_habitat_type', 'priority_zone_type')) %>%
  mutate(across(where(is.numeric), ~replace_na(.x, 0))) %>%
  mutate(across(where(is.numeric), ~round(.x, digits=1))) %>%
  mutate(og_unprotected_tsa = og_priority_area_by_tsa - og_priority_area_pa_by_tsa)

write_csv(priority_du9_tsa, 'out/du9/du9-priority-og_habitat-type_all-herds_by-tsa.csv')

high_priority_tsa_all <- priority_du9_tsa %>%
  filter(priority_zone_type == "High")

write_csv(high_priority_tsa_all, 'out/du9/du9-high-priority-og_habitat-type_all-herds_by-tsa.csv')
