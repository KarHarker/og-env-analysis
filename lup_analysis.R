tar_load(c(og_lup, og_faib_lup, lup_clipped, og_lup_priority, og_pa_lup_priority))


lup_area<-lup_clipped%>%
  mutate(lup_area=st_area(.),
         lup_area =as.numeric(set_units(lup_area, ha))) %>%
  st_set_geometry(NULL) %>%
  summarise(overall_lup_area = sum(lup_area))

overall_og_lup <- og_lup %>%
  mutate(og_lup_area=st_area(.),
         og_lup_area =as.numeric(set_units(og_lup_area, ha))) %>%
  st_set_geometry(NULL) %>%
  summarise(overall_og_lup_area = sum(og_lup_area))

overall_priority <- og_lup_priority%>%
  mutate(faib_lup_area=st_area(.),
         faib_lup_area =as.numeric(set_units(faib_lup_area, ha))) %>%
  st_set_geometry(NULL) %>%
  summarise(overall_priority_lup_area = sum(faib_lup_area))

overall_priority_prot <- og_pa_lup_priority %>%
  mutate(faib_lup_area=st_area(.),
         faib_lup_area =as.numeric(set_units(faib_lup_area, ha))) %>%
  st_set_geometry(NULL) %>%
  summarise(overall_priority_pa_lup_area = sum(faib_lup_area))


output<-bind_cols(lup_area, overall_og_lup, overall_priority, overall_priority_prot) %>%
  mutate(og_lup_percentage = overall_og_lup_area/overall_lup_area*100,
         unprot = overall_priority_lup_area- overall_priority_pa_lup_area,
         at_risk_mod = unprot/overall_og_lup_area*100,
         at_risk_lup = unprot/overall_lup_area*100)

write_csv(output, "og_lup_overlap_terrestrial_summary.csv")

lup_area<-lup_data%>%
  mutate(lup_area=st_area(.),
         lup_area =as.numeric(set_units(lup_area, ha))) %>%
  st_set_geometry(NULL) %>%
  group_by(FLNR_LUP_NAME) %>%
  summarise(overall_lup_area = sum(lup_area))

overall_og_lup <- og_lup %>%
  mutate(og_lup_area=st_area(.),
         og_lup_area =as.numeric(set_units(og_lup_area, ha))) %>%
  st_set_geometry(NULL) %>%
  group_by(FLNR_LUP_NAME) %>%
  summarise(overall_og_lup_area = sum(og_lup_area))

overall_faib <- faib_ch %>%
  mutate(faib_lup_area=st_area(.),
         faib_lup_area =as.numeric(set_units(faib_lup_area, ha))) %>%
  st_set_geometry(NULL) %>%
  group_by(FLNR_LUP_NAME) %>%
  summarise(overall_faib_lup_area = sum(faib_lup_area)) %>%
  left_join(lup_area)%>%
  left_join(overall_og_lup) %>%
  mutate(og_ch_percentage = overall_og_species_area/overall_area*100,
         at_risk_mod = overall_faib/og_species_area*100,
         overall_unprot = (overall_og_species_area-overall_og_ch_prot)/overall_species_area*100)

tar_load(c(heat_lup, lup_clipped, heat_future, future_cons, heat_iwb, iwb, lup_pa))

heat_lup_sum <- heat_lup %>%
  mutate(lup_area=st_area(.),
         lup_area =as.numeric(set_units(lup_area, ha))) %>%
  st_set_geometry(NULL) %>%
  unite(lup_label, c(name_1, flnr_lup_name), remove=FALSE) %>%
  filter(Sum >= 4) %>%
  group_by(lup_label) %>%
  summarise(heat_map_lup_area = sum(lup_area))

lup_area<-lup_pa%>%
  mutate(lup_area=st_area(.),
         lup_area =as.numeric(set_units(lup_area, ha))) %>%
  st_set_geometry(NULL) %>%
  unite(lup_label, c(name_1, flnr_lup_name), remove=FALSE) %>%
  group_by(lup_label) %>%
  summarise(overall_lup_area_pa = sum(lup_area))

lup_area_pa<-lup_clipped%>%
  mutate(lup_area=st_area(.),
         lup_area =as.numeric(set_units(lup_area, ha))) %>%
  st_set_geometry(NULL) %>%
  unite(lup_label, c(name_1, flnr_lup_name), remove=FALSE) %>%
  group_by(lup_label) %>%
  summarise(overall_lup_area = sum(lup_area))

heat_lup_summary <- heat_lup_sum %>%
  left_join(lup_area) %>%
  left_join(lup_area_pa) %>%
  mutate(perc_high_overlap = heat_map_lup_area/(overall_lup_area-overall_lup_area_pa)*100) %>%
  mutate(sum_heat_map = sum(heat_map_lup_area),
         perc_of_total = sum_heat_map/1895852.226*100,
         sum_total_lup = sum(overall_lup_area),
         perc_lup = sum_total_lup/94806046*100)

tar_load(c(future_cons, heat_future))


heat_future_sum <- heat_future %>%
  mutate(lup_area=st_area(.),
         lup_area =as.numeric(set_units(lup_area, ha))) %>%
  st_set_geometry(NULL) %>%
  filter(Sum >= 4) %>%
  group_by(project_name) %>%
  summarise(heat_map_future_area = sum(lup_area))

future_area<-future_cons %>%
  mutate(lup_area=st_area(.),
         lup_area =as.numeric(set_units(lup_area, ha))) %>%
  st_set_geometry(NULL) %>%
  group_by(project_name) %>%
  summarise(overall_future_area = sum(lup_area))

future_area_pa<-pcl_pa %>%
  mutate(lup_area=st_area(.),
         lup_area =as.numeric(set_units(lup_area, ha))) %>%
  st_set_geometry(NULL) %>%
  group_by(project_name) %>%
  summarise(overall_future_area_pa = sum(lup_area))

heat_future_summary <- heat_future_sum %>%
  left_join(future_area) %>%
  left_join(future_area_pa) %>%
  mutate(perc_high_overlap = heat_map_future_area/(overall_future_area-overall_future_area_pa)*100) %>%
  mutate(sum_heat_map = sum(heat_map_future_area),
         perc_of_total = sum_heat_map/1895852.226*100,
         sum_total_future = sum(sum_heat_map),
         perc_future = sum_total_future/94806046*100)

write_csv(heat_future_summary, "out/heat_future_summary.csv")

mean(heat_future_summary$perc_high_overlap)
mean(heat_lup_summary$perc_high_overlap)

tar_load(c(heat_iwb, int_wet_belt, iwb_pa))

heat_iwb_sum <- heat_iwb %>%
  mutate(iwb_area=st_area(.),
         iwb_area =as.numeric(set_units(iwb_area, ha))) %>%
  st_set_geometry(NULL) %>%
  filter(Sum >= 4) %>%
  summarise(heat_map_iwb_area = sum(iwb_area))

iwb_sum_pa <- iwb_pa %>%
  mutate(iwb_area=st_area(.),
         iwb_area =as.numeric(set_units(iwb_area, ha))) %>%
  st_set_geometry(NULL) %>%
  summarise(iwb_area_pa = sum(iwb_area))

iwb_sum <- int_wet_belt %>%
  mutate(iwb_area=st_area(.),
         iwb_area =as.numeric(set_units(iwb_area, ha))) %>%
  st_set_geometry(NULL) %>%
  summarise(iwb_area = sum(iwb_area))

iwb_summary <- bind_cols(heat_iwb_sum, iwb_sum_pa, iwb_sum) %>%
  mutate(perc_high_overlap = heat_map_iwb_area/(iwb_area-iwb_sum_pa)*100)

tar_load(c(heat_gcr, caribou_cons_area))

heat_gcr_sum <- heat_gcr %>%
  mutate(iwb_area=st_area(.),
         iwb_area =as.numeric(set_units(iwb_area, ha))) %>%
  st_set_geometry(NULL) %>%
  filter(Sum >= 4) %>%
  summarise(heat_gcr_area = sum(iwb_area))

heat_gcr_zone <- caribou_cons_area %>%
  mutate(iwb_area=st_area(.),
         iwb_area =as.numeric(set_units(iwb_area, ha))) %>%
  st_set_geometry(NULL) %>%
  filter(Sum > 0) %>%
  summarise(heat_gcr_total = sum(iwb_area))

gcr_sum <- heat_gcr %>%
  mutate(iwb_area=st_area(.),
         iwb_area =as.numeric(set_units(iwb_area, ha))) %>%
  st_set_geometry(NULL) %>%
  group_by(Sum) %>%
  summarise(gcr_area = sum(iwb_area))

gcr_summary <- bind_cols(heat_gcr_sum, heat_gcr_zone) %>%
  mutate(perc_high_overlap = heat_gcr_area/heat_gcr_total *100)



tar_load(heat_map)

heat_summary  <- heat_map %>%
  mutate(iwb_area=st_area(.),
         iwb_area =as.numeric(set_units(iwb_area, ha))) %>%
  st_set_geometry(NULL) %>%
  group_by(Sum) %>%
  summarise(iwb_area = sum(iwb_area)) %>%
  ungroup() %>%
  mutate(total_area = sum(iwb_area),
         perc = iwb_area/total_area*100)
write_csv(heat_summary, "out/heat_summary.csv")
