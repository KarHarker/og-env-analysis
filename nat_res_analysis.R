tar_load(c(og_by_district, ch_by_district, faib_by_district,
           og_ch_by_district, faib_og_ch, prot_ch_by_district,
           prot_by_district_priority, og_by_district_priority, prot_by_district))


nat_res_area <- bcmaps::nr_districts() %>%
  mutate(district_area = st_area(.),
         district_area =as.numeric(set_units(district_area, ha))) %>%
  st_set_geometry(NULL) %>%
  rename_all(tolower) %>%
  group_by(district_name) %>%
  summarise(district_area = sum(district_area)) %>%
  ungroup()

#write_csv(nat_res_area, "nat_res_area.csv")

og_district <- og_by_district %>%
  mutate(og_district_area = st_area(.),
         og_district_area =as.numeric(set_units(og_district_area, ha))) %>%
  st_set_geometry(NULL) %>%
  rename_all(tolower) %>%
  group_by(district_name) %>%
  summarise(og_district_area = sum(og_district_area)) %>%
  ungroup()

priority_og <- og_by_district_priority  %>%
  mutate(priority_og_area = st_area(.),
         priority_og_area =as.numeric(set_units(priority_og_area, ha))) %>%
  st_set_geometry(NULL) %>%
  rename_all(tolower) %>%
  group_by(district_name) %>%
  summarise(priority_og_area = sum(priority_og_area)) %>%
  ungroup()

prot_dist <- prot_by_district %>%
  mutate(og_prot_district_area = st_area(.),
         og_prot_district_area =as.numeric(set_units(og_prot_district_area, ha))) %>%
  st_set_geometry(NULL) %>%
  rename_all(tolower) %>%
  group_by(district_name) %>%
  summarise(og_prot_district_area = sum(og_prot_district_area))

priority_prot  <- prot_by_district_priority %>%
  mutate(priority_og_prot_district_area = st_area(.),
         priority_og_prot_district_area =as.numeric(set_units(priority_og_prot_district_area, ha))) %>%
  st_set_geometry(NULL) %>%
  rename_all(tolower) %>%
  group_by(district_name) %>%
  summarise(priority_og_prot_district_area = sum(priority_og_prot_district_area))

total <- nat_res_area %>%
  left_join(og_district) %>%
  left_join(priority_og) %>%
  left_join(prot_dist) %>%
  left_join(priority_prot) %>%
  mutate(og_dist_all= og_district_area/district_area*100,
         priority_dist_all = priority_og_area/district_area*100,
         unprot_all_og = og_district_area - og_prot_district_area,
         unprot_priority_og = priority_og_area - priority_og_prot_district_area,
         perc_unprot_all = unprot_all_og/og_district_area*100,
         perc_unprot_priority = unprot_priority_og/og_district_area*100
         )

write_csv(total, "out/og-prot_nat-res-area.csv")



#### CH by district - extra

og_ch <- og_ch_by_district %>%
  mutate(og_species_area = st_area(.),
         og_species_area  =as.numeric(set_units(og_species_area, ha))) %>%
  st_set_geometry(NULL) %>%
  rename_all(tolower) %>%
  group_by(scientific_name, common_name_english, cosewic_status, schedule_status, sara_schedule, district_name) %>%
  summarise(og_species_area = sum(og_species_area)) %>%
  ungroup()

og_ch_prot <- prot_ch_by_district%>%
  mutate(og_prot_species_area=st_area(.),
         og_prot_species_area =as.numeric(set_units(og_prot_species_area, ha))) %>%
  st_set_geometry(NULL) %>%
  rename_all(tolower) %>%
  group_by(scientific_name, common_name_english, cosewic_status, schedule_status, sara_schedule, district_name) %>%
  summarise(og_prot_species_area = sum(og_prot_species_area))

og_ch_faib <- faib_og_ch%>%
  mutate(og_faib_species_area=st_area(.),
         og_faib_species_area =as.numeric(set_units(og_faib_species_area, ha))) %>%
  st_set_geometry(NULL) %>%
  rename_all(tolower) %>%
  group_by(scientific_name, common_name_english, cosewic_status, schedule_status, sara_schedule, district_name) %>%
  summarise(og_faib_species_area = sum(og_faib_species_area))


output2<-ch_by_district %>%
  mutate(species_area=st_area(.),
         species_area =as.numeric(set_units(species_area, ha)),
         species_area = replace_na(species_area,0)) %>%
  st_set_geometry(NULL) %>%
  rename_all(tolower) %>%
  group_by(scientific_name, common_name_english, cosewic_status, schedule_status, sara_schedule, district_name) %>%
  summarise(species_area = sum(species_area)) %>%
  left_join(og_ch, by=c("scientific_name", "common_name_english", "cosewic_status",
                             "schedule_status", "sara_schedule", "district_name")) %>%
  left_join(og_ch_prot, by=c("scientific_name", "common_name_english", "cosewic_status",
                             "schedule_status", "sara_schedule", "district_name")) %>%
  left_join(og_ch_faib, by=c("scientific_name", "common_name_english", "cosewic_status",
                             "schedule_status", "sara_schedule", "district_name")) %>%
  left_join(nat_res_area, by = "district_name") %>%
  mutate_if(is.numeric, ~replace(., is.na(.),0)) %>%
  mutate(og_ch_perc = og_species_area/species_area*100,
         prot_ch_perc = og_prot_species_area/og_species_area *100,
         unprotected_area = og_species_area - og_prot_species_area,
         ch_at_risk_perc = og_faib_species_area/og_species_area *100,
         ch_unprot_perc = unprotected_area/og_species_area *100,
         og_ch_index = ch_at_risk_perc/og_ch_perc
  )

write_csv(output2, "ch_species_summary_by_resource-district.csv")


summary_district_all <- output2 %>%
  group_by(district_name) %>%
  summarise(all_species_area = sum(species_area),
            all_og_species_area = sum(og_species_area),
            all_species_prot_area = sum(og_prot_species_area),
            all_species_unprot_area = all_og_species_area - all_species_prot_area,
            all_at_risk_species_area = sum(og_faib_species_area)) %>%
  mutate(og_dist_perc = all_og_species_area/all_species_area*100,
            prot_dist_perc = all_species_prot_area/all_og_species_area*100,
            at_risk_dist_perc = all_at_risk_species_area/all_og_species_area*100,
            og_index_dist = at_risk_dist_perc/og_dist_perc)

write_csv(summary_district_all, "ch_by_district_all.csv")

og_scientific_name <- c("Marmota vancouverensis", "Limnanthes macounii",
                        "Aegolius acadicus brooksi", "Cephalanthera austiniae",
                        "Hemphillia dromedarius", "Prophysaon coeruleum",
                        "Sphyrapicus thyroideus nataliae", "Brachyramphus marmoratus",
                        "Collema coniophilum", "Melanerpes lewis", "Rangifer tarandus",
                        "Accipiter gentilis laingi"
)

summary_district_og_dependent <- output2 %>%
  filter(scientific_name %in% og_scientific_name) %>%
  group_by(district_name) %>%
  summarise(dep_species_area = sum(species_area),
            dep_og_species_area = sum(og_species_area),
            dep_species_prot_area = sum(og_prot_species_area),
            dep_species_unprot_area = dep_og_species_area-dep_species_prot_area,
            dep_at_risk_species_area = sum(og_faib_species_area)) %>%
  mutate(dep_og_dist_perc = dep_og_species_area/dep_species_area*100,
         dep_prot_dist_perc = dep_species_prot_area/dep_og_species_area*100,
         dep_unprot_dist_perc = dep_species_unprot_area/dep_og_species_area*100,
         dep_at_risk_dist_perc = dep_at_risk_species_area/dep_og_species_area*100,
         dep_og_index_dist = dep_at_risk_dist_perc/dep_og_dist_perc)

write_csv(summary_district_og_dependent, "ch_by_district_og_dependent.csv")


