tar_load(c(og_ch, prot_og_ch, priority_og_ch, priority_prot_og_ch, ch_data))

og_ch_overall <- og_ch %>%
  mutate(og_species_area = st_area(.),
         og_species_area  =as.numeric(set_units(og_species_area, ha))) %>%
  st_set_geometry(NULL) %>%
  rename_all(tolower) %>%
  group_by(scientific_name, common_name_english, cosewic_status, schedule_status, sara_schedule) %>%
  summarise(og_species_area = sum(og_species_area)) %>%
  ungroup()

og_ch_prot <- prot_og_ch %>%
  mutate(og_prot_species_area=st_area(.),
         og_prot_species_area =as.numeric(set_units(og_prot_species_area, ha))) %>%
  st_set_geometry(NULL) %>%
  rename_all(tolower) %>%
  group_by(scientific_name, common_name_english, cosewic_status, schedule_status, sara_schedule) %>%
  summarise(og_prot_species_area = sum(og_prot_species_area))

og_ch_priority <- priority_og_ch %>%
  mutate(og_faib_species_area=st_area(.),
         og_faib_species_area =as.numeric(set_units(og_faib_species_area, ha))) %>%
  st_set_geometry(NULL) %>%
  rename_all(tolower) %>%
  group_by(scientific_name, common_name_english, cosewic_status, schedule_status, sara_schedule) %>%
  summarise(og_ch_priority_area = sum(og_faib_species_area))

og_ch_priority_prot <- priority_prot_og_ch %>%
  mutate(species_area=st_area(.),
         species_area =as.numeric(set_units(species_area, ha)),
         species_area = replace_na(species_area,0)) %>%
  st_set_geometry(NULL) %>%
  rename_all(tolower) %>%
  group_by(scientific_name, common_name_english, cosewic_status, schedule_status, sara_schedule) %>%
  summarise(og_ch_priority_area_prot = sum(species_area))

ch_species <- ch_data %>%
  mutate(species_area=st_area(.),
         species_area =as.numeric(set_units(species_area, ha)),
         species_area = replace_na(species_area,0)) %>%
  st_set_geometry(NULL) %>%
  rename_all(tolower) %>%
  group_by(scientific_name, common_name_english, cosewic_status, schedule_status, sara_schedule) %>%
  summarise(species_area = sum(species_area))

output <- og_ch_overall %>%
  left_join(og_ch_prot, by=c("scientific_name", "common_name_english", "cosewic_status",
                        "schedule_status", "sara_schedule")) %>%
  left_join(og_ch_priority, by=c("scientific_name", "common_name_english", "cosewic_status",
                             "schedule_status", "sara_schedule")) %>%
  left_join(og_ch_priority_prot, by=c("scientific_name", "common_name_english", "cosewic_status",
                             "schedule_status", "sara_schedule")) %>%
  left_join(ch_species, by=c("scientific_name", "common_name_english", "cosewic_status",
                                      "schedule_status", "sara_schedule")) %>%
  mutate_if(is.numeric, ~replace(., is.na(.),0)) %>%
  mutate(og_ch_perc = og_species_area/species_area*100,
         prot_ch_perc = og_prot_species_area/og_species_area *100,
         unprotected_area = og_species_area - og_prot_species_area,
         ch_all_perc = unprotected_area/og_species_area *100,
         #
         og_ch_priority_perc = og_species_area/species_area*100,
         prot_ch_perc = og_ch_priority_area_prot/og_ch_priority_area *100,
         unprotected_priority_area = og_ch_priority_area - og_ch_priority_area_prot,
         ch_priority_perc = unprotected_priority_area/og_ch_priority_area *100
  )

write_csv(output, "out/ch_species_summary_by_province.csv")



og_ch_sum <- og_ch %>%
  mutate(og_species_area = st_area(.),
         og_species_area  =as.numeric(set_units(og_species_area, ha))) %>%
  st_set_geometry(NULL) %>%
  rename_all(tolower) %>%
  summarise(og_ch_area = sum(og_species_area)) %>%
  ungroup()

og_ch_prot_sum <- prot_og_ch %>%
  mutate(og_prot_species_area=st_area(.),
         og_prot_species_area =as.numeric(set_units(og_prot_species_area, ha))) %>%
  st_set_geometry(NULL) %>%
  rename_all(tolower) %>%
  summarise(og_prot_area = sum(og_prot_species_area))

og_ch_priority_sum <- priority_og_ch %>%
  mutate(og_faib_species_area=st_area(.),
         og_faib_species_area =as.numeric(set_units(og_faib_species_area, ha))) %>%
  st_set_geometry(NULL) %>%
  rename_all(tolower) %>%
  summarise(og_priority_area = sum(og_faib_species_area))


og_ch_priority_prot_sum <- priority_prot_og_ch %>%
  mutate(species_area=st_area(.),
         species_area =as.numeric(set_units(species_area, ha)),
         species_area = replace_na(species_area,0)) %>%
  st_set_geometry(NULL) %>%
  rename_all(tolower) %>%
  summarise(og_priority_area_prot = sum(species_area)) %>%
  bind_cols(og_ch_sum, og_ch_prot_sum, og_ch_priority_sum) %>%
  mutate_if(is.numeric, ~replace(., is.na(.),0)) %>%
  mutate(og_ch_perc = (og_ch_area/100)/bcmaps::bc_area()*100,
         prot_ch_perc = og_prot_area/og_ch_area *100,
         unprotected_area = og_ch_area - og_prot_area,
         perc_all_unprot = unprotected_area/og_ch_area*100,
         #
         og_priority_ch_perc = (og_priority_area/100)/bcmaps::bc_area()*100,
         prot_priority_ch_perc = og_priority_area_prot/og_priority_area *100,
         unprotected_priority_area = og_priority_area - og_priority_area_prot,
         perc_priority_unprot = og_priority_area_prot/og_priority_area*100,
         #
  )

write_csv(og_ch_priority_prot_sum, "out/ch_totals_full_data.csv")

tar_load(c(og_ch_flat, og_ch_flat_prot, priority_og_ch_flat, priority_og_ch_flat_prot, ch_flat))

ch_total <- ch_flat %>%
  mutate(og_species_area = st_area(.),
         og_species_area  =as.numeric(set_units(og_species_area, ha))) %>%
  st_set_geometry(NULL) %>%
  rename_all(tolower) %>%
  summarise(ch_area = sum(og_species_area)) %>%
  ungroup()

og_ch_sum_flat <- og_ch_flat %>%
  mutate(og_species_area = st_area(.),
         og_species_area  =as.numeric(set_units(og_species_area, ha))) %>%
  st_set_geometry(NULL) %>%
  rename_all(tolower) %>%
  summarise(og_ch_area = sum(og_species_area)) %>%
  ungroup()

og_ch_prot_sum_flat <- og_ch_flat_prot %>%
  mutate(og_prot_species_area=st_area(.),
         og_prot_species_area =as.numeric(set_units(og_prot_species_area, ha))) %>%
  st_set_geometry(NULL) %>%
  rename_all(tolower) %>%
  summarise(og_prot_area = sum(og_prot_species_area))

og_ch_priority_sum_flat <- priority_og_ch_flat %>%
  mutate(og_faib_species_area=st_area(.),
         og_faib_species_area =as.numeric(set_units(og_faib_species_area, ha))) %>%
  st_set_geometry(NULL) %>%
  rename_all(tolower) %>%
  summarise(og_priority_area = sum(og_faib_species_area))


og_ch_priority_prot_sum_flat <- priority_og_ch_flat_prot %>%
  mutate(species_area=st_area(.),
         species_area =as.numeric(set_units(species_area, ha)),
         species_area = replace_na(species_area,0)) %>%
  st_set_geometry(NULL) %>%
  rename_all(tolower) %>%
  summarise(og_priority_area_prot = sum(species_area)) %>%
  bind_cols(og_ch_sum_flat, og_ch_prot_sum_flat, og_ch_priority_sum_flat, ch_total) %>%
  mutate_if(is.numeric, ~replace(., is.na(.),0)) %>%
  mutate(og_ch_perc = (og_ch_area/100)/bcmaps::bc_area()*100,
         prot_ch_perc = og_prot_area/og_ch_area *100,
         unprotected_area = og_ch_area - og_prot_area,
         perc_all_unprot = unprotected_area/og_ch_area*100,
         perc_og_ch = og_ch_area/ch_area*100,
         #
         og_priority_ch_perc = (og_priority_area/100)/bcmaps::bc_area()*100,
         prot_priority_ch_perc = og_priority_area_prot/og_priority_area *100,
         unprotected_priority_area = og_priority_area - og_priority_area_prot,
         perc_priority_unprot = og_priority_area_prot/og_priority_area*100,
         perc_priority_og_ch = og_priority_area/ch_area*100,
         perc_unprot_og_priority = unprotected_priority_area/unprotected_area*100
         #
  )

write_csv(og_ch_priority_prot_sum_flat, "out/ch_totals_flat_data.csv")


tar_load(c(ch_data, og_ch, prot_og_ch, priority_og_ch, priority_prot_og_ch))

og_scientific_name <- c("Marmota vancouverensis", "Limnanthes macounii",
                        "Aegolius acadicus brooksi", "Cephalanthera austiniae",
                        "Hemphillia dromedarius", "Prophysaon coeruleum",
                        "Sphyrapicus thyroideus nataliae", "Brachyramphus marmoratus",
                        "Collema coniophilum", "Melanerpes lewis", "Rangifer tarandus",
                        "Accipiter gentilis laingi"
)

ch_dep_total <- ch_data %>%
  filter(scientific_name %in% og_scientific_name) %>%
  mutate(og_species_area = st_area(.),
       og_species_area  =as.numeric(set_units(og_species_area, ha))) %>%
  st_set_geometry(NULL) %>%
  rename_all(tolower) %>%
  summarise(ch_area = sum(og_species_area)) %>%
  ungroup()


og_dep_sum <- og_ch %>%
  filter(scientific_name %in% og_scientific_name) %>%
  mutate(og_species_area = st_area(.),
         og_species_area  =as.numeric(set_units(og_species_area, ha))) %>%
  st_set_geometry(NULL) %>%
  rename_all(tolower) %>%
  summarise(og_ch_area = sum(og_species_area)) %>%
  ungroup()

og_dep_prot_sum <- prot_og_ch %>%
  filter(scientific_name %in% og_scientific_name) %>%
  mutate(og_prot_species_area=st_area(.),
         og_prot_species_area =as.numeric(set_units(og_prot_species_area, ha))) %>%
  st_set_geometry(NULL) %>%
  rename_all(tolower) %>%
  summarise(og_prot_area = sum(og_prot_species_area))

og_dep_priority_sum <- priority_og_ch %>%
  filter(scientific_name %in% og_scientific_name) %>%
  mutate(og_faib_species_area=st_area(.),
         og_faib_species_area =as.numeric(set_units(og_faib_species_area, ha))) %>%
  st_set_geometry(NULL) %>%
  rename_all(tolower) %>%
  summarise(og_priority_area = sum(og_faib_species_area))


og_dep_priority_prot_sum <- priority_prot_og_ch %>%
  filter(scientific_name %in% og_scientific_name) %>%
  mutate(species_area=st_area(.),
         species_area =as.numeric(set_units(species_area, ha)),
         species_area = replace_na(species_area,0)) %>%
  st_set_geometry(NULL) %>%
  rename_all(tolower) %>%
  summarise(og_priority_area_prot = sum(species_area)) %>%
  bind_cols(og_dep_sum, og_dep_prot_sum, og_dep_priority_sum, ch_dep_total) %>%
  mutate_if(is.numeric, ~replace(., is.na(.),0)) %>%
  mutate(og_ch_perc = (og_ch_area/100)/bcmaps::bc_area()*100,
         prot_ch_perc = og_prot_area/og_ch_area *100,
         unprotected_area = og_ch_area - og_prot_area,
         perc_all_unprot = unprotected_area/og_ch_area*100,
         perc_og_ch = og_ch_area/ch_area*100,
         #
         og_priority_ch_perc = (og_priority_area/100)/bcmaps::bc_area()*100,
         prot_priority_ch_perc = og_priority_area_prot/og_priority_area *100,
         unprotected_priority_area = og_priority_area - og_priority_area_prot,
         perc_priority_unprot = unprotected_priority_area/og_priority_area*100,
         perc_priority_og_ch = og_priority_area/ch_area*100,
         unprot_priority = unprotected_priority_area/unprotected_area *100

         #
  )

write_csv(og_dep_priority_prot_sum, "out/ch_totals_dep_data.csv")
