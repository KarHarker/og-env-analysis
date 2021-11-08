bec_at_risk<- unique(biod$eco_communities)

data<- bec_og %>%
  filter(MAP_LABEL %in% bec_at_risk) %>%
  mutate(bec_at_risk = st_area(.),
         bec_at_risk  =as.numeric(set_units(bec_at_risk, ha))) %>%
  st_set_geometry(NULL) %>%
  rename_all(tolower) %>%
  group_by(zone, subzone, variant, natural_disturbance,
           map_label, bgc_label, zone_name) %>%
  mutate_if(is.numeric, ~replace(., is.na(.),0)) %>%
  summarise(bec_at_risk = sum(bec_at_risk)) %>%
  ungroup()

bec_output<- bec_pa_og %>%
  filter(MAP_LABEL %in% bec_at_risk) %>%
  mutate(bec_at_risk_prot = st_area(.),
         bec_at_risk_prot =as.numeric(set_units(bec_at_risk_prot, ha))) %>%
  st_set_geometry(NULL) %>%
  rename_all(tolower) %>%
  group_by(zone, subzone, variant, natural_disturbance,
           map_label, bgc_label, zone_name) %>%
  mutate_if(is.numeric, ~replace(., is.na(.),0)) %>%
  summarise(bec_at_risk_prot = sum(bec_at_risk_prot)) %>%
  ungroup() %>%
  left_join(data) %>%
  mutate(at_risk_og = bec_at_risk - bec_at_risk_prot,
         perc_prot = round(bec_at_risk_prot/bec_at_risk *100, digits=2),
         perc_at_risk = round(at_risk_og/bec_at_risk*100, digits=2))

bec_output
write_csv(bec_output, "og_eco_communities_at_risk.csv")


# Area calculations

output <- pa_og_tech %>%
  mutate(og_prot = st_area(.),
         og_prot =as.numeric(set_units(og_prot, km^2))) %>%
  st_set_geometry(NULL) %>%
  summarise(og_prot_sum = sum(og_prot))

og_output <- og_data %>%
  mutate(og_area = st_area(.),
         og_area =as.numeric(set_units(og_area, km^2))) %>%
  st_set_geometry(NULL) %>%
  summarise(og_area_sum = sum(og_area))
