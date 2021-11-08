bec <- read.csv("BGC_Catalogue_BECv12_OldGrowth_Targets.csv") %>%
  rename_all(tolower) %>%
  filter(bec_natural_disturbance_code=="NDT1" | bec_natural_disturbance_code=="NDT2") %>%
  unite(label_and_disturbance, c("map_label", "bec_natural_disturbance_code"), remove=FALSE)

bec_og_wet<- bec$label_and_disturbance

data<- data %>%
  unite(label_and_disturbance, c("MAP_LABEL", "NATURAL_DISTURBANCE"), remove=FALSE) %>%
  filter(label_and_disturbance %in% bec_og_wet) %>%
  mutate(bec_wet_og_area = st_area(.),
         bec_wet_og_area  =as.numeric(set_units(bec_wet_og_area , ha))) %>%
  st_set_geometry(NULL) %>%
  rename_all(tolower) %>%
  group_by(label_and_disturbance, zone, subzone, variant, natural_disturbance,
           map_label, bgc_label, zone_name) %>%
  summarise(bec_wet_og_area = sum(bec_wet_og_area)) %>%
  ungroup()

bec_output<- data_prot %>%
  unite(label_and_disturbance, c("MAP_LABEL", "NATURAL_DISTURBANCE"), remove=FALSE) %>%
  filter(label_and_disturbance %in% bec_og_wet) %>%
  mutate(bec_wet_og_area_prot = st_area(.),
         bec_wet_og_area_prot  =as.numeric(set_units(bec_wet_og_area_prot, ha))) %>%
  st_set_geometry(NULL) %>%
  rename_all(tolower) %>%
  group_by(label_and_disturbance, zone, subzone, variant, natural_disturbance,
           map_label, bgc_label, zone_name) %>%
  summarise(bec_wet_og_area_prot = sum(bec_wet_og_area_prot)) %>%
  ungroup() %>%
  left_join(data) %>%
  mutate(at_risk_og = bec_wet_og_area - bec_wet_og_area_prot)

bec_output
