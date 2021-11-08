tar_load(c(df1_og, df2_og, df1_og_prot, df2_og_prot, faib_df1_og,
           faib_df2_og))

overall_area <- read_csv("og-prot_nat-res-area.csv")

bec <- read.csv("BGC_Catalogue_BECv12_OldGrowth_Targets.csv") %>%
  rename_all(tolower) %>%
  filter(bec_natural_disturbance_code=="NDT1" | bec_natural_disturbance_code=="NDT2") %>%
  unite(label_and_disturbance, c("map_label", "bec_natural_disturbance_code"), remove=FALSE)

bec_og_wet<- bec$label_and_disturbance

df1<- df1_og %>%
  unite(label_and_disturbance, c("MAP_LABEL", "NATURAL_DISTURBANCE"), remove=FALSE) %>%
  filter(label_and_disturbance %in% bec_og_wet) %>%
  st_join(bcmaps::nr_districts()) %>%
  mutate(df1_area = st_area(.),
         df1_area  =as.numeric(set_units(df1_area, ha))) %>%
  st_set_geometry(NULL) %>%
  rename_all(tolower) %>%
  group_by(district_name) %>%
  summarise(df1_og_total = sum(df1_area)) %>%
  ungroup()


df2<- df2_og %>%
  unite(label_and_disturbance, c("MAP_LABEL", "NATURAL_DISTURBANCE"), remove=FALSE) %>%
  filter(label_and_disturbance %in% bec_og_wet) %>%
  st_join(bcmaps::nr_districts()) %>%
  mutate(df2_area = st_area(.),
         df2_area  =as.numeric(set_units(df2_area, ha))) %>%
  st_set_geometry(NULL) %>%
  rename_all(tolower) %>%
  group_by(district_name) %>%
  summarise(df2_og_total = sum(df2_area)) %>%
  ungroup()

df1_prot<- df1_og_prot %>%
  unite(label_and_disturbance, c("MAP_LABEL", "NATURAL_DISTURBANCE"), remove=FALSE) %>%
  filter(label_and_disturbance %in% bec_og_wet) %>%
  st_join(bcmaps::nr_districts()) %>%
  mutate(df1_area_prot = st_area(.),
         df1_area_prot  =as.numeric(set_units(df1_area_prot, ha))) %>%
  st_set_geometry(NULL) %>%
  rename_all(tolower) %>%
  group_by(district_name) %>%
  summarise(df1_area_protected = sum(df1_area_prot)) %>%
  ungroup()

df2_prot<- df2_og_prot %>%
  unite(label_and_disturbance, c("MAP_LABEL", "NATURAL_DISTURBANCE"), remove=FALSE) %>%
  filter(label_and_disturbance %in% bec_og_wet) %>%
  st_join(bcmaps::nr_districts()) %>%
  mutate(df2_area_prot = st_area(.),
         df2_area_prot  =as.numeric(set_units(df2_area_prot, ha))) %>%
  st_set_geometry(NULL) %>%
  rename_all(tolower) %>%
  group_by(district_name) %>%
  summarise(df2_area_protected = sum(df2_area_prot)) %>%
  ungroup()

df1_at_risk<- faib_prot_df1_og %>%
  unite(label_and_disturbance, c("MAP_LABEL", "NATURAL_DISTURBANCE"), remove=FALSE) %>%
  filter(label_and_disturbance %in% bec_og_wet) %>%
  mutate(df1_area_at_risk = st_area(.),
         df1_area_at_risk  =as.numeric(set_units(df1_area_at_risk, ha))) %>%
  st_set_geometry(NULL) %>%
  rename_all(tolower) %>%
  group_by(district_name) %>%
  summarise(df1_area_at_risk = sum(df1_area_at_risk)) %>%
  ungroup()

output<- faib_prot_df2_og %>%
  unite(label_and_disturbance, c("MAP_LABEL", "NATURAL_DISTURBANCE"), remove=FALSE) %>%
  filter(label_and_disturbance %in% bec_og_wet) %>%
  mutate(df2_area_at_risk = st_area(.),
         df2_area_at_risk  =as.numeric(set_units(df2_area_at_risk, ha))) %>%
  st_set_geometry(NULL) %>%
  rename_all(tolower) %>%
  group_by(district_name) %>%
  summarise(df2_area_at_risk = sum(df2_area_at_risk)) %>%
  ungroup() %>%
  left_join(df1) %>%
  left_join(df2) %>%
  left_join(df1_prot) %>%
  left_join(df2_prot) %>%
  left_join(df1_at_risk) %>%
  left_join(overall_area) %>%
  group_by(district_name) %>%
  mutate(df_at_risk_sum = df1_area_at_risk + df2_area_at_risk,
         perc_at_risk_df=df_at_risk_sum/og_district_area*100,
         perc_df = og_district_area/district_area*100,
         df_index = perc_at_risk_df/perc_df)


write_csv(output, "df_areas_by_resource_district.csv")
#overall<- bind_rows(df1, df2, df1_prot, df2_prot)
