tar_load(c(df1_og, df2_og, df1_og_prot, df2_og_prot, df1_og_priority, df2_og_priority,
           df1_og_prot_priority, df2_og_prot_priority, df_1, df_2))

overall_area <- read_csv("og-prot_nat-res-area.csv")

bec <- read.csv("BGC_Catalogue_BECv12_OldGrowth_Targets.csv") %>%
  rename_all(tolower) %>%
  filter(bec_natural_disturbance_code=="NDT1" | bec_natural_disturbance_code=="NDT2") %>%
  unite(label_and_disturbance, c("map_label", "bec_natural_disturbance_code"), remove=FALSE)

bec_og_wet<- bec$label_and_disturbance

df1 <- df_1 %>%
  unite(label_and_disturbance, c("MAP_LABEL", "NATURAL_DISTURBANCE"), remove=FALSE) %>%
  filter(label_and_disturbance %in% bec_og_wet) %>%
  mutate(df1_area = st_area(.),
         df1_area  =as.numeric(set_units(df1_area, ha))) %>%
  st_set_geometry(NULL) %>%
  rename_all(tolower) %>%
  summarise(df1_total = sum(df1_area)) %>%
  ungroup()

df2 <- df_2 %>%
  unite(label_and_disturbance, c("MAP_LABEL", "NATURAL_DISTURBANCE"), remove=FALSE) %>%
  filter(label_and_disturbance %in% bec_og_wet) %>%
  mutate(df2_area = st_area(.),
         df2_area  =as.numeric(set_units(df2_area, ha))) %>%
  st_set_geometry(NULL) %>%
  rename_all(tolower) %>%
  summarise(df2_total = sum(df2_area)) %>%
  ungroup()


df1<- df1_og %>%
  unite(label_and_disturbance, c("MAP_LABEL", "NATURAL_DISTURBANCE"), remove=FALSE) %>%
  filter(label_and_disturbance %in% bec_og_wet) %>%
  mutate(df1_area = st_area(.),
         df1_area  =as.numeric(set_units(df1_area, ha))) %>%
  st_set_geometry(NULL) %>%
  rename_all(tolower) %>%
  summarise(df1_og_total = sum(df1_area)) %>%
  ungroup()


df2<- df2_og %>%
  unite(label_and_disturbance, c("MAP_LABEL", "NATURAL_DISTURBANCE"), remove=FALSE) %>%
  filter(label_and_disturbance %in% bec_og_wet) %>%
  mutate(df2_area = st_area(.),
         df2_area  =as.numeric(set_units(df2_area, ha))) %>%
  st_set_geometry(NULL) %>%
  rename_all(tolower) %>%
  summarise(df2_og_total = sum(df2_area)) %>%
  ungroup()

df1_prot<- df1_og_prot %>%
  unite(label_and_disturbance, c("MAP_LABEL", "NATURAL_DISTURBANCE"), remove=FALSE) %>%
  filter(label_and_disturbance %in% bec_og_wet) %>%
  mutate(df1_area_prot = st_area(.),
         df1_area_prot  =as.numeric(set_units(df1_area_prot, ha))) %>%
  st_set_geometry(NULL) %>%
  rename_all(tolower) %>%
  summarise(df1_area_protected = sum(df1_area_prot)) %>%
  ungroup()

df2_prot<- df2_og_prot %>%
  unite(label_and_disturbance, c("MAP_LABEL", "NATURAL_DISTURBANCE"), remove=FALSE) %>%
  filter(label_and_disturbance %in% bec_og_wet) %>%
  mutate(df2_area_prot = st_area(.),
         df2_area_prot  =as.numeric(set_units(df2_area_prot, ha))) %>%
  st_set_geometry(NULL) %>%
  rename_all(tolower) %>%
  summarise(df2_area_protected = sum(df2_area_prot)) %>%
  ungroup()

df1_at_risk<- df1_og_priority %>%
  unite(label_and_disturbance, c("MAP_LABEL", "NATURAL_DISTURBANCE"), remove=FALSE) %>%
  filter(label_and_disturbance %in% bec_og_wet) %>%
  mutate(df1_area_at_risk = st_area(.),
         df1_area_at_risk  =as.numeric(set_units(df1_area_at_risk, ha))) %>%
  st_set_geometry(NULL) %>%
  rename_all(tolower) %>%
  summarise(df1_area_priority = sum(df1_area_at_risk)) %>%
  ungroup()

df2_at_risk<- df2_og_priority  %>%
  unite(label_and_disturbance, c("MAP_LABEL", "NATURAL_DISTURBANCE"), remove=FALSE) %>%
  filter(label_and_disturbance %in% bec_og_wet) %>%
  mutate(df2_area_at_risk = st_area(.),
         df2_area_at_risk  =as.numeric(set_units(df2_area_at_risk, ha))) %>%
  st_set_geometry(NULL) %>%
  rename_all(tolower) %>%
  summarise(df2_area_priority = sum(df2_area_at_risk))


df1_at_risk_prot<- df1_og_prot_priority %>%
  unite(label_and_disturbance, c("MAP_LABEL", "NATURAL_DISTURBANCE"), remove=FALSE) %>%
  filter(label_and_disturbance %in% bec_og_wet) %>%
  mutate(df1_area_at_risk = st_area(.),
         df1_area_at_risk  =as.numeric(set_units(df1_area_at_risk, ha))) %>%
  st_set_geometry(NULL) %>%
  rename_all(tolower) %>%
  summarise(df1_area_priority_prot = sum(df1_area_at_risk)) %>%
  ungroup()

df2_at_risk_prot <- df2_og_prot_priority %>%
  unite(label_and_disturbance, c("MAP_LABEL", "NATURAL_DISTURBANCE"), remove=FALSE) %>%
  filter(label_and_disturbance %in% bec_og_wet) %>%
  mutate(df2_area_at_risk = st_area(.),
         df2_area_at_risk  =as.numeric(set_units(df2_area_at_risk, ha))) %>%
  st_set_geometry(NULL) %>%
  rename_all(tolower) %>%
  summarise(df2_area_priority_prot = sum(df2_area_at_risk))

output <- cbind(df1, df2, df1_prot, df2_prot, df1_at_risk, df2_at_risk, df1_at_risk_prot, df2_at_risk_prot,
                df1_total, df2_total) %>%
  mutate(df_og_all = df1_og_total + df2_og_total,
         df1_unprot = df1_og_total - df1_area_protected,
         df2_unprot = df2_og_total - df2_area_protected,
         perc_df1_unprot = df1_unprot/df1_og_total*100,
         perc_df2_unprot = df2_unprot/df2_og_total*100,
         df_og_unprot_sum = df1_unprot + df2_unprot,
         perc_unprot = df_og_unprot_sum/df_og_all*100,
         df_area = df1_total + df2_total,
         og_area_all = df_og_all/df_area * 100,

         df_priority_og = df1_area_priority + df2_area_priority,
         df1_priority_unprot = df1_area_priority - df1_area_priority_prot,
         df2_priority_unprot = df2_area_priority - df2_area_priority_prot,
         perc_df1_priority_unprot = df1_priority_unprot/df1_area_priority*100,
         perc_df2_priority_unprot = df2_priority_unprot/df2_area_priority*100,
         df_priority_unprot_sum = df1_priority_unprot + df2_priority_unprot,
         perc_priority_unprot = df_priority_unprot_sum/df_priority_og*100,
         priority_og_area = df_priority_og/df_area*100
         )


write_csv(output, "out/df_areas.csv")
#overall<- bind_rows(df1, df2, df1_prot, df2_prot)
