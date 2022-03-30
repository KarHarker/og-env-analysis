tar_load(c(seymour_og_priority, seymour_og_at_risk, seymour_og_all,
           seymour_og_priority_protected, seymour_og_at_risk_protected, seymour_og_all_protected))

sey_og_priority <- seymour_og_priority %>%
  mutate(og_area = st_area(.),
         og_area = as.numeric(set_units(og_area, ha))) %>%
  st_drop_geometry() %>%
  summarise(og_priority_area = sum(og_area))

sey_og_at_risk <- seymour_og_at_risk %>%
  mutate(og_area = st_area(.),
         og_area = as.numeric(set_units(og_area, ha))) %>%
  st_drop_geometry() %>%
  summarise(og_at_risk = sum(og_area))

sey_og_all <- seymour_og_all %>%
  mutate(og_area = st_area(.),
         og_area = as.numeric(set_units(og_area, ha))) %>%
  st_drop_geometry() %>%
  summarise(og_all = sum(og_area))

sey_og_priority_prot <- seymour_og_priority_protected %>%
  mutate(og_area = st_area(.),
         og_area = as.numeric(set_units(og_area, ha))) %>%
  st_drop_geometry() %>%
  summarise(og_priority_area_prot = sum(og_area))

sey_og_at_risk_prot <- seymour_og_at_risk_protected %>%
  mutate(og_area = st_area(.),
         og_area = as.numeric(set_units(og_area, ha))) %>%
  st_drop_geometry() %>%
  summarise(og_at_risk_prot = sum(og_area))

sey_og_all_prot <- seymour_og_all_protected %>%
  mutate(og_area = st_area(.),
         og_area = as.numeric(set_units(og_area, ha))) %>%
  st_drop_geometry() %>%
  summarise(og_all_prot = sum(og_area))

sey_summary <- bind_cols(sey_og_priority, sey_og_at_risk, sey_og_all,
                         sey_og_priority_prot, sey_og_at_risk_prot, sey_og_all_prot) %>%
  mutate(perc_priority_prot = sey_og_priority_prot/sey_og_priority*100,
         perc_at_risk_prot = sey_og_at_risk_prot/sey_og_at_risk*100,
         perc_all_prot = sey_og_all_prot/sey_og_all*100)

write_csv(sey_summary, "out/sey_summary.csv")
