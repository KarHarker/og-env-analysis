tar_load(c(at_risk_bec_eco, og_bec_eco, og_bec_eco_pa))

eco_bec_summary <- read_csv("c:/tmp/ecosystem-representation-analysis/eco_bec_summary.csv") %>%
  mutate(variant = as.character(variant))

pa_eco_bec_summary <- read_csv("c:/tmp/ecosystem-representation-analysis/pa_eco_bec_summary.csv")%>%
  mutate(variant = as.character(variant)) %>%
  group_by(ecoregion_code, ecoregion_name, zone, subzone, variant, bec_variant, bec_eco_area, percent_comp_prov,
           percent_comp_ecoregion) %>%
  summarise(conserved_area = sum(conserved_area),
            percent_conserved = sum(percent_conserved))


og_bec_pa_summary <- og_bec_eco_pa %>%
  mutate(og_pa_area = as.numeric(st_area(.))) %>%
  st_drop_geometry() %>%
  rename_all(tolower) %>%
  group_by(ecoregion_name, ecoregion_code, zone, subzone, variant) %>%
  summarise(og_pa_area = sum(og_pa_area), .groups = "drop") %>%
  mutate(bec_variant =paste0(zone, subzone, replace_na(variant, "")))

og_bec_summary <- og_bec_eco %>%
  mutate(og_area = as.numeric(st_area(.))) %>%
  st_drop_geometry() %>%
  rename_all(tolower) %>%
  group_by(ecoregion_name, ecoregion_code, zone, subzone, variant) %>%
  summarise(og_area = sum(og_area), .groups = "drop") %>%
  mutate(bec_variant =paste0(zone, subzone, replace_na(variant, ""))) %>%
  left_join(og_bec_pa_summary) %>%
  left_join(pa_eco_bec_summary, by= c("ecoregion_code", "zone", "subzone", "variant", "bec_variant")) %>%
  select(-ecoregion_name.x, ecoregion_name=ecoregion_name.y) %>%
  mutate(og_area_not_prot = og_area - og_pa_area) %>%
  mutate_if(is.numeric, ~replace(., is.na(.),0)) %>%
  mutate(tier_1_prot_perc = (og_area_not_prot + conserved_area)/bec_eco_area * 100)

write_csv(og_bec_summary, "out/og_bec_summary_tier_1.csv")


at_risk_bec_summary <- at_risk_bec_eco %>%
  mutate(at_risk_area = as.numeric(st_area(.))) %>%
  st_drop_geometry() %>%
  rename_all(tolower) %>%
  group_by(ecoregion_name, ecoregion_code, zone, subzone, variant) %>%
  summarise(at_risk_area = sum(at_risk_area), .groups = "drop") %>%
  mutate(bec_variant =paste0(zone, subzone, replace_na(variant, ""))) %>%
  left_join(pa_eco_bec_summary, by= c("ecoregion_code", "zone", "subzone", "variant", "bec_variant")) %>%
  select(-ecoregion_name.x, ecoregion_name=ecoregion_name.y) %>%
  mutate_if(is.numeric, ~replace(., is.na(.),0)) %>%
  mutate(tier_2_prot_perc = (at_risk_area + conserved_area)/bec_eco_area * 100)

write_csv(at_risk_bec_summary, "out/og_bec_summary_tier_2.csv")






