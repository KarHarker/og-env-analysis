pop_watersheds<- pop_watersheds %>%
  st_set_geometry(NULL)

cities<-bcmaps::bc_cities() %>%
  rename_all(tolower) %>%
  select(name, pop_2000)

fn_watersheds <- fn_watersheds %>%
  st_set_geometry(NULL) %>%
  select(cw_name, cw_source_name, cw_status, source_category, cw_use, water_treatment_type, water_treatment_location,
         organization_concerns, first_nation_bc_name, preferred_name)


nat_res_data<- read_csv("og-prot_nat-res-area.csv")

community_data<-read_csv("prot_og_community_watersheds.csv") %>%
  select(-cw_date_created) %>%
  select(cw_name, cw_source_name, cw_status, source_category, cw_use, water_treatment_type, water_treatment_location,
         organization_concerns, watershed_area, og_watershed_area, prot_og_watershed_area, og_percentage, prot_percentage,
         at_risk_area, perc_at_risk) %>%
  left_join(pop_watersheds) %>%
  select(cw_name, cw_source_name, cw_status, source_category, cw_use, water_treatment_type, water_treatment_location,
         organization_concerns, watershed_area, og_watershed_area, prot_og_watershed_area, og_percentage, prot_percentage,
         at_risk_area, perc_at_risk, pcname) %>%
  rename(name=pcname) %>%
  left_join(cities<-bcmaps::bc_cities() %>% st_set_geometry(NULL) %>% rename_all(tolower) %>% select(name, pop_2000)) %>%
  left_join(fn_watersheds)

write.csv(community_data, "prot_og_community_watersheds_with_limited_population.csv")

nat_res_data<- read_csv("og-prot_nat-res-area.csv")
