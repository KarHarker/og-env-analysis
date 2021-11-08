tar_load(c(og_ch, prot_og_ch, ch_data))


overall_og_dependent_ch_habitat<- function(data, data2, data3){

  og_scientific_name <- c("Marmota vancouverensis", "Limnanthes macounii",
                          "Aegolius acadicus brooksi", "Cephalanthera austiniae",
                          "Hemphillia dromedarius", "Prophysaon coeruleum",
                          "Sphyrapicus thyroideus nataliae", "Brachyramphus marmoratus",
                          "Collema coniophilum", "Melanerpes lewis", "Rangifer tarandus",
                          "Accipiter gentilis laingi"
  )

  overall_ch <- overall_ch %>%
    filter(scientific_name %in% og_scientific_name) %>%
    mutate(species_area=st_area(.),
           species_area =as.numeric(set_units(species_area, ha))) %>%
    st_set_geometry(NULL) %>%
    summarise(overall_area = sum(species_area))

  overall_og_ch <- overall_og_ch %>%
    filter(scientific_name %in% og_scientific_name) %>%
    mutate(og_species_area=st_area(.),
           og_species_area =as.numeric(set_units(og_species_area, ha))) %>%
    st_set_geometry(NULL) %>%
    summarise(overall_og_ch_area = sum(og_species_area))

  overall_og_ch_prot <- overall_og_ch_prot %>%
    filter(scientific_name %in% og_scientific_name) %>%
    mutate(og_species_area_prot=st_area(.),
           og_species_area_prot =as.numeric(set_units(og_species_area_prot, ha))) %>%
    st_set_geometry(NULL) %>%
    summarise(overall_og_ch_area_prot = sum(og_species_area_prot))

  og_ch_percentage = (overall_og_ch[1,1]-overall_og_ch_prot[1,1])/overall_ch[1,1]*100
}

ch_totals <- read_csv("og-dependent_critical-habitat.csv") %>%
  mutate_if(is.numeric, ~replace(., is.na(.),0))

ch_data_map <-og_ch %>%
  filter(scientific_name %in% og_scientific_name) %>%
  mutate(og_ch_area=st_area(.),
         og_ch_area =as.numeric(set_units(og_ch_area, ha))) %>%
  left_join(ch_totals, by=c("scientific_name", "common_name_english", "cosewic_status", "schedule_status", "sara_schedule")) %>%
  mutate()

  mutate(rel_imp = perc_at_risk/og_percentage)



data <- watershed_data_map %>%
  st_drop_geometry()

write_csv(data, "watershed_data_maps.csv")


geom_bc <- geom_sf(data = bc_bound(), fill = NA, size = 0.2)

watershed_map <- ggplot() +
  geom_bc +
  geom_sf(
    data = watershed_data_map, aes(fill = og_percentage), colour = NA) +
  scale_fill_viridis_c(option="A") +
  labs(#title = "Underrepresented BEC variants x Ecoregions\n in B.C. Parks and Protected Areas\n PPAs & OECMs removed",
    #caption = "Ecoregions*Variants with < 17% protected,\nwhere the variant makes up at least 1.25% of an ecoregion\nor is provincially rare (in the bottom 5% of variants)",
    fill = "Percent Old-Growth (%)") +
  theme_minimal() +
  coord_sf(xlim = c(828843.9,1853257.5), ylim = c(374096.6,760000))
watershed_map

watershed_map_upog <- ggplot() +
  geom_bc +
  geom_sf(
    data = watershed_data_map, aes(fill = rel_imp), colour = NA) +
  scale_fill_viridis_c(option="A") +
  labs(#title = "Underrepresented BEC variants x Ecoregions\n in B.C. Parks and Protected Areas\n PPAs & OECMs removed",
    #caption = "Ecoregions*Variants with < 17% protected,\nwhere the variant makes up at least 1.25% of an ecoregion\nor is provincially rare (in the bottom 5% of variants)",
    fill = "Relative Value of Old Growth") +
  theme_minimal()+
  coord_sf(xlim = c(828843.9,1853257.5), ylim = c(374096.6,760000))
watershed_map_upog

combined <- plot_grid(watershed_map, watershed_map_upog, ncol=1, align="v", rel_heights=c(1,1))

ggsave("out/maps/community_watersheds_both.png", combined, width = 9, height = 6, dpi = 300)
#geojson_write(scenario_output, file="out/bec_scenario_17.geojson")

