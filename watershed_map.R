
tar_load(c(og_watershed, prot_watershed, watershed_data))

watershed_totals <- read_csv("prot_og_community_watersheds.csv") %>%
  mutate_if(is.numeric, ~replace(., is.na(.),0)) %>%
  select(cw_name, cw_source_name, cw_status, source_category, cw_use, water_treatment_type, water_treatment_location,
         organization_concerns, watershed_area, og_watershed_area, prot_og_watershed_area, og_percentage, prot_percentage,
         at_risk_area, perc_at_risk)

watershed_data_map <- watershed_data %>%
  left_join(watershed_totals, by=c("cw_name", "cw_source_name", "cw_status", "source_category", "cw_use",
                                   "water_treatment_type", "water_treatment_location",
                                   "organization_concerns")) %>%
  filter(og_watershed_area >10) %>%
  mutate(rel_imp = perc_at_risk/og_percentage,
         norm_rel_imp = (rel_imp-mean(rel_imp))/sd(rel_imp))

data <- watershed_data_map %>%
  st_drop_geometry()

write_csv(data, "watershed_data_maps.csv")


geom_bc <- geom_sf(data = bc_bound(), fill = NA, size = 0.2)

watershed_map <- ggplot() +
  geom_bc +
  geom_sf(
    data = watershed_data_map, aes(fill = og_percentage), colour = NA) +
  scale_fill_viridis_c(option="B") +
  labs(#title = "Underrepresented BEC variants x Ecoregions\n in B.C. Parks and Protected Areas\n PPAs & OECMs removed",
    #caption = "Ecoregions*Variants with < 17% protected,\nwhere the variant makes up at least 1.25% of an ecoregion\nor is provincially rare (in the bottom 5% of variants)",
    fill = "Percent Old-Growth (%)") +
  theme_minimal() +
  coord_sf(xlim = c(828843.9,1853257.5), ylim = c(374096.6,760000))
watershed_map

watershed_map_upog <- ggplot() +
  geom_bc +
  geom_sf(
    data = watershed_data_map, aes(fill = norm_rel_imp), colour = NA) +
  scale_fill_viridis_c(option="B") +
  labs(#title = "Underrepresented BEC variants x Ecoregions\n in B.C. Parks and Protected Areas\n PPAs & OECMs removed",
    #caption = "Ecoregions*Variants with < 17% protected,\nwhere the variant makes up at least 1.25% of an ecoregion\nor is provincially rare (in the bottom 5% of variants)",
    fill = "Relative Value of Old Growth") +
  theme_minimal()+
  coord_sf(xlim = c(828843.9,1853257.5), ylim = c(374096.6,760000))
watershed_map_upog

combined <- plot_grid(watershed_map, watershed_map_upog, ncol=1, align="v", rel_heights=c(1,1))

ggsave("out/maps/community_watersheds_both.png", combined, width = 9, height = 6, dpi = 300)


