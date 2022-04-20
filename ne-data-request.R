NE<-bcmaps::nr_districts() %>%
  filter(DISTRICT_NAME== "Peace Natural Resource District" | DISTRICT_NAME== "Fort Nelson Natural Resource District") %>%
  st_make_valid() %>%
  st_cast(to = "POLYGON", warn = FALSE) %>%
  select(DISTRICT_NAME) %>%
  rename(district_name=DISTRICT_NAME)

tar_load(c(og_data, pa_data, ch_data, priority_og, des_lands, des_lands_vis, og))

og_5 <- intersect_pa(og_data, NE)

og_5 <- og_5 %>%
  rename_all(tolower) %>%
  st_make_valid() %>%
  st_cast(to = "POLYGON", warn = FALSE) %>%
  relocate(polygon_id)

write_sf(og_5, "ne_out/at-risk_old-growth.shp", layer_options = "ENCODING=UTF-8")

og_2 <- intersect_pa(priority_og, NE)

og_2 <- og_2 %>%
  rename_all(tolower) %>%
  st_make_valid() %>%
  st_cast(to = "POLYGON", warn = FALSE) %>%
  relocate(polygon_id)

write_sf(og_2, "ne_out/priority-at-risk_old-growth.shp")

pa <- intersect_pa(pa_data, NE)
write_sf(pa, "ne_out/ppa-oecm.shp")

ch <- intersect_pa(ch_data, NE)
write_sf(ch, "ne_out/critical-habitat.shp")

des<- intersect_pa(des_lands, NE)
write_sf(des, "ne_out/designated_lands_flat.shp")

des_vis <- intersect_pa(des_lands_vis, NE)
write_sf(des_vis, "ne_out/designated_lands_overlapping.shp")

og_all <- intersect_pa(og, NE)

og_all<- og_all %>%
  rename_all(tolower) %>%
  st_make_valid() %>%
  st_cast(to = "POLYGON", warn = FALSE) %>%
  relocate(polygon_id)

write_sf(og_all, "ne_out/all-old-growth.shp")

tar_load(kba)
