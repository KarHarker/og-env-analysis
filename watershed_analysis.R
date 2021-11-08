tar_load(c(og_watershed, prot_watershed, priority_og_watershed, priority_prot_watershed, watershed_data))

  og_watershed_sum <-og_watershed %>%
    mutate(og_watershed_area = st_area(.),
           og_watershed_area =as.numeric(set_units(og_watershed_area, ha))) %>%
    st_set_geometry(NULL) %>%
    group_by(cw_name, cw_source_name, cw_source_type, source_category,
             source_comments, water_treatment_type, water_treatment_location) %>%
    summarise(og_watershed_area = sum(og_watershed_area)) %>%
    ungroup()

  og_prot_watershed_sum <- prot_watershed %>%
    mutate(prot_og_watershed_area = st_area(.),
           prot_og_watershed_area =as.numeric(set_units(prot_og_watershed_area, ha))) %>%
    st_set_geometry(NULL) %>%
    group_by(cw_name, cw_source_name, cw_source_type, source_category,
             source_comments, water_treatment_type, water_treatment_location) %>%
    summarise(prot_og_watershed_area = sum(prot_og_watershed_area)) %>%
    ungroup()

  priority_og_watershed_sum  <-priority_og_watershed %>%
    mutate(og_watershed_area = st_area(.),
           og_watershed_area =as.numeric(set_units(og_watershed_area, ha))) %>%
    st_set_geometry(NULL) %>%
    group_by(cw_name, cw_source_name, cw_source_type, source_category,
             source_comments, water_treatment_type, water_treatment_location) %>%
    summarise(priority_og_watershed_area = sum(og_watershed_area)) %>%
    ungroup()

  priority_og_prot_watershed_sum <- priority_prot_watershed %>%
    mutate(prot_og_watershed_area = st_area(.),
           prot_og_watershed_area =as.numeric(set_units(prot_og_watershed_area, ha))) %>%
    st_set_geometry(NULL) %>%
    group_by(cw_name, cw_source_name, cw_source_type, source_category,
             source_comments, water_treatment_type, water_treatment_location) %>%
    summarise(priority_prot_og_watershed_area = sum(prot_og_watershed_area)) %>%
    ungroup()


  output <- watershed_data %>%
    mutate(watershed_area = st_area(.),
           watershed_area=as.numeric(set_units(watershed_area, ha))) %>%
    st_set_geometry(NULL) %>%
    left_join(og_watershed_sum) %>%
    left_join(og_prot_watershed_sum) %>%
    left_join(priority_og_watershed_sum) %>%
    left_join(priority_og_prot_watershed_sum) %>%
    mutate_if(is.numeric, ~replace(., is.na(.),0)) %>%
    mutate(og_percentage = round(og_watershed_area/watershed_area * 100, digits=2),
           prot_percentage = round(prot_og_watershed_area/og_watershed_area * 100, digits=2),
           unprot_area = og_watershed_area-prot_og_watershed_area,
           perc_unprot_area = round(unprot_area/og_watershed_area * 100, digits=2),
           #
           priority_og_percentage = round(priority_og_watershed_area/watershed_area * 100, digits=2),
           priority_prot_percentage = round(priority_prot_og_watershed_area/priority_og_watershed_area * 100, digits=2),
           priority_unprot_area = priority_og_watershed_area -priority_prot_og_watershed_area,
           priority_perc_unprot = round(priority_unprot_area/priority_og_watershed_area * 100, digits=2),
           #
           perc_unprot_priority = priority_unprot_area/unprot_area*100
           )

output_og <- output %>%
  filter(og_watershed_area > 0)

output_priority_og <- output %>%
  filter(priority_og_watershed_area > 0)

  write.csv(output_og, "out/og-community-watersheds_Nov2.csv")


  og_watershed_summary <-output_og %>%
    summarise(og_watershed_area = sum(og_watershed_area),
              unprot_area = sum(unprot_area),
              watershed_area = sum(watershed_area),
              #
              priority_og_watershed_area = sum(priority_og_watershed_area),
              priority_unprot_area = sum(priority_unprot_area)) %>%
    mutate(overall_og_perc = round(og_watershed_area/watershed_area * 100, digits=2),
           priority_og_perc = priority_og_watershed_area/watershed_area*100,
           perc_unprot = unprot_area/og_watershed_area*100,
           perc_unprot_priority = priority_unprot_area/unprot_area*100
           )

