# Copyright 2021 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

library(targets)
library(tarchetypes)
source("packages.R")
source("R/functions.R")

conflict_prefer("filter", "dplyr")
#plan(callr)

#tar_option_set(packages=c("dplyr", "tidyr", "readr", "purrr", "stringr", "ggplot2",
#                          "lubridate", "glue", "assertr", "sf", "bcmaps", "bcdata",
#                          "rmapshaper", "geojsonio", "ggiraph", "cowplot", "shiny",
#                          "knitr", "rmarkdown", "kableExtra", "tibble"),
#               imports=c("bcmaps", "bcdata"))

# load datasets ------------------------------------------------------------------------------------

load_datasets <- list(
  tar_target(og_data, og_load()),
  tar_target(pa_data, get_cpcad_bc_data()),
  tar_target(ch_data, ch_data_load()),
  tar_target(watershed_data, get_com_watersheds()),
  tar_target(pa_tech, protected_area_load()),
  tar_target(bec_12, bec_data_load()),
  tar_target(water_licenses, get_water_licenses()),
  tar_target(df_1, df_leading()),
  tar_target(df_2, df_second()),
  tar_target(fn_communities, fn_load()),
  tar_target(population, pop_load()),
  tar_target(lup_data, lup_boundaries()),
  tar_target(priority_og, priority_og_oct()), #new og area
  tar_target(woodland_area, woodlands()), #woodland area tech panel
  tar_target(caribou_cons_area, caribou_area()),
  tar_target(des_lands, designated_lands()),
  tar_target(des_lands_vis, designated_lands_vis()),
  tar_target(ch_flat, ch_data_flat()),
  tar_target(forest_ten, forest_cutblock()),
  tar_target(mining_active, active_min_data()),
  tar_target(mining_tenure, potential_min_data()),
  tar_target(land_tenure, land_tenures()),
  tar_target(eao_points, eao_data()),
  tar_target(forestry_tenures, forest_tenure()),
  tar_target(priority_oct25, priority_mod_og()),
  tar_target(caribou_gcr_herd, caribou_herds()),
  tar_target(harv_auth, harvest_authority()),
  tar_target(heat_map, heat_map_26()),
  tar_target(future_cons, ralcp()),
  tar_target(caribou_full_herd, caribou_full()),
  tar_target(y2y, y2y_area()),
  tar_target(pip, pip_areas())
)

clean_data <- list(
  tar_target(clean_pa, remove_overlaps(pa_data)),
  tar_target(og_dependent_ch, filter_og_dependent(ch_data)),
  tar_target(lup_clipped, intersect_pa(lup_data, bcmaps::bc_bound_hres())),
  tar_target(pa_type, pa_by_type(clean_pa)),
  tar_target(pa_type_mod, pa_tech_simp(pa_tech)),
  tar_target(int_wet_belt, int_wet_all(bec_12)),
  tar_target(int_wet_belt_tran, int_tran(bec_12)),
  tar_target(int_wet_belt_itr, int_itr(bec_12)),
  tar_target(int_wet_belt_iwb, int_iwb(bec_12))
  #tar_target(ch_agg, ch_data_agg(ch_data))
  #tar_target(ppa_og, merge_areas(clean_pa, faib_og_extra))
)

intersect_data <- list(
  #tar_target(pa_og, intersect_pa(pa_data, og_data)),
  tar_target(pa_og_tech, intersect_pa(pa_tech, og_data)),
  tar_target(og_ch, intersect_pa(og_data, ch_data)),
  tar_target(prot_og_ch, intersect_pa(pa_og_tech, ch_data)),
  tar_target(og_watershed, intersect_pa(og_data, watershed_data)),
  tar_target(prot_watershed, intersect_pa(pa_og_tech, watershed_data)),
  tar_target(bec_og, intersect_pa(og_data, bec_12)),
  tar_target(bec_pa_og, intersect_pa(pa_og_tech, bec_12)),
  tar_target(community_watersheds_licenses_test, intersect_point_to_poly(og_watershed, water_licenses)),
  tar_target(community_watersheds_licenses_prot, intersect_point_to_poly(prot_watershed, water_licenses)),
  tar_target(community_population, intersect_point_to_poly(community_watersheds_licenses_test, bcmaps::bc_cities())),
  tar_target(fn_watersheds, intersect_point_to_poly(watershed_data, fn_communities)),
  tar_target(pop_watersheds, intersect_pa(watershed_data, population)),
  tar_target(df1_og, intersect_pa(bec_og, df_1)),
  tar_target(df2_og, intersect_pa(bec_og, df_2)),
  tar_target(df1_og_prot, intersect_pa(bec_pa_og, df_1)),
  tar_target(df2_og_prot, intersect_pa(bec_pa_og, df_2)),
  tar_target(og_ch_flat, intersect_pa(og_data, ch_flat)),
  tar_target(og_ch_flat_prot, intersect_pa(og_ch_flat, pa_og_tech)),
  tar_target(heat_lup, intersect_pa(heat_map, lup_clipped)),
  tar_target(heat_future, intersect_pa(heat_map, future_cons)),
  tar_target(heat_iwb, intersect_pa(heat_map, int_wet_belt)),
  tar_target(heat_gcr, intersect_pa(heat_map, caribou_cons_area)),
  tar_target(lup_pa, intersect_pa(clean_pa, lup_clipped)),
  tar_target(pcl_pa, intersect_pa(clean_pa, future_cons)),
  tar_target(iwb_pa, intersect_pa(clean_pa, int_wet_belt))
)

round_2_analyses<- list(
  #####
  ##### old layers
  #tar_target(faib_og_extra, load_and_combine()),
  #tar_target(faib_og_layer, intersect_pa(pa_og_tech, priority_og)),
  #tar_target(faib_ch, intersect_pa(faib_og_extra, ch_data)),
  #tar_target(faib_watersheds, intersect_pa(faib_og_extra, watershed_data)),
  #tar_target(faib_by_district, intersect_pa(faib_og_extra, bcmaps::nr_districts())),
  #tar_target(ch_by_district, intersect_pa(ch_data, bcmaps::nr_districts())),
  #tar_target(og_ch_by_district, intersect_pa(og_ch, bcmaps::nr_districts())),
  #tar_target(faib_og_ch, intersect_pa(ch_by_district, faib_by_district)),
  #tar_target(faib_prot_watershed, intersect_pa(prot_watershed, faib_by_district)),
  #tar_target(faib_bec_pa, intersect_pa(bec_pa_og, faib_by_district)),
  #tar_target(faib_df1_og, intersect_pa(df1_og, faib_by_district)),
  #tar_target(faib_df2_og, intersect_pa(df2_og, faib_by_district)),
  #tar_target(faib_prot_df1_og, intersect_pa(df1_og_prot, bcmaps::nr_districts())),
  #tar_target(faib_prot_df2_og, intersect_pa(df2_og_prot, bcmaps::nr_districts())),
  #tar_target(prot_ch_by_district, intersect_pa(prot_og_ch, bcmaps::nr_districts())),
  #tar_target(bec_dist, intersect_pa(bec_12, bcmaps::nr_districts())),
  #tar_target(og_faib_lup, intersect_pa(faib_og_extra, lup_clipped)),
  #tar_target(at_risk_bec, intersect_pa(faib_og_extra, bec_12)),
 # tar_target(all_og_bec, intersect_pa(og_data, bec_12)),
  #tar_target(at_risk_bec_eco, intersect_pa(at_risk_bec, bcmaps::ecoregions())),
  #tar_target(og_bec_eco, intersect_pa(bec_og, bcmaps::ecoregions())),
  #tar_target(og_bec_eco_pa, intersect_pa(og_bec_eco, clean_pa))
  #tar_target(bec_dist_og, intersect_pa(bec_og, bcmaps::nr_districts())),
  #tar_target(bec_dist_og_pa, intersect_pa(bec_pa_og, bcmaps::nr_districts())),
  #tar_target(bec_faib, intersect_pa(bec_12, faib_by_district))
  #tar_target(bec_pa_by_district, intersect_pa(bec_pa_og,  bcmaps::nr_districts()))),
  #tar_target(prot_df1_og_by_district, intersect_pa(df1_og_prot, faib_by_district)),
  #tar_target(prot_df2_og_by_district, intersect_pa(df2_og_prot, faib_by_district)),
)

round_3_analyses <- list(
  #
  tar_target(unprotected, remove_pa(bcmaps::bc_bound_hres(), pa_type)),
  #tar_target(prot_gap, remove_pa(pa_type_mod, pa_type))
  tar_target(priority_og_pa, intersect_pa(pa_tech, priority_og)),
  #bec og comparison
  tar_target(bec_og_priority, intersect_pa(priority_og, bec_12)),
  tar_target(bec_pa_og_priority, intersect_pa(priority_og_pa, bec_12)),
  # og by ch
  tar_target(priority_og_ch, intersect_pa(priority_og, ch_data)),
  tar_target(priority_prot_og_ch, intersect_pa(priority_og_pa, ch_data)),
  #
  tar_target(priority_og_ch_flat, intersect_pa(priority_og, ch_flat)),
  tar_target(priority_og_ch_flat_prot, intersect_pa(priority_og_pa, ch_flat)),
  # og by nr district
  tar_target(og_by_district_priority, intersect_pa(priority_og, bcmaps::nr_districts())),
  tar_target(prot_by_district_priority, intersect_pa(priority_og_pa, bcmaps::nr_districts())),
  tar_target(og_by_district, intersect_pa(og_data, bcmaps::nr_districts())),
  tar_target(prot_by_district, intersect_pa(pa_og_tech, bcmaps::nr_districts())),
  # og by lup areas
  tar_target(og_lup_priority, intersect_pa(priority_og, lup_clipped)),
  tar_target(og_pa_lup_priority, intersect_pa(priority_og_pa, lup_clipped)),
  # og df
  tar_target(df1_og_priority, intersect_pa(bec_og_priority, df_1)),
  tar_target(df2_og_priority, intersect_pa(bec_og_priority, df_2)),
  tar_target(df1_og_prot_priority, intersect_pa(bec_pa_og_priority, df_1)),
  tar_target(df2_og_prot_priority, intersect_pa(bec_pa_og_priority, df_2)),
  #
  tar_target(priority_og_watershed, intersect_pa(priority_og, watershed_data)),
  tar_target(priority_prot_watershed, intersect_pa(priority_og_pa, watershed_data))
)

site_based_analyses <- list(
  tar_target(pa_smc, intersect_pa(caribou_cons_area, clean_pa)),
  tar_target(des_lands_smc, intersect_pa(caribou_cons_area, des_lands)),
  tar_target(des_lands_smc_vis, intersect_pa(caribou_cons_area, des_lands_vis)),
  tar_target(priority_og_smc, intersect_pa(caribou_cons_area, priority_oct25)),
  tar_target(priority_og_pa_smc, intersect_pa(priority_og_smc, pa_smc)),
  tar_target(priority_og_des, intersect_pa(priority_og_smc, des_lands_smc)),
  tar_target(priority_og_des_vis, intersect_pa(des_lands_smc_vis, priority_og_smc)),
  #tar_target(priority_og_smc_pa_env, intersect_pa(priority_og_smc, clean_pa)),
  tar_target(og_smc, intersect_pa(caribou_cons_area, og_data)),
  tar_target(og_pa_smc, intersect_pa(og_smc, pa_smc)),
  tar_target(og_des_lands, intersect_pa(des_lands_smc, og_smc)),
  tar_target(og_des_lands_vis, intersect_pa(des_lands_smc_vis, og_smc)),
  #tar_target(og_pa_smc, intersect_pa(og_smc, clean_pa)),
  #bec og comparison
  #tar_target(bec_og_priority, intersect_pa(caribou_cons_area, bec_12)),
  #tar_target(bec_pa_og_priority, intersect_pa(pa_smc, bec_pa_og_priority)),
  #smc by ch areas
  tar_target(smc_ch, intersect_pa(caribou_cons_area, ch_data)),
  tar_target(smc_ch_pa, intersect_pa(pa_smc, ch_data)),
  tar_target(smc_ch_des_vis, intersect_pa(des_lands_smc_vis, ch_data)),
  #threat data
  tar_target(smc_forest, intersect_pa(caribou_cons_area, forest_ten)),
  tar_target(smc_mine_active, intersect_pa(caribou_cons_area, mining_active)),
  tar_target(smc_mine_potential, intersect_pa(caribou_cons_area, mining_tenure)),
  tar_target(smc_land, intersect_pa(caribou_cons_area, land_tenure)),
  tar_target(smc_eao, intersect_point_to_poly(caribou_cons_area, eao_points)),
  tar_target(smc_tenures, intersect_pa(caribou_cons_area, forestry_tenures)),
  tar_target(smc_harv, intersect_pa(caribou_cons_area, harv_auth)),
  #
  tar_target(smc_caribou_pa, intersect_pa(pa_smc, caribou_gcr_herd)),
  tar_target(smc_caribou_pa_des, intersect_pa(des_lands_smc, caribou_gcr_herd)),
  tar_target(smc_caribou_pa_uwr, intersect_pa(des_lands_smc_vis, caribou_gcr_herd)),

  tar_target(caribou_herd_pa, intersect_pa(clean_pa, caribou_full_herd)),

  tar_target(gcr_y2y, intersect_pa(caribou_cons_area, y2y)),
  tar_target(full_herd_y2y, intersect_pa(caribou_full_herd, y2y)),

  tar_target(pip_gcr, intersect_pa(pip, caribou_cons_area)),
  tar_target(pip_full_herd, intersect_pa(pip, caribou_full_herd))
)

process_data <- list(
  #tar_target(og_parks_removed, remove_pa(og_data, pa_tech)),
  tar_target(carbon_interior,carbon_analysis_interior(bec_og, bec_pa_og)),
  tar_target(carbon_coast, carbon_analysis_coastal(bec_og, bec_pa_og))

)


#summarize_data <- list(
 # tar_target(og_watershed_area, measure_og_watershed_area(og_data, og_watershed, prot_watershed))

# )



# targets pipeline --------------------------------------------------------
list(
  load_datasets,
  clean_data,
  intersect_data,
  #round_2_analyses,
  round_3_analyses,
  site_based_analyses,
  process_data
  #summarize_data
  #analyze_data,
  #plot_data,
  #save_csvs,
  #tar_render(report_html, "eco_rep_report.Rmd", output_format = "html_document"),
  #tar_render(report_pdf, "eco_rep_report.Rmd", output_format = "pdf_document")
)
#add list(,
#tar_targets() for each intermediate step of workflow)
