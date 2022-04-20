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

library(dplyr)
library(sf)


install.packages("bcmaps")
install.packages("sf")
library(bcmaps)
library(sf)

cities <- bcmaps::bc_cities()
st_write(cities, "insertfilepath.shp")

### Critical habitat attributes

ch <- st_read("CriticalHabitat.gdb", layer='CriticalHabitats_0826', crs=3005)

ch_area <- ch %>%
  rename_all(tolower) %>%
  st_cast(to = "MULTIPOLYGON", warn = FALSE) %>%
  st_make_valid() %>%
  st_transform(st_crs(3005)) %>%
  mutate(area= as.numeric(st_area(.)),
         area = set_units(area, km^2)) %>%
  st_set_geometry(NULL) %>%
  group_by(scientific_name, common_name_english) %>%
  summarise(area_by_species = as.numeric(sum(area))) %>%
  ungroup()

ch_attributes_long <- ch %>%
  rename_all(tolower) %>%
  st_set_geometry(NULL)

ch_attributes_short <- ch %>%
  rename_all(tolower) %>%
  st_set_geometry(NULL) %>%
  group_by(cosewic_species_id, scientific_name, common_name_english) %>%
  summarise(area=sum(shape_area)) %>%
  ungroup()

write.csv(ch_attributes_long, "ch_attributes.csv")

write.csv(ch_attributes_short, "ch_attributes_names.csv")
