library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(magrittr)
library(readr)
library(tidyr)
library(stringr)
library(scales)
library(ggpubr)


source('packages.R')
conflict_prefer("filter", "dplyr")

oecm_info <- read_csv("designations-info.csv")

du9_des_summary_overlapping <- read_csv("out/des_lands_overlapping_du9.csv")%>%
  left_join(oecm_info, by = "designation") %>%
  filter(oecm_status == "Not OECM") %>%
  filter(priority_zone_type == "High") %>%
  mutate(critical_habitat_type = factor(critical_habitat_type, labels = c("HEWSR", "Matrix"))) %>%
  filter()

caribou_des <- ggplot(du9_des_summary_overlapping, aes(fill = critical_habitat_type, y = des_area_type,
                                                 x= reorder(designation, des_area_herd)))+
  geom_bar(position="stack", stat="identity") +
  ylab("Area in Hectares") +
  xlab("")+
  ggtitle("Non-OECM Designations co-located in High Priority DU9 Areas")+
  scale_fill_brewer(palette = "Set2")+
  scale_y_continuous(labels = comma)+
  theme_bw()+
  theme(legend.position= "bottom")+
  theme(legend.title= element_blank())+
  #theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  coord_flip()+
  facet_grid(rows=vars(herd), scales="free_y", space="free")

caribou_des


ggsave("out/du9/Designations_High-Priority_du9.jpg", units="in", width=9, height=12, dpi=300)


du9_des_summary_flat <- read_csv("out/des_lands_flat_du9.csv")

remove = c("wha_no_harvest", "ogma_legal", "muskwa_kechika_special_wildland", "wildland_area", "flathead")

order = c("Full", "High", "Medium", "Low", "None")

du9_restriction_summary <- du9_des_summary_flat %>%
  filter(!str_detect(designations, "wha_no_harvest")) %>%
  filter(!str_detect(designations, "ogma_legal")) %>%
  filter(!str_detect(designations, "muskwa_kechika_special_wildland")) %>%
  filter(!str_detect(designations, "wildland_area")) %>%
  filter(!str_detect(designations, "flathead")) %>%
  filter(forest_restriction_max != 5 & mine_restriction_max != 5 & og_restriction_max != 5) %>%
  group_by(herd, critical_habitat_type, priority_zone_type, forest_restriction_max, mine_restriction_max,
           og_restriction_max, Group_Description) %>%
  summarise(sum_restriction_type = sum(des_area_type))%>%
  mutate(forest_restriction_max = case_when(forest_restriction_max == 0 ~ "None",
                                            forest_restriction_max == 1 ~ "Low",
                                            forest_restriction_max == 2 ~ "Medium",
                                            forest_restriction_max == 3 ~ "High",
                                            forest_restriction_max == 4 ~ "Full")) %>%
  mutate(forest_restriction_max = factor(forest_restriction_max, levels=order)) %>%
  mutate(mine_restriction_max = case_when(mine_restriction_max == 0 ~ "None",
                                          mine_restriction_max == 1 ~ "Low",
                                          mine_restriction_max == 2 ~ "Medium",
                                          mine_restriction_max == 3 ~ "High",
                                          mine_restriction_max == 4 ~ "Full")) %>%
  mutate(mine_restriction_max = factor(mine_restriction_max, levels=order)) %>%
  mutate(og_restriction_max = case_when(og_restriction_max == 0 ~ "None",
                                        og_restriction_max == 1 ~ "Low",
                                        og_restriction_max == 2 ~ "Medium",
                                        og_restriction_max == 3 ~ "High",
                                        og_restriction_max == 4 ~ "Full")) %>%
  mutate(og_restriction_max = factor(og_restriction_max, levels=order)) %>%
  filter(priority_zone_type == "High")

write_csv(du9_restriction_summary, "out/du9/du9-restriction-level-summary.csv")

priority_order = c("High", "Medium", "Low", "Other")

du9_restriction_forestry <- du9_des_summary_flat %>%
  filter(!str_detect(designations, "wha_no_harvest")) %>%
  filter(!str_detect(designations, "ogma_legal")) %>%
  filter(!str_detect(designations, "muskwa_kechika_special_wildland")) %>%
  filter(!str_detect(designations, "wildland_area")) %>%
  filter(!str_detect(designations, "flathead")) %>%
  filter(forest_restriction_max != 5 & mine_restriction_max != 5 & og_restriction_max != 5) %>%
  group_by(herd, critical_habitat_type, priority_zone_type, forest_restriction_max) %>%
  summarise(sum_restriction_type = round(sum(des_area_type), digits=0))%>%
  mutate(forest_restriction_max = case_when(forest_restriction_max == 0 ~ "None",
                                            forest_restriction_max == 1 ~ "Low",
                                            forest_restriction_max == 2 ~ "Medium",
                                            forest_restriction_max == 3 ~ "High",
                                            forest_restriction_max == 4 ~ "Full")) %>%
  mutate(forest_restriction_max = factor(forest_restriction_max, levels=order),
                priority_zone_type = factor(priority_zone_type, levels=priority_order)) %>%
  arrange(herd, critical_habitat_type, priority_zone_type, forest_restriction_max, desc(sum_restriction_type)) %>%
  filter(forest_restriction_max != "None") %>%
  pivot_wider(id_cols = c(herd, critical_habitat_type, priority_zone_type),
                          names_from = forest_restriction_max, values_from=sum_restriction_type) %>%
  relocate(Full, .before = High) %>%
  mutate(across(where(is.numeric), ~replace(.,is.na(.),0)))

write_csv(du9_restriction_forestry, "out/du9/du9-forest-restriction-level-summary.csv")

caribou_restriction <- ggplot(du9_restriction_summary, aes(fill = critical_habitat_type, y = sum_restriction_type,
                                                       x= reorder(herd, sum_restriction_type)))+
  geom_bar(position="stack", stat="identity") +
  ylab("Area in Hectares") +
  xlab("")+
  ggtitle("Existing Forestry Restrictions in High Priority DU9 Areas")+
  scale_fill_brewer(palette = "Set2")+
  scale_y_continuous(labels = comma)+
  theme_bw()+
  theme(legend.position= "bottom")+
  theme(legend.title= element_blank())+
  #theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  coord_flip()+
  facet_grid(rows=vars(forest_restriction_max), scales="free_y", space="free")

caribou_restriction

ggsave("out/du9/Forestry-Restriction_High-Priority_du9.jpg", units="in", width=11, height=7, dpi=300)
