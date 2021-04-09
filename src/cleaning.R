library(tidyverse)

# Clean greenhouse data

openxlsx::read.xlsx("data/plant_weights.xlsx", sheet = 5) %>%
  na.omit %>% # Remove bad rows
  merge(read.csv("data/plant_assignments.csv")) %>% # Add treatment labels
  group_by(spp, competitor, tag_no) %>%
  filter(length(tag_spp) == 1) %>% # Remove duplicated tags
  rename(tent = shade) %>% # Each of 12 tents (4 x 3 levels of shade)
  rename(tray = set_no) %>% # Each of 72 trays (6 x 12 tents)
  rename(species = spp) %>%
  mutate(shade = gsub("[[:digit:]]+", "", tent)) %>% # Obtain shade values from tent
  ungroup() %>%
  select(species, competitor, shade, salt, tent, tray, roots, shoots) %>%
  mutate(shade = fct_recode(shade, "0" = "C", "75" = "S", "90" = "D")) %>%
  mutate(species = fct_recode(species, 
                              "Distichlis spicata" = "Ds", 
                              "Phragmites australis" = "Pa", 
                              "Spartina alterniflora" = "Sa", 
                              "Spartina patens" = "Sp", 
                              "Schoenoplectus pungens" = "Spu", 
                              "Panicum virgatum" = "Pv")) %>%
  mutate(competitor = fct_recode(competitor, 
                                 "None" = "none",
                                 "Distichlis spicata" = "Ds", 
                                 "Phragmites australis" = "Pa", 
                                 "Spartina alterniflora" = "Sa", 
                                 "Spartina patens" = "Sp", 
                                 "Schoenoplectus pungens" = "Spu", 
                                 "Panicum virgatum" = "Pv")) %>%
  arrange(species, competitor, salt, shade, tray) %>%
  filter(! species %in% c("Phragmites australis", "Schoenoplectus pungens")) %>%
  filter(! competitor %in% c("Phragmites australis", "Schoenoplectus pungens")) %>%
  write.csv("data/greenhouse.csv", row.names = FALSE)

# Clean field data

openxlsx::read.xlsx("data/Biomass samples 2Jun15.xlsx") %>%
  group_by(site, elevation, rep) %>%
  summarise(wt = sum(wt, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(elevation = fct_relevel(elevation, c("low", "mid", "high"))) %>%
  mutate(site = fct_recode(site, "Control East" = "ctrl east", 
                           "Control West" = "ctrl west", 
                           "Forest Cut" = "cut")) %>%
  write.csv("data/field.csv", row.names = FALSE)
