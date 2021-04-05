library(tidyverse)

# Salinity

read.csv("data/measures.csv") %>%
  filter(competitor == "None") %>%
  group_by(species, shade, salt) %>%
  mutate(biomass = roots + shoots) %>%
  summarise(biomassm = mean(biomass), se = sd(biomass) / sqrt(length(biomass))) %>%
  ungroup -> df1

df1 %>%
  filter(salt == 0 & shade == 0) %>%
  rename(biomasst = biomassm) %>%
  select(species, biomasst) %>%
  merge(df1) %>%
  mutate(y = biomassm / biomasst, 
         ymin = (biomassm - se) / biomasst, 
         ymax = (biomassm + se) / biomasst) -> df2

  
df2 %>%
  filter(salt == 0) %>%
  mutate(treatment = "Shade (%)") %>%
  rename(value = shade) %>%
  select(species, treatment, value, y, ymin, ymax) -> df3

df2 %>%
  filter(shade == 0) %>%
  mutate(treatment = "Salinity (psu)") %>%
  rename(value = salt) %>%
  select(species, treatment, value, y, ymin, ymax) -> df4

rbind(df3, df4) %>%
  ggplot(aes(value, 100 * y, color = species)) +
  geom_line(size = 1.5) +
  facet_wrap(~ treatment, scales = "free_x", strip.position = "bottom") +
  geom_errorbar(aes(ymin = 100 * ymin, ymax = 100 * ymax), width = 0, size = .6, alpha = 0.5) +
  scale_color_manual(values = c("yellowgreen", "saddlebrown", 
                                "turquoise", "gold")) + 
  ggthemes::theme_tufte() +
  theme(legend.position = "top", 
        legend.direction = "vertical",
        legend.title = element_blank(),
        legend.text = element_text(size = 12, face = "italic"), 
        strip.text = element_text(size = 12), 
        panel.background = element_rect(color = "grey96", fill = "grey96"),
        axis.title.y = element_text(size = 12, color = "black"),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 12, color = "black"),
        axis.text.x = element_text(size = 12, color = "black"),
        strip.placement = "outside") +
  geom_hline(yintercept = 100,linetype = "dashed") +
  guides(color = guide_legend(ncol = 2)) +
  ylab("Biomass (% of zero-stress treatment)") -> f1

ggsave(f1, file = "results/strees.png", 
       path = NULL, scale = 1, width = 150, height = 120, units = "mm", dpi = 600)

# Competition figures

read.csv("data/measures.csv") %>%
  filter(salt == 0 & shade == 0) %>%
  mutate(competitor = ifelse(competitor == "None", "Alone", "In competition")) %>%
  group_by(species, competitor) %>%
  mutate(biomass = roots + shoots) %>%
  summarise(biomassm = mean(biomass), se = sd(biomass) / sqrt(length(biomass))) %>%
  ungroup ->
  df1

df1 %>%
  filter(competitor == "Alone") %>%
  rename(biomasst = biomassm) %>%
  select(species, biomasst) %>%
  merge(df1) %>%
  mutate(species = fct_relevel(species, c("Spartina alterniflora", "Spartina patens", "Distichlis spicata"))) %>%
  mutate(y = biomassm / biomasst, 
         ymin = (biomassm - se) / biomasst, 
         ymax = (biomassm + se) / biomasst) %>%
  ggplot(aes(species, 100 * y, fill = competitor)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = 100 * ymin, ymax = 100 * ymax), width = 0, size = 1, alpha = 0.5, position = position_dodge(.9)) +
  scale_fill_manual(values = c("forestgreen", "gold"), labels = c("Grown alone", "In competition")) + 
  ggthemes::theme_tufte() +
  theme(legend.position = "top", 
        legend.title = element_blank(),
        legend.text = element_text(size = 12, face = "italic"), 
        strip.text = element_text(size = 12), 
        strip.text.y = element_text(face = "italic"),
        panel.background = element_rect(color = "grey96", fill = "grey96"),
        axis.title.y = element_text(size = 12, color = "black"),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 12, color = "black"),
        axis.text.x = element_text(size = 12, color = "black", face = "italic"),
        strip.placement = "outside") +
  geom_hline(yintercept = 100,linetype = "dashed") +
  guides(color = guide_legend(ncol = 2)) +
  ylab("Biomass (% of grown-alone treatment)") -> f2

ggsave(f2, file = "results/competition.png", 
       path = NULL, scale = 1, width = 150, height = 85, units = "mm", dpi = 600)

# Competition and figures

read.csv("data/measures.csv") %>%
  mutate(competitor = ifelse(competitor == "None", "Alone", "In competition")) %>%
  group_by(species, competitor, shade, salt) %>%
  mutate(biomass = roots + shoots) %>%
  summarise(biomassm = mean(biomass), se = sd(biomass) / sqrt(length(biomass))) %>%
  ungroup -> df1

df1 %>%
  filter(salt == 0 & shade == 0 & competitor == "Alone") %>%
  rename(biomasst = biomassm) %>%
  select(species, biomasst) %>%
  merge(df1) %>%
  mutate(species = fct_relevel(species, c("Spartina alterniflora", "Spartina patens", "Distichlis spicata"))) %>%
  mutate(shade = fct_recode(as.factor(shade), "0% shade" = "0", "75% shade" = "75", "90% shade" = "90")) %>%
  mutate(y = biomassm / biomasst, 
         ymin = (biomassm - se) / biomasst, 
         ymax = (biomassm + se) / biomasst) %>%
  ggplot(aes(salt, 100 * y, color = competitor)) +
  geom_line(size = 1.5) +
  facet_grid(species ~ shade, scales = "free_y") +
  geom_errorbar(aes(ymin = 100 * ymin, ymax = 100 * ymax), width = 0, size = .6, alpha = 0.5) +
  scale_color_manual(values = c("steelblue1", "firebrick"), labels = c("Grown alone", "With Panicum virgatum")) + 
  ggthemes::theme_tufte() +
  theme(legend.position = "top", 
        legend.direction = "vertical",
        legend.title = element_blank(),
        legend.text = element_text(size = 12, face = "italic"), 
        strip.text = element_text(size = 12), 
        strip.text.y = element_text(face = "italic"),
        panel.background = element_rect(color = "grey96", fill = "grey96"),
        axis.title.y = element_text(size = 12, color = "black"),
        axis.title.x = element_text(size = 12, color = "black"),
        axis.text.y = element_text(size = 12, color = "black"),
        axis.text.x = element_text(size = 12, color = "black"),
        strip.placement = "outside") +
  geom_hline(yintercept = 100,linetype = "dashed") +
  guides(color = guide_legend(ncol = 2)) +
  xlab("Salinity (psu)") + 
  ylab("Biomass (% of zero-stress grown-alone treatment)") -> f3

ggsave(f3, file = "results/competition_stress.png", 
       path = NULL, scale = 1, width = 150, height = 190, units = "mm", dpi = 600)


read.csv("data/measures.csv") %>%
  ggplot(aes(x = shoots, y = roots, color = as.factor(salt))) + 
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~ species)
  geom_text(aes(label = round(y, 1))) +
  facet_grid(species ~ competitor) +
  scale_fill_gradient(high = "green", low = "red") -> f4

ggsave(f4, file = "results/tiles.png", 
       path = NULL, scale = 1, width = 150, height = 190, units = "mm", dpi = 600)
