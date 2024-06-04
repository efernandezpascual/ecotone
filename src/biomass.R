library(tidyverse); library(nlme)

# Biomass dataset

read.csv("data/greenhouse.csv") %>%
  mutate(competitor = ifelse(competitor == "None", "Alone", "In competition")) %>%
  mutate(b = shoots + roots) -> df1

# Selection of the minimal adequate model

lme(b ~ salt + shade + species + competitor + 
      salt:shade + salt:species + shade:species + salt:competitor + shade:competitor + species:competitor +
      salt:shade:species + salt:shade:competitor + salt:species:competitor + shade:species:competitor +
      salt:shade:species:competitor, 
    random = list(tent = ~1, tray = ~1), data = df1, method = "ML") -> m1.1 # Full model

lme(b ~ salt + shade + species + competitor + 
      salt:shade + salt:species + shade:species + salt:competitor + shade:competitor + species:competitor +
      salt:shade:species + salt:shade:competitor + salt:species:competitor + shade:species:competitor, 
    random = list(tent = ~1, tray = ~1), data = df1, method = "ML") -> m1.2 # Model without 4x interaction

anova(m1.1, m1.2) # p > 0.05 => 4x interaction can be removed

lme(b ~ salt + shade + species + competitor + 
      salt:shade + salt:species + shade:species + salt:competitor + shade:competitor + species:competitor, 
    random = list(tent = ~1, tray = ~1), data = df1, method = "ML") -> m1.3 # Model without 3x interaction

anova(m1.2, m1.3) # p > 0.05 => 3x interaction can be removed

anova(m1.3) # several 2x interactions not significant

lme(b ~ salt + shade + species + competitor + 
      salt:species + shade:competitor, 
    random = list(tent = ~1, tray = ~1), data = df1, method = "ML") -> m1.4 # Model without n.s. 2x interactions

anova(m1.3, m1.4) # p > 0.05 => interactions can be removed

# Minimal adequate model

car::Anova(m1.4, type = 3) -> Table1; Table1

rownames(Table1) <- c("(Intercept)", "Salinity", "Shade", "Species", 
                      "Competition", "Salinity:Species", "Shade:Competition")
Table1[-1, ] %>% 
  data.frame() %>%
  rownames_to_column("Term") %>%
  mutate(Chisq = round(Chisq, 3),
         Pr..Chisq. = round(Pr..Chisq., 3)) %>%
  rename(`Chi squared` = Chisq,
         df = Df,
         p = Pr..Chisq.) %>%
  mutate(p = ifelse(p == 0.000, "< 0.001", p)) %>%
  select(Term, df, `Chi squared`, p) %>%
  write.csv("results/Table1.csv", row.names = FALSE)

# Salinity x species figure

read.csv("data/greenhouse.csv") %>%
  filter(competitor == "None" & shade == 0) %>%
  group_by(species, shade, salt) %>%
  mutate(biomass = roots + shoots) %>%
  summarise(biomassm = mean(biomass), se = sd(biomass) / sqrt(length(biomass))) %>%
  ungroup -> df1.1

df1.1 %>%
  filter(salt == 0) %>%
  rename(biomasst = biomassm) %>%
  select(species, biomasst) %>%
  merge(df1.1) %>%
  mutate(y = biomassm / biomasst, 
         ymin = (biomassm - se) / biomasst, 
         ymax = (biomassm + se) / biomasst) %>%
  ggplot(aes(salt, 100 * y, color = species)) +
  geom_line(size = 1.5) +
  geom_errorbar(aes(ymin = 100 * ymin, ymax = 100 * ymax), width = 0.08, size = .6, alpha = 0.5) +
  scale_color_manual(values = c("gold", "saddlebrown", 
                                "deepskyblue", "yellowgreen")) + 
  scale_x_continuous(breaks = c(0, 3, 6)) +
  ggthemes::theme_tufte() +
  theme(legend.position = "none", 
        legend.direction = "vertical",
        legend.title = element_blank(),
        legend.text = element_text(size = 12, face = "italic"), 
        strip.text = element_text(size = 12), 
        panel.background = element_rect(color = "grey96", fill = "grey96"),
        axis.title.y = element_text(size = 12, color = "black"),
        axis.title.x = element_text(size = 12, color = "black"),
        axis.text.y = element_text(size = 12, color = "black"),
        axis.text.x = element_text(size = 12, color = "black"),
        strip.placement = "outside") +
  annotate("text", x = 4.8, y = 170, label = "S. alterniflora", color = "deepskyblue", fontface = "italic") +
  annotate("text", x = 4.8, y = 110, label = "D. spicata", color = "gold", fontface = "italic") +
  annotate("text", x = 4.8, y = 85, label = "S. patens", color = "yellowgreen", fontface = "italic") +
  annotate("text", x = 4.8, y = 60, label = "P. virgatum", color = "saddlebrown", fontface = "italic") +
  geom_hline(yintercept = 100,linetype = "dashed") +
  guides(color = guide_legend(ncol = 2)) +
  xlab("Salinity (psu)") + 
  ylab("Biomass (% of zero-stress treatment)") +
  labs(title = "A")  -> f1; f1

ggsave(f1, file = "results/f1.png", 
       path = NULL, scale = 1, width = 85, height = 85, units = "mm", dpi = 600)

# Competition x shade figure

read.csv("data/greenhouse.csv") %>%
  filter(salt == 0) %>%
  mutate(competitor = ifelse(competitor == "None", "Alone", "In competition")) %>%
  group_by(shade, competitor) %>%
  mutate(biomass = roots + shoots) %>%
  summarise(biomassm = mean(biomass), se = sd(biomass) / sqrt(length(biomass))) %>%
  ungroup ->
  df1.2

df1.2 %>%
  filter(shade == "0" & competitor == "Alone") %>%
  rename(biomasst = biomassm) %>%
  select(biomasst) %>%
  merge(df1.2) %>%
  mutate(y = biomassm / biomasst, 
         ymin = (biomassm - se) / biomasst, 
         ymax = (biomassm + se) / biomasst) %>%
  ggplot(aes(shade, 100 * y, color = competitor)) +
  geom_line(size = 1.5) +
  geom_errorbar(aes(ymin = 100 * ymin, ymax = 100 * ymax), width = 1.2, size = .6) +
  scale_color_manual(values = c("forestgreen", "violetred1")) + 
  scale_x_continuous(breaks = c(0, 75, 90)) +
  ggthemes::theme_tufte() +
  theme(legend.position = "none", 
        legend.direction = "vertical",
        legend.title = element_blank(),
        legend.text = element_text(size = 12, face = "italic"), 
        strip.text = element_text(size = 12), 
        panel.background = element_rect(color = "grey96", fill = "grey96"),
        axis.title.y = element_text(size = 12, color = "black"),
        axis.title.x = element_text(size = 12, color = "black"),
        axis.text.y = element_text(size = 12, color = "black"),
        axis.text.x = element_text(size = 12, color = "black"),
        strip.placement = "outside") +
  annotate("text", x = 20, y = 105, label = "Growing alone", color = "forestgreen") +
  annotate("text", x = 20, y = 83, label = "In competition", color = "violetred1") +
  geom_hline(yintercept = 100,linetype = "dashed") +
  guides(color = guide_legend(ncol = 2)) +
  xlab("Shade (%)") + 
  ylab("Biomass (% of zero-stress treatment)") +
  labs(title = "B") -> f2; f2

ggsave(f2, file = "results/f2.png", 
       path = NULL, scale = 1, width = 85, height = 85, units = "mm", dpi = 600)
