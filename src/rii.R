library(tidyverse); library(nlme)

# Calculate Relative Interaction Indices (RII)

read.csv("data/greenhouse.csv") %>% 
  group_by(species, competitor, salt, shade, tent, tray) %>% 
  summarise(y = mean(shoots + roots)) %>% 
  group_by() %>%
  spread(competitor, y) %>%
  mutate(RIImarsh = (`Panicum virgatum` - None) / (`Panicum virgatum` + None)) %>%
  select(species, shade, salt, tent, tray, RIImarsh) %>%
  rename(competitor = species) %>%
  na.omit -> others

read.csv("data/greenhouse.csv") %>%
  filter(species == "Panicum virgatum") %>%
  group_by(species, competitor, salt, shade, tent, tray) %>% 
  summarise(y = mean(shoots + roots)) %>% 
  group_by() %>%
  spread(competitor, y) %>%
  mutate(`Distichlis spicata` = (`Distichlis spicata` - None) / (`Distichlis spicata` + None),
         `Spartina alterniflora` = (`Spartina alterniflora` - None) / (`Spartina alterniflora` + None),
         `Spartina patens` = (`Spartina patens` - None) / (`Spartina patens` + None)) %>%
  select(shade, salt, tent, tray, `Distichlis spicata`:`Spartina patens`) %>%
  gather(competitor, RIIpanicum, `Distichlis spicata`:`Spartina patens`) %>%
  na.omit -> panicum

merge(others, panicum, by = c("competitor", "tray", "tent", "shade", "salt")) %>%
  arrange(competitor, shade, salt) %>%
  mutate(RII = RIIpanicum - RIImarsh) -> df2

# Selection of minimal adequate model

lme(RII ~ salt + shade + competitor + 
      salt:shade + salt:competitor + shade:competitor +
      salt:shade:competitor, 
    random = list(tent = ~1, tray = ~1), data = df2, method = "ML") -> m2.1 # Full model

lme(RII ~ salt + shade + competitor + 
      salt:shade + salt:competitor + shade:competitor, 
    random = list(tent = ~1, tray = ~1), data = df2, method = "ML") -> m2.2 # Model without 3x interaction

anova(m2.1, m2.2) # p > 0.05 => 3x interaction can be removed

anova(m2.2)

lme(RII ~ salt + shade + competitor, 
    random = list(tent = ~1, tray = ~1), data = df2, method = "ML") -> m2.3 # Model without 2x interaction

anova(m2.2, m2.3) # p > 0.05 => 2x interaction can be removed

anova(m2.3) # several main effects not significant

lme(RII ~ competitor, 
    random = list(tent = ~1, tray = ~1), data = df2, method = "ML") -> m2.4 # Model without n.s. main effects

anova(m2.3, m2.4) # p > 0.05 => main effects can be removed

# Minimal adequate model

car::Anova(m2.4, type = 3) -> Table2; Table2

rownames(Table2) <- c("(Intercept)", 
                      "Competitor")
Table2[-1, ] %>% 
  data.frame() %>%
  rownames_to_column("Term") %>%
  mutate(Chisq = round(Chisq, 3),
         Pr..Chisq. = round(Pr..Chisq., 3)) %>%
  rename(`Chi squared` = Chisq,
         df = Df,
         p = Pr..Chisq.) %>%
  mutate(p = ifelse(p == 0.000, "< 0.001", p)) %>%
  select(Term, df, `Chi squared`, p) %>%
  write.csv("results/Table2.csv", row.names = FALSE)

# RII figure

merge(others, panicum, by = c("competitor", "tray", "tent", "shade", "salt")) %>%
  arrange(competitor, shade, salt) %>%
  mutate(RII = RIIpanicum - RIImarsh) %>%
  group_by(competitor) %>%
  summarise(riim = mean(RII), se = sd(RII) / sqrt(length(RII))) %>%
  mutate(competitor = fct_relevel(competitor, c("Spartina alterniflora", "Spartina patens", "Distichlis spicata"))) %>%
  mutate(competitor = fct_recode(competitor, "S. alterniflora" = "Spartina alterniflora", 
                                 "S. patens" = "Spartina patens", 
                                 "D. spicata" = "Distichlis spicata")) %>%
  ggplot(aes(competitor, riim)) +
  geom_bar(stat = "identity", position = "dodge", fill = "aquamarine3") +
  geom_errorbar(aes(ymin = (riim - se), ymax = (riim + se)), width = 0.06, size = .8) +
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
        axis.text.x = element_text(size = 11, color = "black", face = "italic"),
        strip.placement = "outside") +
  geom_hline(yintercept = 0,linetype = "dashed") +
  guides(color = guide_legend(ncol = 2)) +
  xlab("Competitor species") + 
  ylab("Net Relative Interaction Index")-> f3; f3

ggsave(f3, file = "results/f3.png", 
       path = NULL, scale = 1, width = 85, height = 85, units = "mm", dpi = 600)
