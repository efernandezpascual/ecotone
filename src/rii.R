library(tidyverse); library(nlme)

# Calculate Relative Interaction Indices (RII)

read.csv("data/greenhouse.csv") %>% 
  group_by(species, competitor, salt, shade, tent, tray) %>% 
  summarise(y = mean(shoots + roots)) %>% 
  group_by() %>%
  spread(competitor, y) %>%
  mutate(RII = (`Panicum virgatum` - None) / (`Panicum virgatum` + None)) %>%
  select(species, shade, salt, tent, tray, RII) %>%
  rename(competitor = species) %>%
  na.omit %>%
  arrange(competitor, shade, salt) %>%
  mutate(salt = as.factor(salt), shade = as.factor(shade)) -> df2

# Linear mixed model

lme(RII ~ salt * shade * competitor, 
    random = list(tent = ~1, tray = ~1), data = df2, method = "ML") -> m1 # Fully factorial model

car::Anova(m1, type = 3)

lme(RII ~ salt + shade + competitor +
      salt:shade + salt:competitor + shade:competitor, 
    random = list(tent = ~1, tray = ~1), data = df2, method = "ML") -> m2

anova(m1, m2) # 3x interaction can be dropped

car::Anova(m2, type = 3)

lme(RII ~ salt + shade + competitor, 
    random = list(tent = ~1, tray = ~1), data = df2, method = "ML") -> m3

anova(m2, m3) # 2x interactions can be dropped

car::Anova(m3, type = 3)

lme(RII ~ shade + salt, 
    random = list(tent = ~1, tray = ~1), data = df2, method = "ML") -> m4

anova(m3, m4) # competitor can be dropped

car::Anova(m4, type = 3)

lme(RII ~ shade, 
    random = list(tent = ~1, tray = ~1), data = df2, method = "ML") -> m5

anova(m4, m5) # competitor can be dropped

car::Anova(m5, type = 3) -> Table2; Table2

rownames(Table2) <- c("(Intercept)", "Shade")

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

## Figure

df2 %>%
  filter(salt == 0) %>%
  group_by(shade) %>%
  summarise(rii = mean(RII), se = sd(RII) / sqrt(length(RII))) %>%
  ggplot(aes(shade, rii)) +
  geom_bar(stat = "identity", position = "dodge", size = 1.5, fill = "aquamarine3") +
  geom_errorbar(aes(ymin = rii - se, ymax = rii + se), width = 0.05, size = .6) +
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
  geom_hline(yintercept = 0,linetype = "dashed") +
  guides(color = guide_legend(ncol = 2)) +
  xlab("Shade (%)") + 
  ylab("Relative Interaction Index") +
  labs(title = "C")  -> f3; f3

ggsave(fa, file = "results/f3.png", 
       path = NULL, scale = 1, width = 85, height = 85, units = "mm", dpi = 600)
