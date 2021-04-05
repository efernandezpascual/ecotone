library(tidyverse); library(nlme)

read.csv("data/measures.csv") %>%
  mutate(competitor = ifelse(competitor == "None", "Alone", "In competition")) %>%
  mutate(b = shoots + roots) -> df1

lme(b ~ salt + shade + species + competitor + 
      salt:shade + salt:species + shade:species + salt:competitor + shade:competitor + species:competitor +
      salt:shade:species + salt:shade:competitor + salt:species:competitor + shade:species:competitor +
      salt:shade:species:competitor, 
    random = list(tent = ~1, tray = ~1), data = df1, method = "ML") -> m1 # Full model

lme(b ~ salt + shade + species + competitor + 
      salt:shade + salt:species + shade:species + salt:competitor + shade:competitor + species:competitor +
      salt:shade:species + salt:shade:competitor + salt:species:competitor + shade:species:competitor, 
    random = list(tent = ~1, tray = ~1), data = df1, method = "ML") -> m2 # Model without 4x interaction

anova(m1, m2) # p > 0.05 => 4x interaction can be removed

lme(b ~ salt + shade + species + competitor + 
      salt:shade + salt:species + shade:species + salt:competitor + shade:competitor + species:competitor, 
    random = list(tent = ~1, tray = ~1), data = df1, method = "ML") -> m3 # Model without 3x interaction

anova(m2, m3) # p > 0.05 => 3x interaction can be removed

anova(m3) # several 2x interactions not significant

lme(b ~ salt + shade + species + competitor + 
      salt:species + shade:competitor, 
    random = list(tent = ~1, tray = ~1), data = df1, method = "ML") -> m4 # Model without n.s. 2x interactions

anova(m3, m4) # p > 0.05 => interactions can be removed

anova(m4)
