library(tidyverse)

# Field dataset

read.csv("data/field.csv") -> df3

# Model

lm(wt ~ site * elevation, data = df3) -> m3

car::Anova(m3, type = 3) -> Table3; Table3

rownames(Table3) <- c("(Intercept)", "Site", "Location",
                      "Site:Location", "Residuals")
Table3[-c(1, 5), ] %>% 
  data.frame() %>%
  rownames_to_column("Term") %>%
  select(-Sum.Sq) %>%
  mutate(F.value = round(F.value, 3),
         Pr..F. = round(Pr..F., 3)) %>%
  rename(F = F.value,
         df = Df,
         p = Pr..F.) %>%
  mutate(p = ifelse(p == 0.000, "< 0.001", p)) %>%
  write.csv("results/Table3.csv", row.names = FALSE)

# Plot

df3 %>%
  group_by(elevation, site) %>%
  summarise(w = mean(wt), se = sd(wt) / sqrt(length(wt))) %>%
  mutate(elevation = as.factor(elevation)) %>%
  mutate(elevation = fct_relevel(elevation, c("low", "mid", "high"))) %>%
  mutate(elevation = as.numeric(elevation)) %>%
  ggplot(aes(elevation, w, color = site)) + 
  geom_line(size = 1.5) +
  geom_errorbar(aes(ymin = (w - se), ymax = (w + se)), width = 0.03, size = .6, alpha = 0.5) +
  scale_color_manual(values = c("gray11", "gray61", "purple")) + 
  scale_x_continuous(breaks = c(1, 2, 3), labels = c("Marsh", "Ecotone", "Forest")) +
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
  annotate("text", x = 1.6, y = 3.1, label = "Control East", color = "gray11") +
  annotate("text", x = 1.6, y = 4.4, label = "Control West", color = "gray61") +
  annotate("text", x = 1.6, y = 1.5, label = "Forest Cut", color = "purple") +
  geom_hline(yintercept = 0,linetype = "dashed") +
  guides(color = guide_legend(ncol = 2)) +
  xlab("Marsh-forest gradient") + 
  ylab("Biomass (g per 10x10 cm quadrat)") +
  labs(title = "D") -> f4; f4

ggsave(f4, file = "results/f4.png", 
       path = NULL, scale = 1, width = 85, height = 85, units = "mm", dpi = 600)

# Group figures

cowplot::plot_grid(f1, f2, f3, f4) -> f

ggsave(f, file = "results/f.png", 
       path = NULL, scale = 1, width = 6, height = 6, units = "in", dpi = 600)

# Posthoc

aov(wt ~ site * elevation, data = df3) -> m3B
summary(m3B)
TukeyHSD(m3B, conf.level=.95)
