library(dplyr)
library(tidyr)
library(broom)
library(ggplot2)
library(cowplot)
library(igraph)

gh <- read.table("greenhouse.txt", header = T)

### BIOMASS

## Type III anova tables for total biomass

options(contrasts = c("contr.helmert", "contr.poly"))

models <- gh %>% 
          group_by(Species) %>%
          do(fits = drop1(aov(total ~ Neighbor * Shade * Salt, .), .~., test = "F")) 

anovas <- tidy(models, fits) %>%
            data.frame %>%
            mutate(Significance = ifelse(p.value > 0.1, "NS", 
                                  ifelse(p.value > 0.05, ".",
                                  ifelse(p.value > 0.01, "*",
                                  ifelse(p.value > 0.001, "**", "***")))))
write.csv(anovas, "BiomassANOVAfull.csv") 

na.omit(anovas) %$% 
  matrix(Significance, ncol = length(unique(term)), byrow = T, 
         dimnames = list(unique(Species), unique(term))) %>%
  write.csv("BiomassANOVA.csv")

## Minimal adequate models diagnostic plots

par(mfrow = c(2, 2))
gh %>% subset(Species == "Ds") %>% aov(total ~ Neighbor * Shade, .) %>% plot
gh %>% subset(Species == "Pa") %>% aov(total ~ Neighbor * Shade, .) %>% plot
gh %>% subset(Species == "Pv") %>% aov(total ~ Neighbor + Shade + Salt + Neighbor:Salt, .) %>% plot
gh %>% subset(Species == "Sa") %>% aov(total ~ Neighbor * Shade, .) %>% plot
gh %>% subset(Species == "Sp") %>% aov(total ~ Neighbor + Shade, .) %>% plot
gh %>% subset(Species == "Spu") %>% aov(total ~ Neighbor + Shade + Salt, .) %>% plot
     
## Biomass figures

theme_set(theme_cowplot(font_size = 8.5))

gh$Neilab <- gh$Neighbor 
levels(gh$Neilab) <- c("Alone", "D. spicata", "Ph. australis", "P. virgatum", "S. alterniflora", "S. patens", "Sch. pungens")

nxsh <- function(df) {df %>%
                      group_by(Neilab, Shade) %>%
                      summarise(Biomass = mean(total), SE = (sd(total) / sqrt(length(total)))) %>%
                      ggplot(., aes(y = Biomass, x = Shade, fill = Shade, ymax = Biomass + 1.96*SE, ymin = Biomass - 1.96*SE)) + 
                      geom_col(position = "dodge") +
                      facet_wrap(~ Neilab, ncol = 5) + 
                      geom_errorbar(width = 0.2, lwd = 0.7, position = position_dodge(width = 0.9)) +
                      scale_fill_manual(values = c("gold", "goldenrod", "goldenrod4"), name = "Shade (%)", labels = c("0", "75", "90")) +
                      theme(axis.text.x = element_blank(), axis.title.x = element_blank(), legend.position = "none",
                            axis.ticks.x = element_blank(), plot.title = element_text(face = "italic"), strip.text = element_text(face = "italic")) +   
                      scale_x_discrete(name = "Shade (%)", labels = c("control" = "0", "shade1" = "75", "shade2" = "90"))}

nxshxsa <- function(df) {df %>%
                      group_by(Neilab, Shade, Salt) %>%
                      summarise(Biomass = mean(total), SE = (sd(total) / sqrt(length(total)))) %>%
                      ggplot(., aes(y = Biomass, x = Salt, fill = Shade, ymax = Biomass + 1.96*SE, ymin = Biomass - 1.96*SE)) + 
                      geom_col(position = "dodge") +
                      facet_wrap(~ Neilab, ncol = 5) + 
                      geom_errorbar(width = 0.2, lwd = 0.7, position = position_dodge(width = 0.9)) + 
                      scale_fill_manual(values = c("gold", "goldenrod", "goldenrod4")) +
                      theme(plot.title = element_text(face = "italic"), legend.position = "none", strip.text = element_text(face = "italic")) +  
                      scale_x_discrete(name = "Salinity (ppt)", labels = c("0ppt" = "0", "3ppt" = "3", "6ppt" = "6"))}

Dsb <- gh %>% subset(Species == "Ds") %>% nxsh + labs(titles = "Distichlis spicata") +
              scale_y_continuous(name = "Biomass (g)", limits = c(0, 9.5), breaks = c(0, 4, 8)) 
Spub <- gh %>% subset(Species == "Spu") %>% nxshxsa + labs(titles = "Schoenoplectus pungens") +
              scale_y_continuous(name = "Biomass (g)", limits = c(0, 12.5), breaks = c(0, 5, 10))
Sab <- gh %>% subset(Species == "Sa") %>% nxsh + labs(titles = "Spartina alterniflora") +
              scale_y_continuous(name = "Biomass (g)", limits = c(0, 11.5), breaks = c(0, 5, 10)) 
Spb <- gh %>% subset(Species == "Sp") %>% nxsh + labs(titles = "Spartina patens") +
              scale_y_continuous(name = "Biomass (g)", limits = c(0, 11.5), breaks = c(0, 5, 10))     
Pab <- gh %>% subset(Species == "Pa") %>% nxsh + labs(titles = "Phragmites australis") +
              scale_y_continuous(name = "Biomass (g)", limits = c(-0.7, 5), breaks = c(0, 2, 4))  
Pvb <- gh %>% subset(Species == "Pv") %>% nxshxsa + labs(titles = "Panicum virgatum") +
              scale_y_continuous(name = "Biomass (g)", limits = c(0, 13.5), breaks = c(0, 6, 12)) 

bioplot <- plot_grid(get_legend(Dsb + theme(legend.position = "top", legend.justification = "center")),
                     plot_grid(Dsb, Spub, Sab, Spb, ncol = 2), plot_grid(Pab, Pvb, ncol = 1),
                     ncol = 1, rel_heights = c(1, 12, 12))

ggsave(filename = "Biomass.tiff", plot = last_plot(),
       path = NULL, scale = 1, width = 167.64, height = 167.64, units = "mm", dpi = 600)


### ROOTS:SHOOTS RATIO

## Type III anova tables for total biomass

options(contrasts = c("contr.helmert", "contr.poly"))

models <- gh[-c(224, 225), ] %>% 
          group_by(Species) %>%
          do(fits = drop1(aov(rootshoots ~ Neighbor * Shade * Salt, .), .~., test = "F")) 

anovas <- tidy(models, fits) %>%
            data.frame %>%
            mutate(Significance = ifelse(p.value > 0.1, "NS", 
                                  ifelse(p.value > 0.05, ".",
                                  ifelse(p.value > 0.01, "*",
                                  ifelse(p.value > 0.001, "**", "***")))))
write.csv(anovas, "RatioANOVAfull.csv") 

na.omit(anovas) %$% 
  matrix(Significance, ncol = length(unique(term)), byrow = T, 
         dimnames = list(unique(Species), unique(term))) %>%
  write.csv("RatioANOVA.csv")

## Minimal adequate models diagnostic plots

par(mfrow = c(2, 2))
gh %>% subset(Species == "Ds") %>% aov(rootshoots ~ Neighbor + Shade, .) %>% plot
gh[-c(224, 225), ] %>% subset(Species == "Pa") %>% aov(rootshoots ~ Neighbor * Shade * Salt, .) %>% plot
# Points 224 and 225 affected heavily the model and I removed them (although nothing changes)
gh %>% subset(Species == "Pv") %>% aov(rootshoots ~ Neighbor + Shade, .) %>% plot
gh %>% subset(Species == "Sa") %>% aov(rootshoots ~ Neighbor + Shade, .) %>% plot
gh %>% subset(Species == "Sp") %>% aov(rootshoots ~ Neighbor + Shade, .) %>% plot
gh %>% subset(Species == "Spu") %>% aov(rootshoots ~ Neighbor + Shade, .) %>% plot
     
## Ratio figures

theme_set(theme_cowplot(font_size=8.5))

gh$Neilab <- gh$Neighbor 
levels(gh$Neilab) <- c("Alone", "D. spicata", "Ph. australis", "P. virgatum", "S. alterniflora", "S. patens", "Sch. pungens")

Rnxsh <- function(df) {df %>%
                      group_by(Neilab, Shade) %>%
                      summarise(Ratio = mean(rootshoots), SE = (sd(rootshoots) / sqrt(length(rootshoots)))) %>%
                      ggplot(., aes(y = Ratio, x = Shade, fill = Shade, ymax = Ratio + 1.96*SE, ymin = Ratio - 1.96*SE)) + 
                      geom_col(position = "dodge") +
                      facet_wrap(~ Neilab, ncol = 5) + 
                      geom_errorbar(width = 0.2, lwd = 0.7, position = position_dodge(width = 0.9)) +
                      scale_fill_manual(values = c("seagreen1", "seagreen3", "seagreen4"), name = "Shade (%)", labels = c("0", "75", "90")) +
                      theme(axis.text.x = element_blank(), axis.title.x = element_blank(), legend.position = "none",
                            axis.ticks.x = element_blank(), plot.title = element_text(face = "italic"), strip.text = element_text(face = "italic")) +   
                      scale_x_discrete(name = "Shade (%)", labels = c("control" = "0", "shade1" = "75", "shade2" = "90")) +
                      geom_hline(yintercept = 1)}

Rnxshxsa <- function(df) {df %>%
                      group_by(Neilab, Shade, Salt) %>%
                      summarise(Ratio = mean(rootshoots), SE = (sd(rootshoots) / sqrt(length(rootshoots)))) %>%
                      ggplot(., aes(y = Ratio, x = Salt, fill = Shade, ymax = Ratio + 1.96*SE, ymin = Ratio - 1.96*SE)) + 
                      geom_col(position = "dodge") +
                      facet_wrap(~ Neilab, ncol = 5) + 
                      geom_errorbar(width = 0.2, lwd = 0.7, position = position_dodge(width = 0.9)) + 
                      scale_fill_manual(values = c("seagreen1", "seagreen3", "seagreen4")) +
                      theme(plot.title = element_text(face = "italic"), legend.position = "none", strip.text = element_text(face = "italic")) +  
                      scale_x_discrete(name = "Salinity (ppt)", labels = c("0ppt" = "0", "3ppt" = "3", "6ppt" = "6")) +
                      geom_hline(yintercept = 1)}

# These are to see the limits of the plotting area
gh[-c(224, 225), ] %>% group_by(Species, Neilab, Shade) %>%
summarise(Ratio = mean(rootshoots), SE = (sd(rootshoots) / sqrt(length(rootshoots)))) %>% data.frame %>%
mutate(maxim = Ratio + 1.96*SE) %>% mutate(minim = Ratio - 1.96*SE) %>% group_by(Species) %>% summarise(MAX = max(maxim), MIN = min(minim))
gh[-c(224, 225), ] %>% group_by(Species, Neilab, Shade, Salt) %>%
summarise(Ratio = mean(rootshoots), SE = (sd(rootshoots) / sqrt(length(rootshoots)))) %>% data.frame %>%
mutate(maxim = Ratio + 1.96*SE) %>% mutate(minim = Ratio - 1.96*SE) %>% group_by(Species) %>% summarise(MAX = max(maxim), MIN = min(minim))

Dsb <- gh %>% subset(Species == "Ds") %>% Rnxsh + labs(titles = "Distichlis spicata") +
              scale_y_continuous(name = "Roots:shoots ratio", limits = c(0, 3.5), breaks = c(0, 1, 3.5)) 
Spub <- gh %>% subset(Species == "Spu") %>% Rnxsh + labs(titles = "Schoenoplectus pungens") +
              scale_y_continuous(name = "Roots:shoots ratio", limits = c(0, 4.8), breaks = c(0, 1, 4.8))
Sab <- gh %>% subset(Species == "Sa") %>% Rnxsh + labs(titles = "Spartina alterniflora") +
              scale_y_continuous(name = "Roots:shoots ratio", limits = c(0, 3.2), breaks = c(0, 1, 3.2)) 
Spb <- gh %>% subset(Species == "Sp") %>% Rnxsh + labs(titles = "Spartina patens") +
              scale_y_continuous(name = "Roots:shoots ratio", limits = c(0, 1.6), breaks = c(0, 1, 1.6))     
Pab <- gh[-c(224, 225), ] %>% subset(Species == "Pa") %>% Rnxshxsa + labs(titles = "Phragmites australis") +
              scale_y_continuous(name = "Roots:shoots ratio", limits = c(-0.6, 4), breaks = c(-0.6, 1, 4))  
Pvb <- gh %>% subset(Species == "Pv") %>% Rnxsh + labs(titles = "Panicum virgatum") +
              scale_y_continuous(name = "Roots:shoots ratio", limits = c(0, 2), breaks = c(0, 1, 1.9)) 

bioplot <- plot_grid(get_legend(Dsb + theme(legend.position = "top", legend.justification = "center")),
                     plot_grid(Dsb, Spub, Sab, Spb, ncol = 2), plot_grid(Pvb, Pab, ncol = 1),
                     ncol = 1, rel_heights = c(1, 12, 12))

ggsave(filename = "Ratio.tiff", plot = last_plot(),
       path = NULL, scale = 1, width = 167.64, height = 167.64, units = "mm", dpi = 600)

### RII

## Calculate RII

RII <- gh %>% 
        group_by(Species, Neighbor, Salt, Shade, Block) %>% 
        summarise(y = mean(total)) %>% 
        data.frame %>%
        spread(Neighbor, y) %>%
        mutate(Ds = (Ds - Alone)/(Ds + Alone)) %>%
        mutate(Spu = (Spu - Alone)/(Spu + Alone)) %>%
        mutate(Sa = (Sa - Alone)/(Sa + Alone)) %>%
        mutate(Sp = (Sp - Alone)/(Sp + Alone)) %>%
        mutate(Pa = (Pa - Alone)/(Pa + Alone)) %>%
        mutate(Pv = (Pv - Alone)/(Pv + Alone)) %>% 
        select(-Alone) %>%
        gather(Neighbor, RII, Ds:Spu) %>%
        na.omit %>%
        select(Neighbor, Species, everything())%>% 
        mutate(Combo = ifelse(Species == "Pa" | Species == "Pv", 
               paste(Species, Neighbor, sep = "_"), paste(Neighbor, Species, sep = "_")))%>% 
        data.frame %>% mutate(Habitat = ifelse(Species == "Pa" | Species == "Pv", "Upland", "Marsh")) %>%
        separate(col = Combo, into = c("USpp", "MSpp"), sep = "_", remove = F)

## Type III anova tables for RII per combo

options(contrasts = c("contr.helmert", "contr.poly"))

models <- RII %>% 
          group_by(Combo) %>%
          do(fits = drop1(aov(RII ~ Species * Shade * Salt, .), .~., test = "F")) 

anovas <- tidy(models, fits) %>%
            data.frame %>%
            mutate(Significance = ifelse(p.value > 0.1, "NS", 
                                  ifelse(p.value > 0.05, ".",
                                  ifelse(p.value > 0.01, "*",
                                  ifelse(p.value > 0.001, "**", "***")))))
write.csv(anovas, "RIIANOVAfull.csv") 

na.omit(anovas) %$% 
  matrix(Significance, ncol = length(unique(term)), byrow = T, 
         dimnames = list(unique(Combo), unique(term))) %>%
  write.csv("RIIANOVA.csv")

## Minimal adequate models diagnostic plots

par(mfrow = c(2, 2))
RII  %>% subset(Combo == "Pa_Ds") %>% aov(RII ~ Species + Shade, .) %>% plot
RII  %>% subset(Combo == "Pa_Sa") %>% aov(RII ~ Species, .) %>% plot
RII  %>% subset(Combo == "Pa_Sp") %>% aov(RII ~ Species, .) %>% plot
RII  %>% subset(Combo == "Pa_Spu") %>% aov(RII ~ Species * Salt, .) %>% plot
RII  %>% subset(Combo == "Pv_Ds") %>% aov(RII ~ Species * Shade, .) %>% plot
RII  %>% subset(Combo == "Pv_Sa") %>% aov(RII ~ Species * Shade, .) %>% plot
RII  %>% subset(Combo == "Pv_Spu") %>% aov(RII ~ Species, .) %>% plot

## RII plots

RII.means <- RII %>% group_by(Combo, Shade, Salt, Species) %>%
             summarise(Mean = mean(RII), SE = (sd(RII) / sqrt(length(RII)))) %>% 
             data.frame %>% 
             mutate(Habitat = ifelse(Species %in% c("Pa", "Pv"), "Upland", "Marsh"),
                    USpp = ifelse(Combo %in% c("Pa_Ds", "Pa_Sa", "Pa_Sp", "Pa_Spu"), "Pa", "Pv")) 

theme_set(theme_cowplot(font_size = 8.5))
fRIIfig <- function(df) {subset(RII.means, USpp == "Pv") %>% ggplot(., aes(y = Mean, x = Combo,
                           fill = Habitat, ymax = Mean + 1.96*SE, ymin = Mean - 1.96*SE)) + 
                    geom_col(position = "dodge") +
                    geom_errorbar(width = 0.3, lwd = 1, position = position_dodge(width = 0.9)) +
                    geom_hline(yintercept = 0, colour = "darkgrey") + 
                    geom_vline(xintercept = c(0.5, 1.5, 2.5, 3.5, 4.5), colour = "darkgrey") +  
                    facet_grid(Salt ~ Shade, labeller = as_labeller(c("0ppt" = "0 ppt salinity", "3ppt" = "3 ppt salinity", 
                                                          "6ppt" = "6 ppt salinity", "control" = "0% shade",
                                                          "shade1" = "75% shade", "shade2" = "90% shade")), switch = "x") +        
                    theme(axis.title.x = element_blank(), axis.text.x = element_text(face = "italic", 
                                                                                     angle = 45, hjust = 0),
                          legend.position = "none", legend.title = element_blank(),
                          plot.title = element_text(face = "italic")) +
                    scale_x_discrete(name = "Marsh species", position = "top",
                                     labels = c("Distichlis\n spicata", 
                                                "Spartina\n alterniflora", 
                                                "Spartina\n patens",
                                                "Schoenoplectus\n pungens")) +    
                    scale_y_continuous(name = "RII") +
                    scale_fill_manual(values = c("darkgreen", "gold"), name = "Habitat", 
                                      labels = c("Marsh neighbor", "Upland neighbor"))}

RIIplot <- plot_grid(get_legend(RII.means %>% subset(USpp == "Pa") %>% fRIIfig + labs(titles = "Phragmites australis") + 
                                theme(legend.position = "top", legend.justification = "center")),
                     RII.means %>% subset(USpp == "Pa") %>% fRIIfig + labs(titles = "Phragmites australis"),
                     RII.means %>% subset(USpp == "Pv") %>% fRIIfig + labs(titles = "Panicum virgatum"),
                     ncol = 1, rel_heights = c(1, 12, 12))

ggsave(filename = "RII.tiff", plot = last_plot(),
       path = NULL, scale = 1, width = 167.64, height = 200, units = "mm", dpi = 600)

## Network graph to see the winners

# Pairwise tests

options(contrasts = c("contr.helmert", "contr.poly"))

tests <- RII %>% 
         group_by(Combo, Shade, Salt) %>%
         do(fits = aov(RII ~ Species, .)) 

RII.balan <- RII %>% 
             merge(na.omit(data.frame(tidy(tests, fits)))) %>% 
             subset(p.value > 0.05) %>%
             group_by(Combo, Salt, Shade) %>% 
             summarise(RIImean = mean(RII), RIIse = sd(RII) / sqrt(length(RII)),
                       RIIlow = RIImean - 1.96 * RIIse, RIIupp = RIImean + 1.96 * RIIse) %>%
             separate(Combo, c("In", "Out"), "_") %>%
             mutate(Interaction = ifelse(RIIlow < 0 & RIIupp > 0, "Neutral", 
                                         ifelse(RIImean > 0, "Facilitation", "Competition")),
                    Balanced = "Balanced") %>% data.frame
RII.links <- RII %>% 
             merge(na.omit(data.frame(tidy(tests, fits)))) %>%
             subset(p.value <= 0.05) %>%
             group_by(Combo, Neighbor, Species, Salt, Shade) %>%
             summarise(RIImean = mean(RII), RIIse = sd(RII) / sqrt(length(RII)),
                       RIIlow = RIImean - 1.96 * RIIse, RIIupp = RIImean + 1.96 * RIIse) %>%
             mutate(Interaction = ifelse(RIIlow < 0 & RIIupp > 0, "Neutral", 
                                         ifelse(RIImean > 0, "Facilitation", "Competition")),
                    In = Neighbor, Out = Species, Balanced = "Unbalanced") %>% data.frame %>%
             select(-c(Combo, Species, Neighbor)) %>%
             rbind(RII.balan) %>%             
             select(In, Out, Salt, Shade, Interaction, Balanced, RIImean) %>%
             mutate(Habitat = ifelse(In == "Pa" | In == "Pv", "Upland", "Marsh"))

nodes <- unique(RII.links[, c("In", "Habitat")]) %>% arrange(-Habitat)  

# Control

links <- subset(RII.links, Shade == "control" & Salt == "0ppt")

net <- graph_from_data_frame(d = links, vertices = nodes, directed = T)

V(net)$color = V(net)$Habitat
V(net)$color = gsub("Upland", "gold4", V(net)$color)
V(net)$color = gsub("Marsh", "gold", V(net)$color)
V(net)$frame.color <- NA

V(net)$label.color = "black" 
V(net)$size = 50
V(net)$label.cex = 1
V(net)$type <- V(net)$Habitat %in% nodes[1,2]

E(net)$color = E(net)$Interaction
E(net)$color = gsub("Competition", "red", E(net)$color)
E(net)$color = gsub("Facilitation", "blue", E(net)$color)
E(net)$color = gsub("Neutral", "grey", E(net)$color)

net <- delete.edges(net, which(E(net)$Interaction == "Neutral"))
control.N <- delete.edges(net, which(E(net)$Balanced == "Unbalanced"))
control.W <- delete.edges(net, which(E(net)$Balanced == "Balanced"))

# Shade1

links <- subset(RII.links, Shade == "shade1" & Salt == "0ppt")

net <- graph_from_data_frame(d = links, vertices = nodes, directed = T)

V(net)$color = V(net)$Habitat
V(net)$color = gsub("Upland", "gold4", V(net)$color)
V(net)$color = gsub("Marsh", "gold", V(net)$color)
V(net)$frame.color <- NA

V(net)$label.color = "black" 
V(net)$size = 50
V(net)$label.cex = 1
V(net)$type <- V(net)$Habitat %in% nodes[1,2]

E(net)$color = E(net)$Interaction
E(net)$color = gsub("Competition", "red", E(net)$color)
E(net)$color = gsub("Facilitation", "blue", E(net)$color)
E(net)$color = gsub("Neutral", "grey", E(net)$color)

net <- delete.edges(net, which(E(net)$Interaction == "Neutral"))
shade1.N <- delete.edges(net, which(E(net)$Balanced == "Unbalanced"))
shade1.W <- delete.edges(net, which(E(net)$Balanced == "Balanced"))

# Shade2

links <- subset(RII.links, Shade == "shade2" & Salt == "0ppt")

net <- graph_from_data_frame(d = links, vertices = nodes, directed = T)

V(net)$color = V(net)$Habitat
V(net)$color = gsub("Upland", "gold4", V(net)$color)
V(net)$color = gsub("Marsh", "gold", V(net)$color)
V(net)$frame.color <- NA

V(net)$label.color = "black" 
V(net)$size = 50
V(net)$label.cex = 1
V(net)$type <- V(net)$Habitat %in% nodes[1,2]

E(net)$color = E(net)$Interaction
E(net)$color = gsub("Competition", "red", E(net)$color)
E(net)$color = gsub("Facilitation", "blue", E(net)$color)
E(net)$color = gsub("Neutral", "grey", E(net)$color)

net <- delete.edges(net, which(E(net)$Interaction == "Neutral"))
shade2.N <- delete.edges(net, which(E(net)$Balanced == "Unbalanced"))
shade2.W <- delete.edges(net, which(E(net)$Balanced == "Balanced"))

# Salt1

links <- subset(RII.links, Shade == "control" & Salt == "3ppt")

net <- graph_from_data_frame(d = links, vertices = nodes, directed = T)

V(net)$color = V(net)$Habitat
V(net)$color = gsub("Upland", "gold4", V(net)$color)
V(net)$color = gsub("Marsh", "gold", V(net)$color)
V(net)$frame.color <- NA

V(net)$label.color = "black" 
V(net)$size = 50
V(net)$label.cex = 1
V(net)$type <- V(net)$Habitat %in% nodes[1,2]

E(net)$color = E(net)$Interaction
E(net)$color = gsub("Competition", "red", E(net)$color)
E(net)$color = gsub("Facilitation", "blue", E(net)$color)
E(net)$color = gsub("Neutral", "grey", E(net)$color)

net <- delete.edges(net, which(E(net)$Interaction == "Neutral"))
salt1.N <- delete.edges(net, which(E(net)$Balanced == "Unbalanced"))
salt1.W <- delete.edges(net, which(E(net)$Balanced == "Balanced"))

# Salt2

links <- subset(RII.links, Shade == "control" & Salt == "6ppt")

net <- graph_from_data_frame(d = links, vertices = nodes, directed = T)

V(net)$color = V(net)$Habitat
V(net)$color = gsub("Upland", "gold4", V(net)$color)
V(net)$color = gsub("Marsh", "gold", V(net)$color)
V(net)$frame.color <- NA

V(net)$label.color = "black" 
V(net)$size = 50
V(net)$label.cex = 1
V(net)$type <- V(net)$Habitat %in% nodes[1,2]

E(net)$color = E(net)$Interaction
E(net)$color = gsub("Competition", "red", E(net)$color)
E(net)$color = gsub("Facilitation", "blue", E(net)$color)
E(net)$color = gsub("Neutral", "grey", E(net)$color)

net <- delete.edges(net, which(E(net)$Interaction == "Neutral"))
salt2.N <- delete.edges(net, which(E(net)$Balanced == "Unbalanced"))
salt2.W <- delete.edges(net, which(E(net)$Balanced == "Balanced"))

# Shade1 and salt1

links <- subset(RII.links, Shade == "shade1" & Salt == "3ppt")

net <- graph_from_data_frame(d = links, vertices = nodes, directed = T)

V(net)$color = V(net)$Habitat
V(net)$color = gsub("Upland", "gold4", V(net)$color)
V(net)$color = gsub("Marsh", "gold", V(net)$color)
V(net)$frame.color <- NA

V(net)$label.color = "black" 
V(net)$size = 50
V(net)$label.cex = 1
V(net)$type <- V(net)$Habitat %in% nodes[1,2]

E(net)$color = E(net)$Interaction
E(net)$color = gsub("Competition", "red", E(net)$color)
E(net)$color = gsub("Facilitation", "blue", E(net)$color)
E(net)$color = gsub("Neutral", "grey", E(net)$color)

net <- delete.edges(net, which(E(net)$Interaction == "Neutral"))
all11.N <- delete.edges(net, which(E(net)$Balanced == "Unbalanced"))
all11.W <- delete.edges(net, which(E(net)$Balanced == "Balanced"))

# Shade1 and salt2

links <- subset(RII.links, Shade == "shade1" & Salt == "6ppt")

net <- graph_from_data_frame(d = links, vertices = nodes, directed = T)

V(net)$color = V(net)$Habitat
V(net)$color = gsub("Upland", "gold4", V(net)$color)
V(net)$color = gsub("Marsh", "gold", V(net)$color)
V(net)$frame.color <- NA

V(net)$label.color = "black" 
V(net)$size = 50
V(net)$label.cex = 1
V(net)$type <- V(net)$Habitat %in% nodes[1,2]

E(net)$color = E(net)$Interaction
E(net)$color = gsub("Competition", "red", E(net)$color)
E(net)$color = gsub("Facilitation", "blue", E(net)$color)
E(net)$color = gsub("Neutral", "grey", E(net)$color)

net <- delete.edges(net, which(E(net)$Interaction == "Neutral"))
all12.N <- delete.edges(net, which(E(net)$Balanced == "Unbalanced"))
all12.W <- delete.edges(net, which(E(net)$Balanced == "Balanced"))

# Shade2 and salt1

links <- subset(RII.links, Shade == "shade2" & Salt == "3ppt")

net <- graph_from_data_frame(d = links, vertices = nodes, directed = T)

V(net)$color = V(net)$Habitat
V(net)$color = gsub("Upland", "gold4", V(net)$color)
V(net)$color = gsub("Marsh", "gold", V(net)$color)
V(net)$frame.color <- NA

V(net)$label.color = "black" 
V(net)$size = 50
V(net)$label.cex = 1
V(net)$type <- V(net)$Habitat %in% nodes[1,2]

E(net)$color = E(net)$Interaction
E(net)$color = gsub("Competition", "red", E(net)$color)
E(net)$color = gsub("Facilitation", "blue", E(net)$color)
E(net)$color = gsub("Neutral", "grey", E(net)$color)

net <- delete.edges(net, which(E(net)$Interaction == "Neutral"))
all21.N <- delete.edges(net, which(E(net)$Balanced == "Unbalanced"))
all21.W <- delete.edges(net, which(E(net)$Balanced == "Balanced"))

# Shade2 and salt2

links <- subset(RII.links, Shade == "shade2" & Salt == "6ppt")

net <- graph_from_data_frame(d = links, vertices = nodes, directed = T)

V(net)$color = V(net)$Habitat
V(net)$color = gsub("Upland", "gold4", V(net)$color)
V(net)$color = gsub("Marsh", "gold", V(net)$color)
V(net)$frame.color <- NA

V(net)$label.color = "black" 
V(net)$size = 50
V(net)$label.cex = 1
V(net)$type <- V(net)$Habitat %in% nodes[1,2]

E(net)$color = E(net)$Interaction
E(net)$color = gsub("Competition", "red", E(net)$color)
E(net)$color = gsub("Facilitation", "blue", E(net)$color)
E(net)$color = gsub("Neutral", "grey", E(net)$color)

net <- delete.edges(net, which(E(net)$Interaction == "Neutral"))
all22.N <- delete.edges(net, which(E(net)$Balanced == "Unbalanced"))
all22.W <- delete.edges(net, which(E(net)$Balanced == "Balanced"))

# Build plot

png(filename = "RII.net.tiff",
    width = 167.64, height = 167.64, units = "mm", pointsize = 16,
    bg = "transparent", res = 600, family = "", restoreConsole = TRUE,
    type = "windows")
par(mar = c(0, 0, 1, 0))
par(mfrow = c(3, 3))

plot(control.N, layout = layout_in_circle, edge.curved = 0, edge.arrow.size = 0, edge.width = abs(E(control.N)$RIImean) * 2)
title("0% shade - 0 ppt salinity", cex.main = 0.74, col.main = "grey25")
plot(control.W, layout = layout_in_circle, edge.curved = 0.35, edge.arrow.size = 0.3, edge.width = abs(E(control.W)$RIImean) * 2, add = T)
plot(shade1.N, layout = layout_in_circle, edge.curved = 0, edge.arrow.size = 0, edge.width = abs(E(shade1.N)$RIImean) * 2)
title("75% shade - 0 ppt salinity", cex.main = 0.74, col.main = "grey25")
plot(shade1.W, layout = layout_in_circle, edge.curved = 0.35, edge.arrow.size = 0.3, edge.width = abs(E(shade1.W)$RIImean) * 2, add = T)
plot(shade2.N, layout = layout_in_circle, edge.curved = 0, edge.arrow.size = 0, edge.width = abs(E(shade2.N)$RIImean) * 2)
title("90% shade - 0 ppt salinity", cex.main = 0.74, col.main = "grey25")
plot(shade2.W, layout = layout_in_circle, edge.curved = 0.35, edge.arrow.size = 0.3, edge.width = abs(E(shade2.W)$RIImean) * 2, add = T)
plot(salt1.N, layout = layout_in_circle, edge.curved = 0, edge.arrow.size = 0, edge.width = abs(E(salt1.N)$RIImean) * 2)
title("0% shade - 3 ppt salinity", cex.main = 0.74, col.main = "grey25")
plot(salt1.W, layout = layout_in_circle, edge.curved = 0.35, edge.arrow.size = 0.3, edge.width = abs(E(salt1.W)$RIImean) * 3, add = T)
plot(all11.N, layout = layout_in_circle, edge.curved = 0, edge.arrow.size = 0, edge.width = abs(E(all11.N)$RIImean) * 3)
title("75% shade - 3 ppt salinity", cex.main = 0.74, col.main = "grey25")
plot(all11.W, layout = layout_in_circle, edge.curved = 0.35, edge.arrow.size = 0.3, edge.width = abs(E(all11.W)$RIImean) * 3, add = T)
plot(all12.N, layout = layout_in_circle, edge.curved = 0, edge.arrow.size = 0, edge.width = abs(E(all12.N)$RIImean) * 3)
title("90% shade - 3 ppt salinity", cex.main = 0.74, col.main = "grey25")
plot(all12.W, layout = layout_in_circle, edge.curved = 0.35, edge.arrow.size = 0.3, edge.width = abs(E(all12.W)$RIImean) * 3, add = T)
plot(salt2.N, layout = layout_in_circle, edge.curved = 0, edge.arrow.size = 0, edge.width = abs(E(salt2.N)$RIImean) * 3)
title("0% shade - 6 ppt salinity", cex.main = 0.74, col.main = "grey25")
plot(salt2.W, layout = layout_in_circle, edge.curved = 0.35, edge.arrow.size = 0.3, edge.width = abs(E(salt2.W)$RIImean) * 3, add = T)
plot(all21.N, layout = layout_in_circle, edge.curved = 0, edge.arrow.size = 0, edge.width = abs(E(all21.N)$RIImean) * 3)
title("75% shade - 6 ppt salinity", cex.main = 0.74, col.main = "grey25")
plot(all21.W, layout = layout_in_circle, edge.curved = 0.35, edge.arrow.size = 0.3, edge.width = abs(E(all21.W)$RIImean) * 3, add = T)
plot(all22.N, layout = layout_in_circle, edge.curved = 0, edge.arrow.size = 0, edge.width = abs(E(all22.N)$RIImean) * 3)
title("90% shade - 6 ppt salinity", cex.main = 0.74, col.main = "grey25")
plot(all22.W, layout = layout_in_circle, edge.curved = 0.2, edge.arrow.size = 0.3, edge.width = abs(E(all22.W)$RIImean) * 3, add = T)

dev.off()

## Network graph to see the winners Pa ONLY

# Pairwise tests

options(contrasts = c("contr.helmert", "contr.poly"))

RIIpa <- subset(RII, Combo == "Pa_Ds" | Combo == "Pa_Sa" | Combo == "Pa_Sp" | Combo == "Pa_Spu")

tests <- RIIpa %>% 
         group_by(Combo, Shade, Salt) %>%
         do(fits = aov(RII ~ Species, .)) 

RII.balan <- RIIpa %>% 
             merge(na.omit(data.frame(tidy(tests, fits)))) %>% 
             subset(p.value > 0.05) %>%
             group_by(Combo, Salt, Shade) %>% 
             summarise(RIImean = mean(RII), RIIse = sd(RII) / sqrt(length(RII)),
                       RIIlow = RIImean - 1.96 * RIIse, RIIupp = RIImean + 1.96 * RIIse) %>%
             separate(Combo, c("In", "Out"), "_") %>%
             mutate(Interaction = ifelse(RIIlow < 0 & RIIupp > 0, "Neutral", 
                                         ifelse(RIImean > 0, "Facilitation", "Competition")),
                    Balanced = "Balanced") %>% data.frame
RII.links <- RIIpa %>% 
             merge(na.omit(data.frame(tidy(tests, fits)))) %>%
             subset(p.value <= 0.05) %>%
             group_by(Combo, Neighbor, Species, Salt, Shade) %>%
             summarise(RIImean = mean(RII), RIIse = sd(RII) / sqrt(length(RII)),
                       RIIlow = RIImean - 1.96 * RIIse, RIIupp = RIImean + 1.96 * RIIse) %>%
             mutate(Interaction = ifelse(RIIlow < 0 & RIIupp > 0, "Neutral", 
                                         ifelse(RIImean > 0, "Facilitation", "Competition")),
                    In = Neighbor, Out = Species, Balanced = "Unbalanced") %>% data.frame %>%
             select(-c(Combo, Species, Neighbor)) %>%
             rbind(RII.balan) %>%             
             select(In, Out, Salt, Shade, Interaction, Balanced, RIImean) %>%
             mutate(Habitat = ifelse(In == "Pa" | In == "Pv", "Upland", "Marsh"))

nodespa <- nodes %>% subset(In != "Pv") %>% arrange(desc(Habitat))  
 
# Control

links <- subset(RII.links, Shade == "control" & Salt == "0ppt")

net <- graph_from_data_frame(d = links, vertices = nodespa, directed = T)

V(net)$color = V(net)$Habitat
V(net)$color = gsub("Upland", "gold4", V(net)$color)
V(net)$color = gsub("Marsh", "gold", V(net)$color)
V(net)$frame.color <- NA

V(net)$label.color = "black" 
V(net)$size = 50
V(net)$label.cex = 1
V(net)$type <- V(net)$Habitat %in% nodespa[1,2]

E(net)$color = E(net)$Interaction
E(net)$color = gsub("Competition", "red", E(net)$color)
E(net)$color = gsub("Facilitation", "blue", E(net)$color)
E(net)$color = gsub("Neutral", "grey", E(net)$color)

net <- delete.edges(net, which(E(net)$Interaction == "Neutral"))
control.N <- delete.edges(net, which(E(net)$Balanced == "Unbalanced"))
control.W <- delete.edges(net, which(E(net)$Balanced == "Balanced"))

# Shade1

links <- subset(RII.links, Shade == "shade1" & Salt == "0ppt")

net <- graph_from_data_frame(d = links, vertices = nodespa, directed = T)

V(net)$color = V(net)$Habitat
V(net)$color = gsub("Upland", "gold4", V(net)$color)
V(net)$color = gsub("Marsh", "gold", V(net)$color)
V(net)$frame.color <- NA

V(net)$label.color = "black" 
V(net)$size = 50
V(net)$label.cex = 1
V(net)$type <- V(net)$Habitat %in% nodespa[1,2]

E(net)$color = E(net)$Interaction
E(net)$color = gsub("Competition", "red", E(net)$color)
E(net)$color = gsub("Facilitation", "blue", E(net)$color)
E(net)$color = gsub("Neutral", "grey", E(net)$color)

net <- delete.edges(net, which(E(net)$Interaction == "Neutral"))
shade1.N <- delete.edges(net, which(E(net)$Balanced == "Unbalanced"))
shade1.W <- delete.edges(net, which(E(net)$Balanced == "Balanced"))

# Shade2

links <- subset(RII.links, Shade == "shade2" & Salt == "0ppt")

net <- graph_from_data_frame(d = links, vertices = nodespa, directed = T)

V(net)$color = V(net)$Habitat
V(net)$color = gsub("Upland", "gold4", V(net)$color)
V(net)$color = gsub("Marsh", "gold", V(net)$color)
V(net)$frame.color <- NA

V(net)$label.color = "black" 
V(net)$size = 50
V(net)$label.cex = 1
V(net)$type <- V(net)$Habitat %in% nodespa[1,2]

E(net)$color = E(net)$Interaction
E(net)$color = gsub("Competition", "red", E(net)$color)
E(net)$color = gsub("Facilitation", "blue", E(net)$color)
E(net)$color = gsub("Neutral", "grey", E(net)$color)

net <- delete.edges(net, which(E(net)$Interaction == "Neutral"))
shade2.N <- delete.edges(net, which(E(net)$Balanced == "Unbalanced"))
shade2.W <- delete.edges(net, which(E(net)$Balanced == "Balanced"))

# Salt1

links <- subset(RII.links, Shade == "control" & Salt == "3ppt")

net <- graph_from_data_frame(d = links, vertices = nodespa, directed = T)

V(net)$color = V(net)$Habitat
V(net)$color = gsub("Upland", "gold4", V(net)$color)
V(net)$color = gsub("Marsh", "gold", V(net)$color)
V(net)$frame.color <- NA

V(net)$label.color = "black" 
V(net)$size = 50
V(net)$label.cex = 1
V(net)$type <- V(net)$Habitat %in% nodespa[1,2]

E(net)$color = E(net)$Interaction
E(net)$color = gsub("Competition", "red", E(net)$color)
E(net)$color = gsub("Facilitation", "blue", E(net)$color)
E(net)$color = gsub("Neutral", "grey", E(net)$color)

net <- delete.edges(net, which(E(net)$Interaction == "Neutral"))
salt1.N <- delete.edges(net, which(E(net)$Balanced == "Unbalanced"))
salt1.W <- delete.edges(net, which(E(net)$Balanced == "Balanced"))

# Salt2

links <- subset(RII.links, Shade == "control" & Salt == "6ppt")

net <- graph_from_data_frame(d = links, vertices = nodespa, directed = T)

V(net)$color = V(net)$Habitat
V(net)$color = gsub("Upland", "gold4", V(net)$color)
V(net)$color = gsub("Marsh", "gold", V(net)$color)
V(net)$frame.color <- NA

V(net)$label.color = "black" 
V(net)$size = 50
V(net)$label.cex = 1
V(net)$type <- V(net)$Habitat %in% nodespa[1,2]

E(net)$color = E(net)$Interaction
E(net)$color = gsub("Competition", "red", E(net)$color)
E(net)$color = gsub("Facilitation", "blue", E(net)$color)
E(net)$color = gsub("Neutral", "grey", E(net)$color)

net <- delete.edges(net, which(E(net)$Interaction == "Neutral"))
salt2.N <- delete.edges(net, which(E(net)$Balanced == "Unbalanced"))
salt2.W <- delete.edges(net, which(E(net)$Balanced == "Balanced"))

# Shade1 and salt1

links <- subset(RII.links, Shade == "shade1" & Salt == "3ppt")

net <- graph_from_data_frame(d = links, vertices = nodespa, directed = T)

V(net)$color = V(net)$Habitat
V(net)$color = gsub("Upland", "gold4", V(net)$color)
V(net)$color = gsub("Marsh", "gold", V(net)$color)
V(net)$frame.color <- NA

V(net)$label.color = "black" 
V(net)$size = 50
V(net)$label.cex = 1
V(net)$type <- V(net)$Habitat %in% nodespa[1,2]

E(net)$color = E(net)$Interaction
E(net)$color = gsub("Competition", "red", E(net)$color)
E(net)$color = gsub("Facilitation", "blue", E(net)$color)
E(net)$color = gsub("Neutral", "grey", E(net)$color)

net <- delete.edges(net, which(E(net)$Interaction == "Neutral"))
all11.N <- delete.edges(net, which(E(net)$Balanced == "Unbalanced"))
all11.W <- delete.edges(net, which(E(net)$Balanced == "Balanced"))

# Shade1 and salt2

links <- subset(RII.links, Shade == "shade1" & Salt == "6ppt")

net <- graph_from_data_frame(d = links, vertices = nodespa, directed = T)

V(net)$color = V(net)$Habitat
V(net)$color = gsub("Upland", "gold4", V(net)$color)
V(net)$color = gsub("Marsh", "gold", V(net)$color)
V(net)$frame.color <- NA

V(net)$label.color = "black" 
V(net)$size = 50
V(net)$label.cex = 1
V(net)$type <- V(net)$Habitat %in% nodespa[1,2]

E(net)$color = E(net)$Interaction
E(net)$color = gsub("Competition", "red", E(net)$color)
E(net)$color = gsub("Facilitation", "blue", E(net)$color)
E(net)$color = gsub("Neutral", "grey", E(net)$color)

net <- delete.edges(net, which(E(net)$Interaction == "Neutral"))
all12.N <- delete.edges(net, which(E(net)$Balanced == "Unbalanced"))
all12.W <- delete.edges(net, which(E(net)$Balanced == "Balanced"))

# Shade2 and salt1

links <- subset(RII.links, Shade == "shade2" & Salt == "3ppt")

net <- graph_from_data_frame(d = links, vertices = nodespa, directed = T)

V(net)$color = V(net)$Habitat
V(net)$color = gsub("Upland", "gold4", V(net)$color)
V(net)$color = gsub("Marsh", "gold", V(net)$color)
V(net)$frame.color <- NA

V(net)$label.color = "black" 
V(net)$size = 50
V(net)$label.cex = 1
V(net)$type <- V(net)$Habitat %in% nodespa[1,2]

E(net)$color = E(net)$Interaction
E(net)$color = gsub("Competition", "red", E(net)$color)
E(net)$color = gsub("Facilitation", "blue", E(net)$color)
E(net)$color = gsub("Neutral", "grey", E(net)$color)

net <- delete.edges(net, which(E(net)$Interaction == "Neutral"))
all21.N <- delete.edges(net, which(E(net)$Balanced == "Unbalanced"))
all21.W <- delete.edges(net, which(E(net)$Balanced == "Balanced"))

# Shade2 and salt2

links <- subset(RII.links, Shade == "shade2" & Salt == "6ppt")

net <- graph_from_data_frame(d = links, vertices = nodespa, directed = T)

V(net)$color = V(net)$Habitat
V(net)$color = gsub("Upland", "gold4", V(net)$color)
V(net)$color = gsub("Marsh", "gold", V(net)$color)
V(net)$frame.color <- NA

V(net)$label.color = "black" 
V(net)$size = 50
V(net)$label.cex = 1
V(net)$type <- V(net)$Habitat %in% nodespa[1,2]

E(net)$color = E(net)$Interaction
E(net)$color = gsub("Competition", "red", E(net)$color)
E(net)$color = gsub("Facilitation", "blue", E(net)$color)
E(net)$color = gsub("Neutral", "grey", E(net)$color)

net <- delete.edges(net, which(E(net)$Interaction == "Neutral"))
all22.N <- delete.edges(net, which(E(net)$Balanced == "Unbalanced"))
all22.W <- delete.edges(net, which(E(net)$Balanced == "Balanced"))

# Build plot

png(filename = "RII.netPa.tiff",
    width = 167.64, height = 167.64, units = "mm", pointsize = 16,
    bg = "transparent", res = 600, family = "", restoreConsole = TRUE,
    type = "windows")
par(mar = c(0, 0, 1, 0))
par(mfrow = c(3, 3))

plot(control.N, layout = layout_in_circle, edge.curved = 0, edge.arrow.size = 0, edge.width = abs(E(control.N)$RIImean) * 2)
title("0% shade - 0 ppt salinity", cex.main = 0.74, col.main = "grey25")
plot(control.W, layout = layout_in_circle, edge.curved = 0.35, edge.arrow.size = 0.3, edge.width = abs(E(control.W)$RIImean) * 2, add = T)
plot(shade1.N, layout = layout_in_circle, edge.curved = 0, edge.arrow.size = 0, edge.width = abs(E(shade1.N)$RIImean) * 2)
title("75% shade - 0 ppt salinity", cex.main = 0.74, col.main = "grey25")
plot(shade1.W, layout = layout_in_circle, edge.curved = 0.35, edge.arrow.size = 0.3, edge.width = abs(E(shade1.W)$RIImean) * 2, add = T)
plot(shade2.N, layout = layout_in_circle, edge.curved = 0, edge.arrow.size = 0, edge.width = abs(E(shade2.N)$RIImean) * 2)
title("90% shade - 0 ppt salinity", cex.main = 0.74, col.main = "grey25")
plot(shade2.W, layout = layout_in_circle, edge.curved = 0.35, edge.arrow.size = 0.3, edge.width = abs(E(shade2.W)$RIImean) * 2, add = T)
plot(salt1.N, layout = layout_in_circle, edge.curved = 0, edge.arrow.size = 0, edge.width = abs(E(salt1.N)$RIImean) * 2)
title("0% shade - 3 ppt salinity", cex.main = 0.74, col.main = "grey25")
plot(salt1.W, layout = layout_in_circle, edge.curved = 0.35, edge.arrow.size = 0.3, edge.width = abs(E(salt1.W)$RIImean) * 3, add = T)
plot(all11.N, layout = layout_in_circle, edge.curved = 0, edge.arrow.size = 0, edge.width = abs(E(all11.N)$RIImean) * 3)
title("75% shade - 3 ppt salinity", cex.main = 0.74, col.main = "grey25")
plot(all11.W, layout = layout_in_circle, edge.curved = 0.35, edge.arrow.size = 0.3, edge.width = abs(E(all11.W)$RIImean) * 3, add = T)
plot(all12.N, layout = layout_in_circle, edge.curved = 0, edge.arrow.size = 0, edge.width = abs(E(all12.N)$RIImean) * 3)
title("90% shade - 3 ppt salinity", cex.main = 0.74, col.main = "grey25")
plot(all12.W, layout = layout_in_circle, edge.curved = 0.35, edge.arrow.size = 0.3, edge.width = abs(E(all12.W)$RIImean) * 3, add = T)
plot(salt2.N, layout = layout_in_circle, edge.curved = 0, edge.arrow.size = 0, edge.width = abs(E(salt2.N)$RIImean) * 3)
title("0% shade - 6 ppt salinity", cex.main = 0.74, col.main = "grey25")
plot(salt2.W, layout = layout_in_circle, edge.curved = 0.35, edge.arrow.size = 0.3, edge.width = abs(E(salt2.W)$RIImean) * 3, add = T)
plot(all21.N, layout = layout_in_circle, edge.curved = 0, edge.arrow.size = 0, edge.width = abs(E(all21.N)$RIImean) * 3)
title("75% shade - 6 ppt salinity", cex.main = 0.74, col.main = "grey25")
plot(all21.W, layout = layout_in_circle, edge.curved = 0.35, edge.arrow.size = 0.3, edge.width = abs(E(all21.W)$RIImean) * 3, add = T)
plot(all22.N, layout = layout_in_circle, edge.curved = 0, edge.arrow.size = 0, edge.width = abs(E(all22.N)$RIImean) * 3)
title("90% shade - 6 ppt salinity", cex.main = 0.74, col.main = "grey25")
plot(all22.W, layout = layout_in_circle, edge.curved = 0.2, edge.arrow.size = 0.3, edge.width = abs(E(all22.W)$RIImean) * 3, add = T)

dev.off()

## Network graph to see the winners Pv ONLY

# Pairwise tests

options(contrasts = c("contr.helmert", "contr.poly"))

RIIpv <- subset(RII, Combo == "Pv_Ds" | Combo == "Pv_Sa" | Combo == "Pv_Sp" | Combo == "Pv_Spu")

tests <- RIIpv %>% 
         group_by(Combo, Shade, Salt) %>%
         do(fits = aov(RII ~ Species, .)) 

RII.balan <- RIIpv %>% 
             merge(na.omit(data.frame(tidy(tests, fits)))) %>% 
             subset(p.value > 0.05) %>%
             group_by(Combo, Salt, Shade) %>% 
             summarise(RIImean = mean(RII), RIIse = sd(RII) / sqrt(length(RII)),
                       RIIlow = RIImean - 1.96 * RIIse, RIIupp = RIImean + 1.96 * RIIse) %>%
             separate(Combo, c("In", "Out"), "_") %>%
             mutate(Interaction = ifelse(RIIlow < 0 & RIIupp > 0, "Neutral", 
                                         ifelse(RIImean > 0, "Facilitation", "Competition")),
                    Balanced = "Balanced") %>% data.frame

RII.links <- RIIpv %>% 
             merge(na.omit(data.frame(tidy(tests, fits)))) %>%
             subset(p.value <= 0.05) %>%
             group_by(Combo, Neighbor, Species, Salt, Shade) %>%
             summarise(RIImean = mean(RII), RIIse = sd(RII) / sqrt(length(RII)),
                       RIIlow = RIImean - 1.96 * RIIse, RIIupp = RIImean + 1.96 * RIIse) %>%
             mutate(Interaction = ifelse(RIIlow < 0 & RIIupp > 0, "Neutral", 
                                         ifelse(RIImean > 0, "Facilitation", "Competition")),
                    In = Neighbor, Out = Species, Balanced = "Unbalanced") %>% data.frame %>%
             select(-c(Combo, Species, Neighbor)) %>%
             rbind(RII.balan) %>%             
             select(In, Out, Salt, Shade, Interaction, Balanced, RIImean) %>%
             mutate(Habitat = ifelse(In == "Pa" | In == "Pv", "Upland", "Marsh"))

nodespv <- nodes %>% subset(In != "Pa") %>% arrange(desc(Habitat))  
 
# Control

links <- subset(RII.links, Shade == "control" & Salt == "0ppt")

net <- graph_from_data_frame(d = links, vertices = nodespv, directed = T)

V(net)$color = V(net)$Habitat
V(net)$color = gsub("Upland", "gold4", V(net)$color)
V(net)$color = gsub("Marsh", "gold", V(net)$color)
V(net)$frame.color <- NA

V(net)$label.color = "black" 
V(net)$size = 50
V(net)$label.cex = 1
V(net)$type <- V(net)$Habitat %in% nodespv[1,2]

E(net)$color = E(net)$Interaction
E(net)$color = gsub("Competition", "red", E(net)$color)
E(net)$color = gsub("Facilitation", "blue", E(net)$color)
E(net)$color = gsub("Neutral", "grey", E(net)$color)

net <- delete.edges(net, which(E(net)$Interaction == "Neutral"))
control.N <- delete.edges(net, which(E(net)$Balanced == "Unbalanced"))
control.W <- delete.edges(net, which(E(net)$Balanced == "Balanced"))

# Shade1

links <- subset(RII.links, Shade == "shade1" & Salt == "0ppt")

net <- graph_from_data_frame(d = links, vertices = nodespv, directed = T)

V(net)$color = V(net)$Habitat
V(net)$color = gsub("Upland", "gold4", V(net)$color)
V(net)$color = gsub("Marsh", "gold", V(net)$color)
V(net)$frame.color <- NA

V(net)$label.color = "black" 
V(net)$size = 50
V(net)$label.cex = 1
V(net)$type <- V(net)$Habitat %in% nodespv[1,2]

E(net)$color = E(net)$Interaction
E(net)$color = gsub("Competition", "red", E(net)$color)
E(net)$color = gsub("Facilitation", "blue", E(net)$color)
E(net)$color = gsub("Neutral", "grey", E(net)$color)

net <- delete.edges(net, which(E(net)$Interaction == "Neutral"))
shade1.N <- delete.edges(net, which(E(net)$Balanced == "Unbalanced"))
shade1.W <- delete.edges(net, which(E(net)$Balanced == "Balanced"))

# Shade2

links <- subset(RII.links, Shade == "shade2" & Salt == "0ppt")

net <- graph_from_data_frame(d = links, vertices = nodespv, directed = T)

V(net)$color = V(net)$Habitat
V(net)$color = gsub("Upland", "gold4", V(net)$color)
V(net)$color = gsub("Marsh", "gold", V(net)$color)
V(net)$frame.color <- NA

V(net)$label.color = "black" 
V(net)$size = 50
V(net)$label.cex = 1
V(net)$type <- V(net)$Habitat %in% nodespv[1,2]

E(net)$color = E(net)$Interaction
E(net)$color = gsub("Competition", "red", E(net)$color)
E(net)$color = gsub("Facilitation", "blue", E(net)$color)
E(net)$color = gsub("Neutral", "grey", E(net)$color)

net <- delete.edges(net, which(E(net)$Interaction == "Neutral"))
shade2.N <- delete.edges(net, which(E(net)$Balanced == "Unbalanced"))
shade2.W <- delete.edges(net, which(E(net)$Balanced == "Balanced"))

# Salt1

links <- subset(RII.links, Shade == "control" & Salt == "3ppt")

net <- graph_from_data_frame(d = links, vertices = nodespv, directed = T)

V(net)$color = V(net)$Habitat
V(net)$color = gsub("Upland", "gold4", V(net)$color)
V(net)$color = gsub("Marsh", "gold", V(net)$color)
V(net)$frame.color <- NA

V(net)$label.color = "black" 
V(net)$size = 50
V(net)$label.cex = 1
V(net)$type <- V(net)$Habitat %in% nodespv[1,2]

E(net)$color = E(net)$Interaction
E(net)$color = gsub("Competition", "red", E(net)$color)
E(net)$color = gsub("Facilitation", "blue", E(net)$color)
E(net)$color = gsub("Neutral", "grey", E(net)$color)

net <- delete.edges(net, which(E(net)$Interaction == "Neutral"))
salt1.N <- delete.edges(net, which(E(net)$Balanced == "Unbalanced"))
salt1.W <- delete.edges(net, which(E(net)$Balanced == "Balanced"))

# Salt2

links <- subset(RII.links, Shade == "control" & Salt == "6ppt")

net <- graph_from_data_frame(d = links, vertices = nodespv, directed = T)

V(net)$color = V(net)$Habitat
V(net)$color = gsub("Upland", "gold4", V(net)$color)
V(net)$color = gsub("Marsh", "gold", V(net)$color)
V(net)$frame.color <- NA

V(net)$label.color = "black" 
V(net)$size = 50
V(net)$label.cex = 1
V(net)$type <- V(net)$Habitat %in% nodespv[1,2]

E(net)$color = E(net)$Interaction
E(net)$color = gsub("Competition", "red", E(net)$color)
E(net)$color = gsub("Facilitation", "blue", E(net)$color)
E(net)$color = gsub("Neutral", "grey", E(net)$color)

net <- delete.edges(net, which(E(net)$Interaction == "Neutral"))
salt2.N <- delete.edges(net, which(E(net)$Balanced == "Unbalanced"))
salt2.W <- delete.edges(net, which(E(net)$Balanced == "Balanced"))

# Shade1 and salt1

links <- subset(RII.links, Shade == "shade1" & Salt == "3ppt")

net <- graph_from_data_frame(d = links, vertices = nodespv, directed = T)

V(net)$color = V(net)$Habitat
V(net)$color = gsub("Upland", "gold4", V(net)$color)
V(net)$color = gsub("Marsh", "gold", V(net)$color)
V(net)$frame.color <- NA

V(net)$label.color = "black" 
V(net)$size = 50
V(net)$label.cex = 1
V(net)$type <- V(net)$Habitat %in% nodespv[1,2]

E(net)$color = E(net)$Interaction
E(net)$color = gsub("Competition", "red", E(net)$color)
E(net)$color = gsub("Facilitation", "blue", E(net)$color)
E(net)$color = gsub("Neutral", "grey", E(net)$color)

net <- delete.edges(net, which(E(net)$Interaction == "Neutral"))
all11.N <- delete.edges(net, which(E(net)$Balanced == "Unbalanced"))
all11.W <- delete.edges(net, which(E(net)$Balanced == "Balanced"))

# Shade1 and salt2

links <- subset(RII.links, Shade == "shade1" & Salt == "6ppt")

net <- graph_from_data_frame(d = links, vertices = nodespv, directed = T)

V(net)$color = V(net)$Habitat
V(net)$color = gsub("Upland", "gold4", V(net)$color)
V(net)$color = gsub("Marsh", "gold", V(net)$color)
V(net)$frame.color <- NA

V(net)$label.color = "black" 
V(net)$size = 50
V(net)$label.cex = 1
V(net)$type <- V(net)$Habitat %in% nodespv[1,2]

E(net)$color = E(net)$Interaction
E(net)$color = gsub("Competition", "red", E(net)$color)
E(net)$color = gsub("Facilitation", "blue", E(net)$color)
E(net)$color = gsub("Neutral", "grey", E(net)$color)

net <- delete.edges(net, which(E(net)$Interaction == "Neutral"))
all12.N <- delete.edges(net, which(E(net)$Balanced == "Unbalanced"))
all12.W <- delete.edges(net, which(E(net)$Balanced == "Balanced"))

# Shade2 and salt1

links <- subset(RII.links, Shade == "shade2" & Salt == "3ppt")

net <- graph_from_data_frame(d = links, vertices = nodespv, directed = T)

V(net)$color = V(net)$Habitat
V(net)$color = gsub("Upland", "gold4", V(net)$color)
V(net)$color = gsub("Marsh", "gold", V(net)$color)
V(net)$frame.color <- NA

V(net)$label.color = "black" 
V(net)$size = 50
V(net)$label.cex = 1
V(net)$type <- V(net)$Habitat %in% nodespv[1,2]

E(net)$color = E(net)$Interaction
E(net)$color = gsub("Competition", "red", E(net)$color)
E(net)$color = gsub("Facilitation", "blue", E(net)$color)
E(net)$color = gsub("Neutral", "grey", E(net)$color)

net <- delete.edges(net, which(E(net)$Interaction == "Neutral"))
all21.N <- delete.edges(net, which(E(net)$Balanced == "Unbalanced"))
all21.W <- delete.edges(net, which(E(net)$Balanced == "Balanced"))

# Shade2 and salt2

links <- subset(RII.links, Shade == "shade2" & Salt == "6ppt")

net <- graph_from_data_frame(d = links, vertices = nodespv, directed = T)

V(net)$color = V(net)$Habitat
V(net)$color = gsub("Upland", "gold4", V(net)$color)
V(net)$color = gsub("Marsh", "gold", V(net)$color)
V(net)$frame.color <- NA

V(net)$label.color = "black" 
V(net)$size = 50
V(net)$label.cex = 1
V(net)$type <- V(net)$Habitat %in% nodespv[1,2]

E(net)$color = E(net)$Interaction
E(net)$color = gsub("Competition", "red", E(net)$color)
E(net)$color = gsub("Facilitation", "blue", E(net)$color)
E(net)$color = gsub("Neutral", "grey", E(net)$color)

net <- delete.edges(net, which(E(net)$Interaction == "Neutral"))
all22.N <- delete.edges(net, which(E(net)$Balanced == "Unbalanced"))
all22.W <- delete.edges(net, which(E(net)$Balanced == "Balanced"))

# Build plot

png(filename = "RII.netPv.tiff",
    width = 167.64, height = 167.64, units = "mm", pointsize = 16,
    bg = "transparent", res = 600, family = "", restoreConsole = TRUE,
    type = "windows")
par(mar = c(0, 0, 1, 0))
par(mfrow = c(3, 3))

plot(control.N, layout = layout_in_circle, edge.curved = 0, edge.arrow.size = 0, edge.width = abs(E(control.N)$RIImean) * 2)
title("0% shade - 0 ppt salinity", cex.main = 0.74, col.main = "grey25")
plot(control.W, layout = layout_in_circle, edge.curved = 0.35, edge.arrow.size = 0.3, edge.width = abs(E(control.W)$RIImean) * 2, add = T)
plot(shade1.N, layout = layout_in_circle, edge.curved = 0, edge.arrow.size = 0, edge.width = abs(E(shade1.N)$RIImean) * 2)
title("75% shade - 0 ppt salinity", cex.main = 0.74, col.main = "grey25")
plot(shade1.W, layout = layout_in_circle, edge.curved = 0.35, edge.arrow.size = 0.3, edge.width = abs(E(shade1.W)$RIImean) * 2, add = T)
plot(shade2.N, layout = layout_in_circle, edge.curved = 0, edge.arrow.size = 0, edge.width = abs(E(shade2.N)$RIImean) * 2)
title("90% shade - 0 ppt salinity", cex.main = 0.74, col.main = "grey25")
plot(shade2.W, layout = layout_in_circle, edge.curved = 0.35, edge.arrow.size = 0.3, edge.width = abs(E(shade2.W)$RIImean) * 2, add = T)
plot(salt1.N, layout = layout_in_circle, edge.curved = 0, edge.arrow.size = 0, edge.width = abs(E(salt1.N)$RIImean) * 2)
title("0% shade - 3 ppt salinity", cex.main = 0.74, col.main = "grey25")
plot(salt1.W, layout = layout_in_circle, edge.curved = 0.35, edge.arrow.size = 0.3, edge.width = abs(E(salt1.W)$RIImean) * 3, add = T)
plot(all11.N, layout = layout_in_circle, edge.curved = 0, edge.arrow.size = 0, edge.width = abs(E(all11.N)$RIImean) * 3)
title("75% shade - 3 ppt salinity", cex.main = 0.74, col.main = "grey25")
plot(all11.W, layout = layout_in_circle, edge.curved = 0.35, edge.arrow.size = 0.3, edge.width = abs(E(all11.W)$RIImean) * 3, add = T)
plot(all12.N, layout = layout_in_circle, edge.curved = 0, edge.arrow.size = 0, edge.width = abs(E(all12.N)$RIImean) * 3)
title("90% shade - 3 ppt salinity", cex.main = 0.74, col.main = "grey25")
plot(all12.W, layout = layout_in_circle, edge.curved = 0.35, edge.arrow.size = 0.3, edge.width = abs(E(all12.W)$RIImean) * 3, add = T)
plot(salt2.N, layout = layout_in_circle, edge.curved = 0, edge.arrow.size = 0, edge.width = abs(E(salt2.N)$RIImean) * 3)
title("0% shade - 6 ppt salinity", cex.main = 0.74, col.main = "grey25")
plot(salt2.W, layout = layout_in_circle, edge.curved = 0.35, edge.arrow.size = 0.3, edge.width = abs(E(salt2.W)$RIImean) * 3, add = T)
plot(all21.N, layout = layout_in_circle, edge.curved = 0, edge.arrow.size = 0, edge.width = abs(E(all21.N)$RIImean) * 3)
title("75% shade - 6 ppt salinity", cex.main = 0.74, col.main = "grey25")
plot(all21.W, layout = layout_in_circle, edge.curved = 0.35, edge.arrow.size = 0.3, edge.width = abs(E(all21.W)$RIImean) * 3, add = T)
plot(all22.N, layout = layout_in_circle, edge.curved = 0, edge.arrow.size = 0, edge.width = abs(E(all22.N)$RIImean) * 3)
title("90% shade - 6 ppt salinity", cex.main = 0.74, col.main = "grey25")
plot(all22.W, layout = layout_in_circle, edge.curved = 0.2, edge.arrow.size = 0.3, edge.width = abs(E(all22.W)$RIImean) * 3, add = T)

dev.off()

