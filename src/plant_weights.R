library(tidyverse)

openxlsx::read.xlsx("data/.xlsx")


setwd("/Users/kgedan/Documents/projects_ongoing/greenhouse exp/data")
weights = read.csv("plant_weights.csv", header = TRUE, na.strings = "NA")

#remove bad data rows
weights = na.omit(weights)
#check to see that the number of observations goes down
#remove comment columns
weights = weights[,1:7]
head(weights)
str(weights)

assign = read.csv("plant_assignments.csv", header = TRUE, na.strings = "NA")
levels(assign$shade)
assign$shadetype[assign$shade == "C1" | assign$shade == "C2" | assign$shade == "C3" | assign$shade == "C4"] = "control"
assign$shadetype[assign$shade == "D1" | assign$shade == "D2" | assign$shade == "D3"| assign$shade == "D4"] = "double"
assign$shadetype[assign$shade == "S1" | assign$shade == "S2" | assign$shade == "S3"| assign$shade == "S4"] = "single"
assign$shadetype = as.factor(assign$shadetype)
head(assign)
str(assign)
assign$salt = as.factor(assign$salt)

library(reshape2)

#data checks on weights data
#need to make sure there are not duplicates in the data 
a = melt(weights, na.rm = FALSE, id.vars=c("tag_spp", "tag_no"), measure.vars="roots")
str(a)
dcast(a, tag_spp ~ tag_no, length)
# for mono spp treatments with values >1 -> duplicate
# for combo spp treatments with values >2 -> duplicate
# go back and throwout duplicates for the merge with plant_assignments
# values that are <1 and <2, respectively, indicate missing data, but that's ok
# data were lost to mice getting into samples and data entry mistakes that could not be resolved
M = merge(weights, assign, all=TRUE)
missing = M[!complete.cases(M),] # where are the NAs? 
missing

# work with the merged data - add response variables and calculate RII
M1 = na.omit(M)
M1$rootshoot = M1$roots/M1$shoots # root:shoot ratio
M1$totalbio = M1$roots + M1$shoots # total biomass (above and belowgr)
M1$neighbors = factor(ifelse(M1$combo == "Ds" | M1$combo == "Pa" | M1$combo == "Pv" | 
                     M1$combo == "Sa" | M1$combo == "Sp" | M1$combo == "Spu", 
                     c("solo"), c("neighbor")))
M1$combo2 = paste(as.character(M1$spp),as.character(M1$neighbors), sep="-")
head(M1); str(M1)

solo = M1[M1$neighbors == "solo",]

boxplot(totalbio~M1$combo2, data=M1, cex.axis=0.7, las=2, ylab="Total biomass")
#overall, biomass is higher in solo treatments

boxplot(rootshoot~M1$combo2, data=M1, cex.axis=0.7, las=2, ylim=c(0,6), 
        ylab="Root:shoot ratio")
#overall, root:shoot ratios are lower in solo treatments -> 
#greater investment in roots with neighbors -> evidence of competition 
#each of these graphs would make a good bar graphs - need means and s.e.
#t-test within species to see biomass differences

# RII
# calculate Relative Interaction Index (RII) = (Bn – Bo)/(Bn + Bo)
#calculate Bn and Bo for each species
#   Bn = mean biomass when grown with neighbors (competitors / facilitators)
#   Bo = mean biomass when grown alone
#   within like salt treatments in a shade block
#   block (e.g. S1, S2, S3, S4) is the replicate
M1$salt_shade = factor(paste(as.character(M1$salt), as.character(M1$shade), sep ="-"))
M1.melt = melt(data=M1, id.vars=c("spp", "competitor", "salt_shade"), 
               measure.vars=c("totalbio"))
head(M1.melt)

#just to see it - data within a block for one species in all combos
M1.melt[M1.melt$spp == "Ds" & M1.melt$salt_shade == "3-S3",]
M1.melt[M1.melt$spp == "Ds" & M1.melt$salt_shade == "6-S3",]
M1.melt[M1.melt$spp == "Ds" & M1.melt$salt_shade == "0-S3",]


# calculating Bn 
M1.melt.comp = M1.melt[M1.melt$competitor != "none",]; head(M1.melt.comp)
mean.Bn = dcast(M1.melt.comp, variable + spp ~ salt_shade, mean)
mean.Bn

# calculating Bo
M1.melt.solo = M1.melt[M1.melt$competitor == "none",]; head(M1.melt.solo)
mean.Bo = dcast(M1.melt.solo, variable + spp ~ salt_shade, mean)
mean.Bo
str(mean.Bo)

RII = (mean.Bn - mean.Bo)/(mean.Bn + mean.Bo)
RII$spp = mean.Bo$spp

#test for differences in RII across spp, salt, and shade treatments
# a massive ANOVA
# recode salt_shade variable as factors
a = melt(RII, id.vars="spp")
a = na.omit(a); 
a$var = droplevels(a$var); a$spp = droplevels(a$spp)
a$salt[a$var ==  "0-C1" | a$var ==  "0-C2" | a$var ==  "0-C3" | 
            a$var ==  "0-C4" | a$var ==  "0-D1" | a$var ==  "0-D2" | 
            a$var ==  "0-D3" | a$var ==  "0-D4" | a$var ==  "0-S1" | 
            a$var ==  "0-S2" | a$var ==  "0-S3" | a$var ==  "0-S4"] = "0"
a$salt[a$var ==  "3-C1" | a$var ==  "3-C2" | a$var ==  "3-C3" | 
            a$var ==  "3-C4" | a$var ==  "3-D1" | a$var ==  "3-D2" | 
            a$var ==  "3-D3" | a$var ==  "3-D4" | a$var ==  "3-S1" | 
            a$var ==  "3-S2" | a$var ==  "3-S3" | a$var ==  "3-S4"] = "3"
a$salt[a$var ==  "6-C1" | a$var ==  "6-C2" | a$var ==  "6-C3" | 
            a$var ==  "6-C4" | a$var ==  "6-D1" | a$var ==  "6-D2" | 
            a$var ==  "6-D3" | a$var ==  "6-D4" | a$var ==  "6-S1" | 
            a$var ==  "6-S2" | a$var ==  "6-S3" | a$var ==  "6-S4"] = "6"
a$shade[a$var ==  "0-C1" | a$var ==  "0-C2" | a$var ==  "0-C3" | 
            a$var ==  "0-C4" | a$var ==  "3-C1" | a$var ==  "3-C2" | 
            a$var ==  "3-C3" | a$var ==  "3-C4" | a$var ==  "6-C1" | 
            a$var ==  "6-C2" | a$var ==  "6-C3" | a$var ==  "6-C4"] = "control"
a$shade[a$var ==  "0-S1" | a$var ==  "0-S2" | a$var ==  "0-S3" | 
            a$var ==  "0-S4" | a$var ==  "3-S1" | a$var ==  "3-S2" | 
            a$var ==  "3-S3" | a$var ==  "3-S4" | a$var ==  "6-S1" | 
            a$var ==  "6-S2" | a$var ==  "6-S3" | a$var ==  "6-S4"] = "single"
a$shade[a$var ==  "0-D1" | a$var ==  "0-D2" | a$var ==  "0-D3" | 
            a$var ==  "0-D4" | a$var ==  "3-D1" | a$var ==  "3-D2" | 
            a$var ==  "3-D3" | a$var ==  "3-D4" | a$var ==  "6-D1" | 
            a$var ==  "6-D2" | a$var ==  "6-D3" | a$var ==  "6-D4"] = "double"
a$shade = as.factor(a$shade)
a$salt = as.factor(a$salt)
head(a); str(a)
mod = aov(value ~ spp*salt*shade, data = a)
summary(mod) # significant effects of spp and shade on RII, no interactions
    # no spp*shade or spp*salt interactions means that species are affected the same 
    # by treatments (both salt marsh and upland understory plants)
TukeyHSD(mod, which=c("spp", "shade"))
  # Pa is different from all other species
  # double shade is different from control - RII is more positive in deep shade
boxplot(value ~ spp*salt*shade, data = a, las=2, cex.axis=0.7)


#recode salt and shade treatments in Bo and Bn for ANOVA
B.o = melt(mean.Bo, id.vars="spp")
B.o = B.o[B.o$value != NaN,] # get rid of row 206 where value = NaN
B.o$var = droplevels(B.o$var); B.o$spp = droplevels(B.o$spp)
B.o$salt[B.o$var ==  "0-C1" | B.o$var ==  "0-C2" | B.o$var ==  "0-C3" | 
         B.o$var ==  "0-C4" | B.o$var ==  "0-D1" | B.o$var ==  "0-D2" | 
         B.o$var ==  "0-D3" | B.o$var ==  "0-D4" | B.o$var ==  "0-S1" | 
         B.o$var ==  "0-S2" | B.o$var ==  "0-S3" | B.o$var ==  "0-S4"] = "0"
B.o$salt[B.o$var ==  "3-C1" | B.o$var ==  "3-C2" | B.o$var ==  "3-C3" | 
         B.o$var ==  "3-C4" | B.o$var ==  "3-D1" | B.o$var ==  "3-D2" | 
         B.o$var ==  "3-D3" | B.o$var ==  "3-D4" | B.o$var ==  "3-S1" | 
         B.o$var ==  "3-S2" | B.o$var ==  "3-S3" | B.o$var ==  "3-S4"] = "3"
B.o$salt[B.o$var ==  "6-C1" | B.o$var ==  "6-C2" | B.o$var ==  "6-C3" | 
         B.o$var ==  "6-C4" | B.o$var ==  "6-D1" | B.o$var ==  "6-D2" | 
         B.o$var ==  "6-D3" | B.o$var ==  "6-D4" | B.o$var ==  "6-S1" | 
         B.o$var ==  "6-S2" | B.o$var ==  "6-S3" | B.o$var ==  "6-S4"] = "6"
B.o$shade[B.o$var ==  "0-C1" | B.o$var ==  "0-C2" | B.o$var ==  "0-C3" | 
          B.o$var ==  "0-C4" | B.o$var ==  "3-C1" | B.o$var ==  "3-C2" | 
          B.o$var ==  "3-C3" | B.o$var ==  "3-C4" | B.o$var ==  "6-C1" | 
          B.o$var ==  "6-C2" | B.o$var ==  "6-C3" | B.o$var ==  "6-C4"] = "control"
B.o$shade[B.o$var ==  "0-S1" | B.o$var ==  "0-S2" | B.o$var ==  "0-S3" | 
          B.o$var ==  "0-S4" | B.o$var ==  "3-S1" | B.o$var ==  "3-S2" | 
          B.o$var ==  "3-S3" | B.o$var ==  "3-S4" | B.o$var ==  "6-S1" | 
          B.o$var ==  "6-S2" | B.o$var ==  "6-S3" | B.o$var ==  "6-S4"] = "single"
B.o$shade[B.o$var ==  "0-D1" | B.o$var ==  "0-D2" | B.o$var ==  "0-D3" | 
          B.o$var ==  "0-D4" | B.o$var ==  "3-D1" | B.o$var ==  "3-D2" | 
          B.o$var ==  "3-D3" | B.o$var ==  "3-D4" | B.o$var ==  "6-D1" | 
          B.o$var ==  "6-D2" | B.o$var ==  "6-D3" | B.o$var ==  "6-D4"] = "double"
B.o$shade = as.factor(B.o$shade)
B.o$salt = as.factor(B.o$salt)
B.o$value = as.numeric(B.o$value)
B.o = na.omit(B.o)
head(B.o); str(B.o)
mod.B.o = aov(value ~ spp*salt*shade, data = B.o)
summary(mod.B.o) # spp and shade factors highly significant
TukeyHSD(mod.B.o, which=c("spp", "shade"))
# many species are different from one another in total biomass
# biomass is lower in single and double shade treatments relative to controls
boxplot(value ~ spp*salt*shade, data = B.o, las=2, cex.axis=0.7)

# see how factors affect Bn = biomass when neighbors are present
B.n = melt(mean.Bn, id.vars="spp")
B.n$var = droplevels(B.n$var); B.n$spp = droplevels(B.n$spp)
B.n$salt[B.n$var ==  "0-C1" | B.n$var ==  "0-C2" | B.n$var ==  "0-C3" | 
           B.n$var ==  "0-C4" | B.n$var ==  "0-D1" | B.n$var ==  "0-D2" | 
           B.n$var ==  "0-D3" | B.n$var ==  "0-D4" | B.n$var ==  "0-S1" | 
           B.n$var ==  "0-S2" | B.n$var ==  "0-S3" | B.n$var ==  "0-S4"] = "0"
B.n$salt[B.n$var ==  "3-C1" | B.n$var ==  "3-C2" | B.n$var ==  "3-C3" | 
           B.n$var ==  "3-C4" | B.n$var ==  "3-D1" | B.n$var ==  "3-D2" | 
           B.n$var ==  "3-D3" | B.n$var ==  "3-D4" | B.n$var ==  "3-S1" | 
           B.n$var ==  "3-S2" | B.n$var ==  "3-S3" | B.n$var ==  "3-S4"] = "3"
B.n$salt[B.n$var ==  "6-C1" | B.n$var ==  "6-C2" | B.n$var ==  "6-C3" | 
           B.n$var ==  "6-C4" | B.n$var ==  "6-D1" | B.n$var ==  "6-D2" | 
           B.n$var ==  "6-D3" | B.n$var ==  "6-D4" | B.n$var ==  "6-S1" | 
           B.n$var ==  "6-S2" | B.n$var ==  "6-S3" | B.n$var ==  "6-S4"] = "6"
B.n$shade[B.n$var ==  "0-C1" | B.n$var ==  "0-C2" | B.n$var ==  "0-C3" | 
            B.n$var ==  "0-C4" | B.n$var ==  "3-C1" | B.n$var ==  "3-C2" | 
            B.n$var ==  "3-C3" | B.n$var ==  "3-C4" | B.n$var ==  "6-C1" | 
            B.n$var ==  "6-C2" | B.n$var ==  "6-C3" | B.n$var ==  "6-C4"] = "control"
B.n$shade[B.n$var ==  "0-S1" | B.n$var ==  "0-S2" | B.n$var ==  "0-S3" | 
            B.n$var ==  "0-S4" | B.n$var ==  "3-S1" | B.n$var ==  "3-S2" | 
            B.n$var ==  "3-S3" | B.n$var ==  "3-S4" | B.n$var ==  "6-S1" | 
            B.n$var ==  "6-S2" | B.n$var ==  "6-S3" | B.n$var ==  "6-S4"] = "single"
B.n$shade[B.n$var ==  "0-D1" | B.n$var ==  "0-D2" | B.n$var ==  "0-D3" | 
            B.n$var ==  "0-D4" | B.n$var ==  "3-D1" | B.n$var ==  "3-D2" | 
            B.n$var ==  "3-D3" | B.n$var ==  "3-D4" | B.n$var ==  "6-D1" | 
            B.n$var ==  "6-D2" | B.n$var ==  "6-D3" | B.n$var ==  "6-D4"] = "double"
B.n$shade = as.factor(B.n$shade)
B.n$shade = factor(B.n$shade, levels = c("control", "single","double"))
B.n$salt = as.factor(B.n$salt)
B.n$value = as.numeric(B.n$value)
B.n$spp = factor(B.n$spp, levels = c("Pa", "Ds", "Spu", "Pv", "Sa", "Sp"))
B.n = na.omit(B.n)
head(B.n); str(B.n)
mod.B.n = aov(value ~ spp*salt*shade, data = B.n)
summary(mod.B.n) # spp and shade factors highly significant; salt, spp*salt, and spp*shade terms also significant
TukeyHSD(mod.B.n, which=c("spp", "shade", "salt", "spp:salt", "spp:shade"))
  # spp effect: many species, when in comp, are different from one another in total biomass
  # shade effect: biomass is lower in single and double shade treatments relative to controls
  # salt effect: biomass is lower in 6ppt than 0ppt
# but because significant interactions - 
# need to interpret these over the independent predictor variable effects
  # spp:salt effect: these could result from salt affecting Bn of some species and not others
      # or from some salt-species combinations differing from others, which would be expected since species differ
      # based on Tukeys results: only one difference due to salt within species: 
        # Spu (with neighbors) 6ppt was lower than Spu (with neighbors) 0ppt
  # spp:shade effect: similarly, these could come from shade affecting Bn of some spp and not others
      # or from some shade-spp combos differing from others, which is highly likely
      # based on Tukeys results: only two diff due to shade within species effects: 
        # Spu with neighbors in double shade diff from control
        # Spu with neighbors in single shade diff from control
boxplot(value ~ spp*salt*shade, data = B.n, las=2, cex.axis=0.7)

# BAR GRAPHS of Bo, Bn, and RII

#first, a function for standard error of a mean
se = function(x) {
    n = sum(!is.na(x))
    sqrt(var(x,na.rm=T)/n)
      }

B.o.mean = acast(B.o, spp ~ shade, mean)
y = B.o.mean[c(2,3,1,6,4,5),c(1,3,2)]
spp = c("P. australis", "P. virgatum", "D. spicata", 
        "S. pungens", "S. alterniflora", "S. patens")
B.o.se = acast(B.o, spp ~ shade, se)
y.se = B.o.se[c(2,3,1,6,4,5),c(1,3,2)]
bp = barplot(y, beside=TRUE, ylim=c(0,16), xaxt="n", col = c("orange1", 
          "orange3", "palegreen", "springgreen2", "springgreen3", "springgreen4"), 
             las=2, ylab="", cex.axis = 1.25, cex.lab = 1.25)
mtext("Total biomass (g)", 2, cex=1.2, line=2)
axis(1, at=c(1,25), labels=c("",""))
axis(1, at=c(4,11,18), labels=c("100%\n (control)", "25%\n(sub-canopy)","10%\n(understory)"), 
     tick=FALSE, cex.axis=1.25, line=1)
arrows(bp, y, bp, y + y.se, lwd = 1.5,
       angle = 90, length = 0.05)
legend(2,16.5, spp, bty ="n", fill= c("orange1", "orange3", "palegreen", 
          "springgreen2", "springgreen3", "springgreen4"), ncol=3)

B.n.mean = acast(B.n, spp ~ shade, mean)
z = B.n.mean[c(1,4,2,3,5,6),]
B.n.se = acast(B.n, spp ~ shade, se)
z.se = B.n.se[c(1,4,2,3,5,6),]
bp = barplot(z, beside=TRUE, ylim=c(0,16), xaxt="n", col = c("orange1", "orange3", 
             "palegreen", "springgreen2", "springgreen3", "springgreen4"), 
             las=2, cex.axis = 1.25, cex.lab = 1.25)
mtext("Total biomass (g)", 2, cex=1.2, line=2)
axis(1, at=c(1,25), labels=c("",""))
axis(1, at=c(4,11,18), labels=c("100%\n (control)", "25%\n(sub-canopy)","10%\n(understory)"), 
     tick=FALSE, cex.axis=1.25, line=1)
arrows(bp, z, bp, z + z.se, lwd = 1.5,
       angle = 90, length = 0.05)
legend(1,16.5, spp, bty ="n", fill= c("orange1", "orange3", 
       "palegreen", "springgreen2", "springgreen3", "springgreen4"), ncol=3)

#Graph RII (with x axis at y=0)
a.mean = acast(a, spp ~ shade, mean)
x = a.mean[c(2,3,1,6,4,5),c(1,3,2)]
a.se = acast(a, spp ~ shade, se)
x.se = a.se[c(2,3,1,6,4,5),c(1,3,2)]
bp = barplot(x, beside=TRUE, ylim=c(-1,1), xaxt="n", col = c("orange1", "orange3", 
            "palegreen", "springgreen2", "springgreen3", "springgreen4"), 
             las=2, cex.axis = 1.25, cex.lab = 1.25)
mtext("RII", 2, line=3, cex=1.2)
axis(1, at=c(1,25), pos=0, labels=c("",""))
axis(1, at=c(4,11,18), labels=c("100%\n (control)", "25%\n(sub-canopy)","10%\n(understory)"), 
     tick=FALSE, cex.axis=1.25)
arrows(bp, x, bp, x - x.se, lwd = 1,
       angle = 90, length = 0.05)
arrows(bp, x, bp, x + x.se, lwd = 1,
       angle = 90, length = 0.05)
legend(3,1, spp, bty ="n", fill= c("orange1", "orange3", 
      "palegreen", "springgreen2", "springgreen3", "springgreen4"), ncol=3)

##########################################

#comparing root:shoot ratios
head(M1); str(M1)
mod.rootshoot = aov(rootshoot~spp*neighbors*salt*shadetype, data = M1)
summary(mod.rootshoot)
# Phragmites root:shoot bargraph
Pa_rootshoot = M1[M1$spp=="Pa", c("neighbors","rootshoot","salt", "shadetype")]
Pa.m = melt(Pa_rootshoot, measure.vars="rootshoot")
Pa.c = acast(Pa.m, neighbors ~ shadetype, mean)
v = Pa.c[,c(1,3,2)]
Pa.se = acast(Pa.m, neighbors ~ shadetype, se)
v.se = Pa.se[,c(1,3,2)]
bp = barplot(v, beside=T, ylim = c(0,3), las=2, ylab = "Root:shoot", xaxt = "n", main = "Phragmites australis") 
axis(1, at=c(2,5,8), labels=c("100%\n (control)", "25%\n(sub-canopy)","10%\n(understory)"), 
     tick=FALSE, cex.axis=1.25, line=1)
arrows(bp, v, bp, v + v.se, lwd = 1,
       angle = 90, length = 0.05)
legend(2,3, c("with neighbors", "without"), bty ="n", fill= c("black", "gray"),ncol=1)

# Spartina patens
Sp_rootshoot = M1[M1$spp=="Sp", c("neighbors","rootshoot","salt", "shadetype")]
Sp.m = melt(Sp_rootshoot, measure.vars="rootshoot")
Sp.c = acast(Sp.m, neighbors ~ shadetype, mean)
v = Sp.c[,c(1,3,2)]
Sp.se = acast(Sp.m, neighbors ~ shadetype, se)
v.se = Sp.se[,c(1,3,2)]
bp = barplot(v, beside=T, ylim = c(0,2), las=2, ylab = "Root:shoot", xaxt = "n",
             main = "Spartina patens") 
axis(1, at=c(2,5,8), labels=c("100%\n (control)", "25%\n(sub-canopy)","10%\n(understory)"), 
     tick=FALSE, cex.axis=1.25, line=1)
arrows(bp, v, bp, v + v.se, lwd = 1,
       angle = 90, length = 0.05)
legend(2,2, c("with neighbors", "without"), bty ="n", fill= c("black", "gray"), ncol=1)

#Distichlis spicata
Ds_rootshoot = M1[M1$spp=="Ds", c("neighbors","rootshoot","salt", "shadetype")]
Ds.m = melt(Ds_rootshoot, measure.vars="rootshoot")
Ds.c = acast(Ds.m, neighbors ~ shadetype, mean)
v = Ds.c[,c(1,3,2)]
Ds.se = acast(Ds.m, neighbors ~ shadetype, se)
v.se = Ds.se[,c(1,3,2)]
bp = barplot(v, beside=T, ylim = c(0,3.75), las=2, ylab = "Root:shoot", xaxt = "n",
             main = "Distichlis spicata") 
axis(1, at=c(2,5,8), labels=c("100%\n (control)", "25%\n(sub-canopy)","10%\n(understory)"), 
     tick=FALSE, cex.axis=1.25, line=1)
arrows(bp, v, bp, v + v.se, lwd = 1,
       angle = 90, length = 0.05)
legend(2,3.5, c("with neighbors", "without"), bty ="n", fill= c("black", "gray"), ncol=1)

#Spartina alterniflora
Sa_rootshoot = M1[M1$spp=="Sa", c("neighbors","rootshoot","salt", "shadetype")]
Sa.m = melt(Sa_rootshoot, measure.vars="rootshoot")
Sa.c = acast(Sa.m, neighbors ~ shadetype, mean)
v = Sa.c[,c(1,3,2)]
Sa.se = acast(Sa.m, neighbors ~ shadetype, se)
v.se = Sa.se[,c(1,3,2)]
bp = barplot(v, beside=T, ylim = c(0,3.6), las=2, ylab = "Root:shoot", xaxt = "n",
             main = "Spartina alterniflora") 
axis(1, at=c(2,5,8), labels=c("100%\n (control)", "25%\n(sub-canopy)","10%\n(understory)"), 
     tick=FALSE, cex.axis=1.25, line=1)
arrows(bp, v, bp, v + v.se, lwd = 1,
       angle = 90, length = 0.05)
legend(2,3.5, c("with neighbors", "without"), bty ="n", fill= c("black", "gray"), ncol=1)

#Panicum virgatum
Pv_rootshoot = M1[M1$spp=="Pv", c("neighbors","rootshoot","salt", "shadetype")]
Pv.m = melt(Pv_rootshoot, measure.vars="rootshoot")
Pv.c = acast(Pv.m, neighbors ~ shadetype, mean)
v = Pv.c[,c(1,3,2)]
Pv.se = acast(Pv.m, neighbors ~ shadetype, se)
v.se = Pv.se[,c(1,3,2)]
bp = barplot(v, beside=T, ylim = c(0,3), las=2, ylab = "Root:shoot", xaxt = "n",
             main = "Panicum virgatum") 
axis(1, at=c(2,5,8), labels=c("100%\n (control)", "25%\n(sub-canopy)","10%\n(understory)"), 
     tick=FALSE, cex.axis=1.25, line=1)
arrows(bp, v, bp, v + v.se, lwd = 1,
       angle = 90, length = 0.05)
legend(2,3, c("with neighbors", "without"), bty ="n", fill= c("black", "gray"), ncol=1)

# Schoenoplectus pungens
Spu_rootshoot = M1[M1$spp=="Spu", c("neighbors","rootshoot","salt", "shadetype")]
Spu.m = melt(Spu_rootshoot, measure.vars="rootshoot")
Spu.c = acast(Spu.m, neighbors ~ shadetype, mean)
v = Spu.c[,c(1,3,2)]
Spu.se = acast(Spu.m, neighbors ~ shadetype, se)
v.se = Spu.se[,c(1,3,2)]
bp = barplot(v, beside=T, ylim = c(0,5.5), las=2, ylab = "Root:shoot", xaxt = "n",
             main = "Schoenoplectus pungens") 
axis(1, at=c(2,5,8), labels=c("100%\n (control)", "25%\n(sub-canopy)","10%\n(understory)"), 
     tick=FALSE, cex.axis=1.25, line=1)
arrows(bp, v, bp, v + v.se, lwd = 1,
       angle = 90, length = 0.05)
legend(2,5, c("with neighbors", "without"), bty ="n", fill= c("black", "gray"), ncol=1)

# Grand ANOVA on biomass
str(M1)
mod.biomass = aov(totalbio~spp*neighbors*salt*shadetype, data = M1)
summary(mod.biomass)
# need to revise this ANOVA style to account for blocking of the shading treatment
mod.abovegr = aov(shoots~spp*neighbors*salt*shadetype, data = M1)
summary(mod.abovegr)
TukeyHSD(mod.abovegr, c("spp", "neighbors", "salt", "shadetype", "spp:neighbors"))
Bo.shoot = M1[M1$neighbors=="solo", c("spp","shoots","salt", "shadetype")]
xy = melt(Bo.shoot, measure.vars="shoots")
xy.mean = acast(xy, spp ~ shadetype, mean)
xyz = xy.mean[c(2,3,1,6,4,5),c(1,3,2)]
xy.se = acast(xy, spp ~ shadetype, se)
xyz.se = xy.se[c(2,3,1,6,4,5),c(1,3,2)]
bp = barplot(xyz, beside=TRUE, ylim=c(0,8), xaxt="n", col = c("orange1", "orange3", 
             "palegreen", "springgreen2", "springgreen3", "springgreen4"), 
             las=2, cex.axis = 1.25, cex.lab = 1.25)
mtext("Aboveground biomass (g)", 2, cex=1.2, line=2)
axis(1, at=c(1,25), labels=c("",""))
axis(1, at=c(4,11,18), labels=c("100%\n (control)", "25%\n(sub-canopy)","10%\n(understory)"), 
     tick=FALSE, cex.axis=1.25, line=1)
arrows(bp, xyz, bp, xyz + xyz.se, lwd = 1.5,
       angle = 90, length = 0.05)
legend(1,8.5, spp, bty ="n", fill= c("orange1", "orange3", 
       "palegreen", "springgreen2", "springgreen3", "springgreen4"), ncol=3)

#abovegr biomass with neighbors
Bn.shoot = M1[M1$neighbors=="neighbor", c("spp","shoots","salt", "shadetype")]
ab = melt(Bn.shoot, measure.vars="shoots")
ab.mean = acast(ab, spp ~ shadetype, mean)
abc = ab.mean[c(2,3,1,6,4,5),c(1,3,2)]
ab.se = acast(ab, spp ~ shadetype, se)
abc.se = ab.se[c(2,3,1,6,4,5),c(1,3,2)]
bp = barplot(abc, beside=TRUE, ylim=c(0,8), xaxt="n", col = c("orange1", "orange3", 
            "palegreen", "springgreen2", "springgreen3", "springgreen4"), 
             las=2, cex.axis = 1.25, cex.lab = 1.25, main="With neighbors")
mtext("Aboveground biomass (g)", 2, cex=1.2, line=2)
axis(1, at=c(1,25), labels=c("",""))
axis(1, at=c(4,11,18), labels=c("100%\n (control)", "25%\n(sub-canopy)","10%\n(understory)"), 
     tick=FALSE, cex.axis=1.25, line=1)
arrows(bp, abc, bp, abc + abc.se, lwd = 1.5,
       angle = 90, length = 0.05)
legend(1,8.5, spp, bty ="n", fill= c("orange1", "orange3", 
     "palegreen", "springgreen2", "springgreen3", "springgreen4"), ncol=3)

#exploring the salt effect, which is stronger on abovegr biomass than total biomass
salt.mean = acast(xy, spp ~ salt, mean)
salt.mean2 = salt.mean[c(2,3,1,6,4,5),]
salt.se = acast(xy, spp ~ salt, se)
salt.se2 = salt.se[c(2,3,1,6,4,5),]
bp = barplot(salt.mean2, beside=TRUE, ylim=c(0,8), xaxt="n", col = c("orange1", "orange3", 
             "palegreen", "springgreen2", "springgreen3", "springgreen4"), 
             las=2, cex.axis = 1.25, cex.lab = 1.25, main="Without neighbors")
mtext("Aboveground biomass (g)", 2, cex=1.2, line=2)
axis(1, at=c(1,25), labels=c("",""))
axis(1, at=c(4,11,18), labels=c("0ppt", "3ppt","6ppt"), 
     tick=FALSE, cex.axis=1.25, line=1)
arrows(bp, salt.mean2, bp, salt.mean2 + salt.se2, lwd = 1.5,
       angle = 90, length = 0.05)
legend(1,8.5, spp, bty ="n", fill= c("orange1", "orange3", 
      "palegreen", "springgreen2", "springgreen3", "springgreen4"), ncol=3)

