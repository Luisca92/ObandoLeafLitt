###################################### LOAD PACKAGES ##################################### 
setwd("~/Desktop/R") # Set the working directory

library(plyr) # Necessary for Rmisc
library(Rmisc) # Necessary for summarySE
library(dplyr) # Necessary for group_by
library(reshape) # Necessary for melt & cast
library(ggplot2) # Necessary for ggplots
library(grid) # Necessary for pushViewPort
library(ggpubr) # Necessary for ggarrange
library(conover.test) # Necessary for conover test
library(lmtest) # Necessary for homoscedasticity test.
library(lme4) # Generalized linear models and mixed effects.
library(lmerTest) # Necessary to get p-vals in glms
library(lubridate) # Necessary for monthiness? 
library(ggthemes) # Necessary for cool themes 
library(emmeans) # Display all pairwise comparisons
library(ggfortify) # Necessary for PCA. 
library(patchwork) # Necessary for mixing ggplots and legends

##################################### DATAFRAME SETUP #################################### 

LeafLitt <- read.csv("Data/Leaf.Litter.csv") 
Clim <- read.csv("Data/Clim.csv") 
Struc <- read.csv("Data/StrucDF.csv") 
Solar <- read.csv("Data/SolarRadiation.csv") 
Irad <- read.csv("Data/SolarIrradiation.csv") 

LeafLitt$Paisaje <- as.factor(LeafLitt$Paisaje) 
LeafLitt$Sample <- as.factor(LeafLitt$Sample) 
LeafLitt$Locality <- as.factor(LeafLitt$Locality) 
LeafLitt$Successional.State <- as.factor(LeafLitt$Successional.State) 
LeafLitt$Succession <- as.factor(LeafLitt$Succession) 
LeafLitt$Plot <- as.factor(LeafLitt$Plot) 
LeafLitt$Record <- as.factor(LeafLitt$Record) 
LeafLitt$PlotSample <- paste(LeafLitt$Succession, LeafLitt$Sample)

Clim$Landscape <- as.factor(Clim$Landscape) 
Clim$Plot <- as.factor(Clim$Plot) 
Clim$Sample <- as.factor(Clim$Sample) 

Struc$Landscape <- as.factor(Struc$Landscape) 
Struc$Plot <- as.factor(Struc$Plot) 
Struc$Succession <- as.factor(Struc$Succession) 
Struc$CodeZ <- as.factor(Struc$CodeZ) 

# Records: 353 (DM2), 365(RM), and 377(RM) did not collect any leaf litter. Why? 
# They were all part of muestreo 5. 
# Perhaps best to set them as NA. 

LeafLitt$LeafLitter.g[which(LeafLitt$Record == "353")] <- NA
LeafLitt$LeafLitter.g[which(LeafLitt$Record == "365")] <- NA
LeafLitt$LeafLitter.g[which(LeafLitt$Record == "377")] <- NA

LeafLitt$Branches.g[which(LeafLitt$Record == "353")] <- NA
LeafLitt$Branches.g[which(LeafLitt$Record == "365")] <- NA
LeafLitt$Branches.g[which(LeafLitt$Record == "377")] <- NA

LeafLitt$Detritus.g[which(LeafLitt$Record == "353")] <- NA
LeafLitt$Detritus.g[which(LeafLitt$Record == "365")] <- NA
LeafLitt$Detritus.g[which(LeafLitt$Record == "377")] <- NA

LeafLitt$Flowers.g[which(LeafLitt$Record == "353")] <- NA
LeafLitt$Flowers.g[which(LeafLitt$Record == "365")] <- NA
LeafLitt$Flowers.g[which(LeafLitt$Record == "377")] <- NA

LeafLitt$Fruits.g[which(LeafLitt$Record == "353")] <- NA
LeafLitt$Fruits.g[which(LeafLitt$Record == "365")] <- NA
LeafLitt$Fruits.g[which(LeafLitt$Record == "377")] <- NA

LeafLitt$Total.g[which(LeafLitt$Record == "353")] <- NA
LeafLitt$Total.g[which(LeafLitt$Record == "365")] <- NA
LeafLitt$Total.g[which(LeafLitt$Record == "377")] <- NA

LeafLitt$LeafLitter.Mg.ha.month[which(LeafLitt$Record == "353")] <- NA
LeafLitt$LeafLitter.Mg.ha.month[which(LeafLitt$Record == "365")] <- NA
LeafLitt$LeafLitter.Mg.ha.month[which(LeafLitt$Record == "377")] <- NA

LeafLitt$Branches.Mg.ha.month[which(LeafLitt$Record == "353")] <- NA
LeafLitt$Branches.Mg.ha.month[which(LeafLitt$Record == "365")] <- NA
LeafLitt$Branches.Mg.ha.month[which(LeafLitt$Record == "377")] <- NA

LeafLitt$Detritus.Mg.ha.month[which(LeafLitt$Record == "353")] <- NA
LeafLitt$Detritus.Mg.ha.month[which(LeafLitt$Record == "365")] <- NA
LeafLitt$Detritus.Mg.ha.month[which(LeafLitt$Record == "377")] <- NA

LeafLitt$Flowers.Mg.ha.month[which(LeafLitt$Record == "353")] <- NA
LeafLitt$Flowers.Mg.ha.month[which(LeafLitt$Record == "365")] <- NA
LeafLitt$Flowers.Mg.ha.month[which(LeafLitt$Record == "377")] <- NA

LeafLitt$Fruits.Mg.ha.month[which(LeafLitt$Record == "353")] <- NA
LeafLitt$Fruits.Mg.ha.month[which(LeafLitt$Record == "365")] <- NA
LeafLitt$Fruits.Mg.ha.month[which(LeafLitt$Record == "377")] <- NA

LeafLitt$Total.g[which(LeafLitt$Record == "353")] <- NA
LeafLitt$Total.g[which(LeafLitt$Record == "365")] <- NA
LeafLitt$Total.g[which(LeafLitt$Record == "377")] <- NA

# Remove DV? 
LeafLitt2 <- droplevels(subset(LeafLitt, LeafLitt$Succession != "DV1"))
LeafLitt2$CodeX <- paste(LeafLitt2$Paisaje , LeafLitt2$Successional.State, LeafLitt2$Plot)
LeafLitt2$CodeZ <- paste(LeafLitt2$Succession , LeafLitt2$Plot)

Clim$CodeX <- paste(Clim$Landscape , Clim$Successional.State, Clim$Plot)
Clim$Succession <- with(LeafLitt2, Succession[match(Clim$CodeX, CodeX)]) 
Clim2 <- droplevels(subset(Clim, Clim$Succession != "NA"))
Clim2$CodeY <- paste(Clim2$CodeX , Clim2$Sample)
LeafLitt2$CodeY <- paste(LeafLitt2$CodeX , LeafLitt2$Sample)

# Match Climate
LeafLitt2$RainNow <- with(Clim2, RainNow[match(LeafLitt2$CodeY, Clim2$CodeY)]) 
LeafLitt2$Rain1 <- with(Clim2, Rain1[match(LeafLitt2$CodeY, Clim2$CodeY)]) 
LeafLitt2$Rain2 <- with(Clim2, Rain2[match(LeafLitt2$CodeY, Clim2$CodeY)]) 
LeafLitt2$Rain3 <- with(Clim2, Rain3[match(LeafLitt2$CodeY, Clim2$CodeY)]) 

LeafLitt2$TempNow <- with(Clim2, TempNow[match(LeafLitt2$CodeY, Clim2$CodeY)]) 
LeafLitt2$Temp1 <- with(Clim2, Temp1[match(LeafLitt2$CodeY, Clim2$CodeY)]) 
LeafLitt2$Temp2 <- with(Clim2, Temp2[match(LeafLitt2$CodeY, Clim2$CodeY)]) 
LeafLitt2$Temp3 <- with(Clim2, Temp3[match(LeafLitt2$CodeY, Clim2$CodeY)]) 


# Split 
Lomerio <- droplevels(subset(LeafLitt2, LeafLitt2$Paisaje == "Lomerio"))
Mountain <- droplevels(subset(LeafLitt2, LeafLitt2$Paisaje == "Mountain"))

levels(Lomerio$Sample) <- list("Aug" = "1",  "Sept" = "2", "Oct"  = "3", 
                               "Nov" = "4", "Dec"  = "5", "Jan" = "6",
                               "Feb" = "7", "Mar" = "8", "Apr"  = "9", 
                               "May" = "10", "Jun"  = "11",  "Jul" = "12")

levels(Mountain$Sample) <- list("Sept" = "1",  "Oct" = "2", "Nov"  = "3", 
                                "Dec" = "4", "Jan"  = "5", "Feb" = "6",
                                "Mar" = "7", "Apr" = "8", "May"  = "9", 
                                "Jun" = "10", "Jul"  = "11",  "Aug" = "12")

LomeDL1 <- droplevels(subset(Lomerio, Lomerio$Succession == "DL1"))
LomeDL2 <- droplevels(subset(Lomerio, Lomerio$Succession == "DL2"))
LomeRL <- droplevels(subset(Lomerio, Lomerio$Succession == "RL"))

MounDM1 <- droplevels(subset(Mountain, Mountain$Succession == "DM1"))
MounDM2 <- droplevels(subset(Mountain, Mountain$Succession == "DM2"))
MounRL <- droplevels(subset(Mountain, Mountain$Succession == "RM"))

Mountain$CodeZ <- as.factor(Mountain$CodeZ)
as.data.frame(summary(Mountain$CodeZ))

####################### LITTERFALL AVERAGES AND STANDARD ERROR ####################### 

T1 <- summarySE(LeafLitt2, measurevar="LeafLitter.g", groupvars=c("Succession"), na.rm = TRUE)
T2 <- summarySE(LeafLitt2, measurevar="Branches.g", groupvars=c("Succession"), na.rm = TRUE)
T3 <- summarySE(LeafLitt2, measurevar="Detritus.g", groupvars=c("Succession"), na.rm = TRUE)
T4 <- summarySE(LeafLitt2, measurevar="Flowers.g", groupvars=c("Succession"), na.rm = TRUE)
T5 <- summarySE(LeafLitt2, measurevar="Fruits.g", groupvars=c("Succession"), na.rm = TRUE)
T7 <- summarySE(LeafLitt2, measurevar="Total.g", groupvars=c("Succession"), na.rm = TRUE)

T6 <- T1[,-2]
T6 <- T6[,-5]
T6 <- T6[,-3]

T6$Branches.g <- T2$Branches.g
T6$se2 <- T2$se
T6$Detritus.g <- T3$Detritus.g
T6$se3 <- T3$se
T6$Flowers.g <- T4$Flowers.g
T6$se4 <- T4$se
T6$Fruits.g <- T5$Fruits.g
T6$se5 <- T5$se
T6$Total.g <- T7$Total.g
T6$se6 <- T7$se

T6$Paisaje <- with(LeafLitt2, Paisaje[match(T6$Succession, Succession)]) 

colnames(T6) <- c("Succession", "Leaf","seLeaf","Branch","seBranch","Detri","seDetri","Flower","seFlower","Fruit","seFruit", "Total", "seTotal"
                  ,"Landscape")

T6 # Averages and SE 

##################################### Leaf Litter (g) x Habitat #################################### 

# Testing for normality using Shapiro-Wilk test.
shapiro.test(LeafLitt2$LeafLitter.g) # W = 0.94317, p-value = 1.09e-10
shapiro.test(LeafLitt2$LeafLitter.g) # W = 0.94317, p-value = 1.09e-10
shapiro.test(LeafLitt2$Branches.g) # W = 0.85268, p-value < 2.2e-16
shapiro.test(LeafLitt2$Detritus.g) # W = 0.85358, p-value < 2.2e-16
shapiro.test(LeafLitt2$Flowers.g) # W = 0.41979, p-value < 2.2e-16
shapiro.test(LeafLitt2$Fruits.g) # W = 0.60558, p-value < 2.2e-16
shapiro.test(LeafLitt2$Total.g) # W = 0.95589, p-value = 4.528e-09

# Plotting histograms of data distribution. 
hist(LeafLitt2$LeafLitter.g)
hist(LeafLitt2$Branches.g)
hist(LeafLitt2$Detritus.g)
hist(LeafLitt2$Flowers.g)
hist(LeafLitt2$Fruits.g)
hist(LeafLitt2$Total.g)
# All histograms are skewed left. 

# Next we chec for homoscedasticity and linearity. 
lm1 <- lm(LeafLitter.g ~ Succession, data = Lomerio)
plot(lm1, 1) # Residual vs Fitted looks fine. 
plot(lm1, 3) # Scale-Location looks fine too. 

lm2 <- lm(LeafLitter.g ~ Succession, data = Mountain,na.action=na.exclude)
plot(lm2, 1) # Residual vs Fitted looks fine. 
plot(lm2, 3) # Scale-Location looks fine too. 

lm3 <- lm(Branches.g ~ Succession, data = Lomerio)
plot(lm3, 1) # Residual vs Fitted looks fine. 
plot(lm3, 3) # Scale-Location shows increase of residuals with fitted values. 

lm4 <- lm(Branches.g ~ Succession, data = Mountain,na.action=na.exclude)
plot(lm4, 1) # Residual vs Fitted looks fine. 
plot(lm4, 3) # Scale-Location looks fine too. 

lm5 <- lm(Flowers.g ~ Succession, data = Lomerio)
plot(lm5, 1) # Residual vs Fitted looks fine. 
plot(lm5, 3) # Scale-Location Shows mild increase.

lm6 <- lm(Flowers.g ~ Succession, data = Mountain,na.action=na.exclude)
plot(lm6, 1) # Residual vs Fitted looks fine. 
plot(lm6, 3) # Scale-Location shows increase of residuals with fitted values. 

lm7 <- lm(Fruits.g ~ Succession, data = Lomerio)
plot(lm7, 1) # Residual vs Fitted looks fine. 
plot(lm7, 3) # Scale-Location looks fine too. 

lm8 <- lm(Fruits.g ~ Succession, data = Mountain,na.action=na.exclude)
plot(lm8, 1) # Residual vs Fitted looks fine. 
plot(lm8, 3) # Scale-Location looks fine too. 

lm9 <- lm(Detritus.g ~ Succession, data = Lomerio)
plot(lm9, 1) # Residual vs Fitted looks fine. 
plot(lm9, 3) # Scale-Location looks fine too. 

lm10 <- lm(Detritus.g ~ Succession, data = Mountain,na.action=na.exclude)
plot(lm10, 1) # Residual vs Fitted looks fine. 
plot(lm10, 3) # Scale-Location looks fine too. 

lm11 <- lm(Total.g ~ Succession, data = Lomerio)
plot(lm11, 1) # Residual vs Fitted looks fine. 
plot(lm11, 3) # Scale-Location looks fine too. 

lm12 <- lm(Total.g ~ Succession, data = Mountain,na.action=na.exclude)
plot(lm12, 1) # Residual vs Fitted looks fine. 
plot(lm12, 3) # Scale-Location looks fine too. 

# CONCLUSION: Largely fine, so just a problem of non-normality, mostly with a left-skew. 

# For left-skewed data—tail is on the left, negative skew, common transformations include 
# (i) square root, (ii) cube root, and (iii) log.

# First test: Square root transformation.
par(mfrow=c(3,2))
hist(sqrt(LeafLitt2$LeafLitter.g)) 
hist(sqrt(LeafLitt2$Branches.g))
hist(sqrt(LeafLitt2$Detritus.g))
hist(sqrt(LeafLitt2$Flowers.g))
hist(sqrt(LeafLitt2$Fruits.g))
hist(sqrt(LeafLitt2$Total.g))
# Square-root seems to work well for Leaf, Detritus, and Total. Branches sorta. 

# Second test: Log transformation.
par(mfrow=c(1,2))
hist(log(LeafLitt2$Flowers.g)) # Great
hist(log(LeafLitt2$Fruits.g)) # Ok
hist(log(LeafLitt2$Branches.g)) # Right skew.

# Log works well for flowers and fruit. Branches needs something else. 

par(mfrow=c(1,1))
hist(LeafLitt2$Branches.g^(1/3)) 
# Cube root best for Branches. 

par(mfrow=c(3,2))
hist(sqrt(LeafLitt2$LeafLitter.g)) 
hist(LeafLitt2$Branches.g^(1/3)) # Cube root best for Branches. 
hist(sqrt(LeafLitt2$Detritus.g))
hist(log(LeafLitt2$Flowers.g)) # Great, but has a 0. 
hist(log(LeafLitt2$Fruits.g)) # Ok, but has a 0. 
hist(sqrt(LeafLitt2$Total.g))

write.csv(LeafLitt2, "Hojarasca.csv")

Groups1 <- c("DL1 x DL2", "DL1 x RL","DL2 x RL") # For Conover Test

# Now what we'll do is compare the results from the normal model with the transformed.

# Lomerio 
lm1log = lm(sqrt(LeafLitter.g) ~ Succession, data = Lomerio)
lm3log = lm(Branches.g^(1/3) ~ Succession, data = Lomerio)
lm5log = lm(log(Flowers.g+0.05) ~ Succession, data = Lomerio)
lm7log = lm(log(Fruits.g) ~ Succession, data = Lomerio)
lm9log = lm(sqrt(Detritus.g) ~ Succession, data = Lomerio)
lm11log = lm(sqrt(Total.g) ~ Succession, data = Lomerio)

# Same results 
emmeans(lm1, specs = pairwise ~ Succession)$contrasts
emmeans(lm1log, specs = pairwise ~ Succession)$contrasts
anova(lm1log)

# Same results 
emmeans(lm3, specs = pairwise ~ Succession)$contrasts
emmeans(lm3log, specs = pairwise ~ Succession)$contrasts
anova(lm3log)

# Diff results 
emmeans(lm5, specs = pairwise ~ Succession)$contrasts
emmeans(lm5log, specs = pairwise ~ Succession)$contrasts
anova(lm5log)

# Diff results 
emmeans(lm7, specs = pairwise ~ Succession)$contrasts
emmeans(lm7log, specs = pairwise ~ Succession)$contrasts
anova(lm7log)

# Same results 
emmeans(lm9, specs = pairwise ~ Succession)$contrasts
emmeans(lm9log, specs = pairwise ~ Succession)$contrasts
anova(lm9log)

# Same results 
emmeans(lm11, specs = pairwise ~ Succession)$contrasts
emmeans(lm11log, specs = pairwise ~ Succession)$contrasts
anova(lm11log)

# CONCLUSION: Lomerio comparisons are different between models. We'll go with the transformations
# since those meet more model assumptions. 

# Mountain
lm2log = lm(sqrt(LeafLitter.g) ~ Succession, data = Mountain,na.action=na.exclude)
lm4log = lm(Branches.g^(1/3) ~ Succession, data = Mountain,na.action=na.exclude)
lm6log = lm(log(Flowers.g+0.05) ~ Succession, data = Mountain,na.action=na.exclude)
lm8log = lm(log(Fruits.g+0.05) ~ Succession, data = Mountain,na.action=na.exclude)
lm10log = lm(sqrt(Detritus.g) ~ Succession, data = Mountain,na.action=na.exclude)
lm12log = lm(sqrt(Total.g) ~ Succession, data = Mountain,na.action=na.exclude)

# Same results 
emmeans(lm2, specs = pairwise ~ Succession)$contrasts
emmeans(lm2log, specs = pairwise ~ Succession)$contrasts
anova(lm2log)

# Same results 
emmeans(lm4, specs = pairwise ~ Succession)$contrasts
emmeans(lm4log, specs = pairwise ~ Succession)$contrasts
anova(lm4log)

# Same, but with marginally sig. results 
emmeans(lm6, specs = pairwise ~ Succession)$contrasts
emmeans(lm6log, specs = pairwise ~ Succession)$contrasts
anova(lm6log)

# Same results 
emmeans(lm8, specs = pairwise ~ Succession)$contrasts
emmeans(lm8log, specs = pairwise ~ Succession)$contrasts
anova(lm8log)

# Same results 
emmeans(lm10, specs = pairwise ~ Succession)$contrasts
emmeans(lm10log, specs = pairwise ~ Succession)$contrasts
anova(lm10log)

# Same results 
emmeans(lm12, specs = pairwise ~ Succession)$contrasts
emmeans(lm12log, specs = pairwise ~ Succession)$contrasts
anova(lm12log)

# CONCLUSION: For Mountain, largely all the same outcome. 
# We don't want to have one group with transformations and one without, so we'll keep both on transformations.

##################################### PERCENT LITTER BY HABITAT #################################### 

DFX4 <- aggregate(cbind(LeafLitter.g, Branches.g, Flowers.g, Fruits.g, Detritus.g, Total.g) ~ Succession,
                  data = LeafLitt2, FUN = sum, na.action=na.exclude)

DFX4$LeafP <- DFX4[,2]/DFX4[,7]*100
DFX4$BranchP <- DFX4[,3]/DFX4[,7]*100
DFX4$FlowerP <- DFX4[,4]/DFX4[,7]*100
DFX4$FruitP <- DFX4[,5]/DFX4[,7]*100
DFX4$DetritusP <- DFX4[,6]/DFX4[,7]*100
DFX4 # This is the percent that each litter type takes from the total in lomerio.

DFX5 <- aggregate(cbind(LeafLitter.g, Branches.g, Flowers.g, Fruits.g, Detritus.g, Total.g) ~ Paisaje,
                  data = LeafLitt2, FUN = sum,na.action=na.exclude)

DFX5$LeafP <- DFX5[,2]/DFX5[,7]*100
DFX5$BranchP <- DFX5[,3]/DFX5[,7]*100
DFX5$FlowerP <- DFX5[,4]/DFX5[,7]*100
DFX5$FruitP <- DFX5[,5]/DFX5[,7]*100
DFX5$DetritusP <- DFX5[,6]/DFX5[,7]*100
DFX5 # This is the percent that each litter type takes from the total in mountain


##################################### FLOWER LITTER PEAKS #################################### 
# When do flower litter peaks occur?

# First for lomerio:
aggregate(Flowers.g ~ Sample, data = LomeRL, FUN = mean, na.rm = TRUE)  
aggregate(Flowers.g ~ Sample, data = LomeDL2, FUN = mean, na.rm = TRUE)  
aggregate(Flowers.g ~ Sample, data = LomeDL1, FUN = mean, na.rm = TRUE)  



ggplot(data=Lomerio, aes(x=Sample, y=Flowers.g )) + # Flowers
  geom_bar(stat="identity", aes(fill = Succession), position="dodge") + 
  xlab("Month") +
  scale_y_continuous(name = "Flower Litter (g)") + 
  scale_fill_manual(values=c("#82C27B", "#43833C", "#23431F")) + 
  guides(fill=guide_legend(title="Habitat")) +
  theme_bw(base_size = 15)

# Next for mountain: 
aggregate(Flowers.g ~ Sample, data = MounRL, FUN = mean, na.rm = TRUE, na.action=na.exclude)  
aggregate(Flowers.g ~ Sample, data = MounDM2, FUN = mean, na.rm = TRUE, na.action=na.exclude)  
aggregate(Flowers.g ~ Sample, data = MounDM1, FUN = mean, na.rm = TRUE, na.action=na.exclude)  

ggplot(data=Mountain, aes(x=Sample, y=Flowers.g )) + # Flowers
  geom_bar(stat="identity", aes(fill = Succession), position="dodge") + 
  xlab("Month") +
  scale_y_continuous(name = "Flower Litter (g)") + 
  scale_fill_manual(values=c("#FBBA84", "#CE803F", "#67360D")) + 
  guides(fill=guide_legend(title="Habitat")) +
  theme_bw(base_size = 15)

############################ REFERENCE COMPARISONS (RL VS RM) ############################ 
# Lomerio plots appear to have higher leaf litter and structural variable totals.
# Lets check. 
ReferencePlots <- droplevels(subset(LeafLitt2, LeafLitt2$Succession == c("RM", "RL")))
aggregate(cbind(LeafLitter.g, Branches.g, Flowers.g, Fruits.g, Detritus.g, Total.g) ~ Succession,
          data = ReferencePlots, FUN = mean, na.action=na.exclude)
# Look at those totals! Lomerio (RL) has higher leaf, detritus, and total leaf litter.
ReferenceStrucPlots <- droplevels(subset(Struc, Struc$Succession == c("RM", "RL")))
aggregate(cbind(TreesHA, BasalHA, Biomass, Richness, Simpson_1.D) ~ Landscape,
          data = ReferenceStrucPlots, FUN = mean, na.rm = TRUE)
# Sure enough. Higher tree density, basal area, biomass, richness, and a lil bit of diversity. 

############################ PLOT COMPARISONS OF RAINFALL ############################ 
# How do plots differ in rainfall? 
aggregate(cbind(RainNow) ~ Plot,
          data = Lomerio, FUN = mean, na.rm = TRUE)
# Dance around 300, a couple have 320.
aggregate(cbind(RainNow) ~ CodeZ,
          data = Mountain, FUN = mean, na.rm = TRUE)
# Dance around 260. A couple have 288.

anova(lm(data=LeafLitt2, RainNow ~ Paisaje))
# Paisaje     1  140235  140235  9.3291 0.002419 **
# Landscapes do differ. Lomerio receives more rain. 

summarySE(LeafLitt2, measurevar="RainNow", groupvars=c("Paisaje"), na.rm = TRUE)
#    Paisaje   N  RainNow       sd        se       ci
# 1  Lomerio 156 310.3263 133.9929 10.728020 21.19199
# 2 Mountain 216 270.9795 113.6891  7.735564 15.24725

##################################### FRUIT LITTER PEAKS #################################### 
# When do fruit litter peaks occur?

# First for lomerio:
aggregate(Fruits.g ~ Sample, data = LomeRL, FUN = mean, na.rm = TRUE)  
aggregate(Fruits.g ~ Sample, data = LomeDL2, FUN = mean, na.rm = TRUE)  
aggregate(Fruits.g ~ Sample, data = LomeDL1, FUN = mean, na.rm = TRUE)  

ggplot(data=Lomerio, aes(x=Sample, y=Fruits.g )) + # Flowers
  geom_bar(stat="identity", aes(fill = Succession), position="dodge") + 
  xlab("Month") +
  scale_y_continuous(name = "Fruit Litter (g)") + 
  scale_fill_manual(values=c("#82C27B", "#43833C", "#23431F")) + 
  guides(fill=guide_legend(title="Habitat")) +
  theme_bw(base_size = 15)

# Next for mountain: 
aggregate(Fruits.g ~ Sample, data = MounRL, FUN = mean, na.rm = TRUE,na.action=na.exclude)  
aggregate(Fruits.g ~ Sample, data = MounDM2, FUN = mean, na.rm = TRUE,na.action=na.exclude)  
aggregate(Fruits.g ~ Sample, data = MounDM1, FUN = mean, na.rm = TRUE,na.action=na.exclude)  

ggplot(data=Mountain, aes(x=Sample, y=Fruits.g )) + # Flowers
  geom_bar(stat="identity", aes(fill = Succession), position="dodge") + 
  xlab("Month") +
  scale_y_continuous(name = "Fruit Litter (g)") + 
  scale_fill_manual(values=c("#FBBA84", "#CE803F", "#67360D")) + 
  guides(fill=guide_legend(title="Habitat")) +
  theme_bw(base_size = 15)

##################################### LITTERFALL x LANDSCAPE #################################### 
# Does litterfall differ between lomerio and mountain? 

anova(lm(data = LeafLitt2, sqrt(LeafLitter.Mg.ha.month) ~ Paisaje,na.action=na.exclude))
# Paisaje     1  1.2917 1.29174  46.996 3.023e-11 ***
anova(lm(data = LeafLitt2, Branches.Mg.ha.month^1/3 ~ Paisaje,na.action=na.exclude))
# Paisaje     1 0.001178 0.00117752  7.5124 0.006427 **
anova(lm(data = LeafLitt2, log(Fruits.Mg.ha.month+0.05) ~ Paisaje,na.action=na.exclude))
# Paisaje     1  0.156 0.156237   1.699 0.1932
anova(lm(data = LeafLitt2, log(Flowers.Mg.ha.month+0.05) ~ Paisaje,na.action=na.exclude))
# Paisaje     1 0.0509 0.050926  4.4805 0.03496 *
anova(lm(data = LeafLitt2, sqrt(Detritus.Mg.ha.month) ~ Paisaje,na.action=na.exclude))
# Paisaje     1 0.0002 0.0001688  0.0129 0.9098
anova(lm(data = LeafLitt2, sqrt(Total.Mg.ha.month) ~ Paisaje,na.action=na.exclude))
# Paisaje     1  1.0476 1.04761  25.697 6.325e-07 ***

aggregate(LeafLitter.Mg.ha.month ~ Paisaje, data = LeafLitt2, FUN = sum, na.action=na.exclude)  
#   Paisaje LeafLitter.Mg.ha.month
# 1  Lomerio                 94.033
# 2 Mountain                 92.312
aggregate(Flowers.Mg.ha.month ~ Paisaje, data = LeafLitt2, FUN = sum, na.action=na.exclude)  
# Paisaje Flowers.Mg.ha.month
#1  Lomerio               0.295
#2 Mountain               0.712
aggregate(Fruits.Mg.ha.month ~ Paisaje, data = LeafLitt2, FUN = sum, na.action=na.exclude)  
#    Paisaje Fruits.Mg.ha.month
# 1  Lomerio              3.419
# 2 Mountain              3.859
aggregate(Total.g ~ Paisaje, data = LeafLitt2, FUN = sum, na.action=na.exclude)  
# 1  Lomerio 113253.5
# 2 Mountain 124029.5
aggregate(Total.Mg.ha.month ~ Paisaje, data = LeafLitt2, FUN = sum, na.action=na.exclude)  
#    Paisaje Total.Mg.ha.month
# 1  Lomerio           125.833
#2 Mountain           137.811

##################################### Leaf Litter x Rain #################################### 
# Lets make a stacked barplot to visualize how leaf litter is composed in each landscape.
LomerioStack1 <- Lomerio[ , -which(names(Lomerio) %in% c("Record","Paisaje", "Locality", "Successional.State", "Plot", 
                                                         "LeafLitter.Mg.ha.month", "LeafLitter.Mg.ha.month2",
                                                         "Branches.Mg.ha.month", "Detritus.Mg.ha.month",
                                                         "Flowers.Mg.ha.month", "Fruits.Mg.ha.month", "Total.g",
                                                         "Total.Mg.ha.month", "Precipitation","Temp.Ave","Temp.Max", "PlotSample",
                                                         "CodeX",  "CodeZ","CodeY", "RainNow",  "Rain1",  "Rain2",  "Rain3", "TempNow", 
                                                         "Temp1", "Temp2", "Temp3"))]

LomerioStack2 <- melt(LomerioStack1, id.vars=c("Sample", "Succession"))
colnames(LomerioStack2) <- c("Month", "Habitat", "LitterType", "Value")

LomerioStack3 <- as.data.frame(LomerioStack2 %>% 
                                 group_by(LitterType, Habitat) %>% 
                                 summarise(Value = mean(Value)))

MountainStack1 <- Mountain[ , -which(names(Mountain) %in% c("Record","Paisaje", "Locality", "Successional.State", "Plot", 
                                                            "LeafLitter.Mg.ha.month", "LeafLitter.Mg.ha.month2",
                                                            "Branches.Mg.ha.month", "Detritus.Mg.ha.month",
                                                            "Flowers.Mg.ha.month", "Fruits.Mg.ha.month", "Total.g",
                                                            "Total.Mg.ha.month", "Precipitation","Temp.Ave","Temp.Max", "PlotSample",
                                                            "CodeX",  "CodeZ","CodeY", "RainNow",  "Rain1",  "Rain2",  "Rain3", "TempNow", 
                                                            "Temp1", "Temp2", "Temp3"))]

MountainStack2 <- melt(MountainStack1, id.vars=c("Sample", "Succession"))
colnames(MountainStack2) <- c("Month", "Habitat", "LitterType", "Value")

MountainStack2.5 <- na.omit(MountainStack2)

MountainStack3 <- as.data.frame(MountainStack2.5 %>% 
                                  group_by(LitterType, Habitat) %>% 
                                  summarise(Value = mean(Value)))


# Stacked barplot: 
Stack1 <- ggplot(LomerioStack3, aes(fill=LitterType, y=Value, x=Habitat)) + 
  geom_bar(position="stack", stat="identity", color = "black", alpha = 0.7) + 
  ylab("Mean Total Litter (g)") + 
  ylim(0, 900) + 
  labs(fill = "Litter Type") + 
  scale_fill_brewer(palette = "Accent", labels=c("Leaf", "Branch", "Detritus", "Flower", "Fruit"),
                    guide = guide_legend(
                      direction = "horizontal",
                      title.position = "top")) + 
  theme_bw(base_size = 11) + theme(legend.title = element_text(color = "black")) + 
  theme(legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid')) + 
  theme(legend.position = "bottom") + theme(panel.border = element_rect(color = "#23431F", fill = "transparent", size = 2))+ 
  theme(legend.title.align=0.5) 

Stack2 <- ggplot(MountainStack3, aes(fill=LitterType, y=Value, x=Habitat)) + 
  geom_bar(position="stack", stat="identity", color = "black", alpha = 0.7) + 
  ylab("Mean Total Litter (g)") + 
  ylim(0, 900) + 
  labs(fill = "Litter Type") + 
  scale_fill_brewer(palette = "Accent", labels=c("Leaf", "Branch", "Detritus", "Flower", "Fruit"),
                    guide = guide_legend(
                      direction = "horizontal",
                      title.position = "top")) + 
  theme_bw(base_size = 11) + theme(legend.title = element_text(color = "black")) + 
  theme(legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid')) +
  theme(legend.position = "bottom") + theme(panel.border = element_rect(color = "#67360D", fill = "transparent", size = 2)) + 
  theme(legend.title.align=0.5) 
  
# Export the ggarrange and mix in PPT to make just one legend.
ggarrange(Stack1, Stack2+ rremove("ylab"), # 900 x 450
          align='h', labels = "auto", legend = "bottom", 
          common.legend = T, nrow = 1, ncol = 2)


LomerioStack3$Landscape <- "Hill"
MountainStack3$Landscape <- "Mountain"

Megastack <- rbind(LomerioStack3, MountainStack3)

levels(Megastack$Habitat) <- list(EH  = "DL1", IH = "DL2", RH = "RL",
                                  EM  = "DM1", IM = "DM2", RM = "RM")


Figure1 <- ggplot(Megastack, aes(fill=LitterType, y=Value, x=Habitat)) + 
  geom_bar(position="stack", stat="identity", color = "black", alpha = 0.7) + 
  ylab("Mean Total Litter (g)") + 
  ylim(0, 900) + 
  labs(fill = "") + 
  scale_fill_brewer(palette = "Accent", labels=c("Leaf", "Branch", "Detritus", "Flower", "Fruit"),
                    guide = guide_legend(
                      direction = "horizontal",
                      title.position = "left")) + 
  theme_bw(base_size = 11) + theme(legend.title = element_text(color = "black")) + 
  theme(legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid')) +
  theme(legend.position = "bottom")  + 
  theme(legend.title.align=0.5) +
  theme(legend.key.size = unit(0.4, 'cm')) +
  facet_wrap(~Landscape, scales = "free") 

png("~/Downloads/Figure 1 (Stacked Litter x Habitat).png", width = 127, height = 90, units = 'mm', res = 300)
Figure1
dev.off()

###################### LITTERFALL X PRECIPITATION X SOLAR IRRADIATION###################### 

# First, some cleanup details for the dataframes:
# This solar dataset has the hrs/day of sunlight.
Solar$Estacion <- as.factor(Solar$Estacion)
Solar$Municipio <- as.factor(Solar$Municipio)
SolarMelt <- melt(Solar,id.vars=c("Estacion", "Municipio"))
colnames(SolarMelt) <- c("Estacion", "Municipio", "Month", "Sunlight")

# This dataset has the solar irradiation in Florencia data.
Irad$Month <- as.factor(Irad$Month)

# Ok, now lets see what all the solar hours per day stations look like.
ggplot(SolarMelt, aes(x=Month, y=Sunlight, group = Estacion, color = Estacion)) +
  geom_line(size=1, alpha=0.9, linetype=1) +
  ylab("Sunlight (hrs/day)") + 
  scale_colour_manual(values = c("#00AFBB", "#E7B800", "#FC4E07","#FFDB6D", "#C4961A", "#F4EDCA", 
                                 "#D16103", "#C3D7A4", "#52854C", "#4E84C4", "#293352")) + 
  theme_bw(base_size = 15) 
# A lot of similarity! Lets take the average.
SolarMeltAve <- droplevels(subset(SolarMelt, SolarMelt$Estacion == "Average"))

# Now lets get the dataframes into the same scale.
Lome3 <- Lomerio %>% dplyr::select(Sample, LeafLitter.g, Branches.g, Detritus.g, Flowers.g, Fruits.g, Total.g, Precipitation) # Subset with select function
Moun3 <- Mountain %>% dplyr::select(Sample, LeafLitter.g, Branches.g, Detritus.g, Flowers.g, Fruits.g, Total.g, Precipitation)  # Subset with select function

Lome3Ave <- aggregate(cbind(LeafLitter.g, Branches.g, Flowers.g, Fruits.g, Detritus.g, Total.g, Precipitation) ~ Sample,
                      data = Lome3, FUN = mean, na.action=na.exclude)

Moun3Ave <- aggregate(cbind(LeafLitter.g, Branches.g, Flowers.g, Fruits.g, Detritus.g, Total.g, Precipitation) ~ Sample,
                      data = Moun3, FUN = mean, na.action=na.exclude)

colnames(Lome3Ave) <- c("Month", "LeafLitter.g", "Branches.g", "Detritus.g", 
                        "Flowers.g", "Fruits.g", "Total.g", "Precipitation")

colnames(Moun3Ave) <- c("Month", "LeafLitter.g", "Branches.g", "Detritus.g", 
                        "Flowers.g", "Fruits.g", "Total.g", "Precipitation")

AveLom <- merge(SolarMeltAve, Lome3Ave,  by="Month")
AveMon <- merge(SolarMeltAve, Moun3Ave, by="Month")

# Solar irradiation considerably easier to get to the same scale given lack of multiple measures.
AveLomX <- merge(AveLom, Irad,  by="Month")
AveMonX <- merge(AveMon, Irad,  by="Month")

# Lets visualize real quick.
ggplot(AveLom, aes(x=Precipitation, y=Sunlight)) +
  geom_point(size=1, alpha=0.9, linetype=1) +
  geom_smooth(method=lm, colour = "black") +
  ylab("Sunlight (hrs/day)") + 
  theme_bw(base_size = 15) 

ggplot(AveMon, aes(x=Precipitation, y=Sunlight)) +
  geom_point(size=1, alpha=0.9, linetype=1) +
  geom_smooth(method=lm, colour = "black") +
  ylab("Sunlight (hrs/day)") + 
  theme_bw(base_size = 15) 

# And what are the correlations? 
cor.test(AveLom$Precipitation, AveLom$Sunlight, method = "pearson")
cor.test(AveLom$Precipitation, AveLom$Sunlight, method = "spearman")
# t = -2.6435, df = 8, p-value = 0.02955, r -0.6828239 
# S = 265.3, p-value = 0.06225, rho = -0.6079055 
cor.test(AveMon$Precipitation, AveMon$Sunlight, method = "pearson")
cor.test(AveMon$Precipitation, AveMon$Sunlight, method = "spearman")
# t = -2.1782, df = 8, p-value = 0.06104, r = -0.6101406 
# S = 254.27, p-value = 0.1063, rho = -0.5410359 

cor.test(AveLomX$Precipitation, AveLomX$Irradiation, method = "pearson")
cor.test(AveLomX$Precipitation, AveLomX$Irradiation, method = "spearman")
# t = -2.5464, df = 10, p-value = 0.02904, r = -0.5804196 
# S = 452, p-value = 0.05209, rho = -0.5804196 

cor.test(AveMonX$Precipitation, AveMonX$Irradiation, method = "pearson")
cor.test(AveMonX$Precipitation, AveMonX$Irradiation, method = "spearman")
# t = -2.3339, df = 10, p-value = 0.04177, -0.5938297 
# S = 454, p-value = 0.04884, rho = -0.5874126 

# Excellent. Now, I need this to be separaed by habitat so I can illustrate it.
Lome4 <- Lomerio %>% dplyr::select(Sample, LeafLitter.g, Branches.g, Detritus.g, Flowers.g, Fruits.g, Total.g, Precipitation, Succession)
Moun4 <- Mountain %>% dplyr::select(Sample, LeafLitter.g, Branches.g, Detritus.g, Flowers.g, Fruits.g, Total.g, Precipitation, Succession)

Lome4Ave <- aggregate(Lome4[ , 2:8], by = list(Lome4$Sample, Lome4$Succession), FUN = mean)
Moun4Ave <- aggregate(Moun4[ , 2:8], by = list(Moun4$Sample, Moun4$Succession), FUN = mean, na.rm = TRUE)

colnames(Lome4Ave) <- c("Month", "Habitat", "LeafLitter.g", "Branches.g", "Detritus.g", 
                        "Flowers.g", "Fruits.g", "Total.g", "Precipitation")

colnames(Moun4Ave) <- c("Month",  "Habitat", "LeafLitter.g", "Branches.g", "Detritus.g", 
                        "Flowers.g", "Fruits.g", "Total.g", "Precipitation")

summaryAveLom <- merge(SolarMeltAve, Lome4Ave, by="Month")
summaryAveMon <- merge(SolarMeltAve, Moun4Ave, by="Month")

summaryAveLomX <- merge(summaryAveLom, Irad, by="Month") # Merge with Irradiation
summaryAveMonX <- merge(summaryAveMon, Irad, by="Month") # Merge with Irradiation
write.csv(summaryAveMonX, "Data/Precip.csv")

# And now, do the habitats differ in their relationship between leaf litter and precipitation? 
anova(lm(data = summaryAveLom, sqrt(LeafLitter.g) ~ Precipitation * Habitat))
# Precipitation:Habitat  2   0.957   0.479  0.0516 0.9498265    
anova(lm(data = summaryAveMon, sqrt(LeafLitter.g) ~ Precipitation * Habitat))
# Precipitation:Habitat  2   1.996   0.998  0.1773 0.8386234    

levels(summaryAveLomX$Month) <- list("2017-08-01" = "Aug", "2017-09-01" =  "Sept", "2017-10-01" = "Oct", 
                                     "2017-11-01" = "Nov", "2017-12-01" = "Dec", "2018-01-01" = "Jan",
                                     "2018-02-01" = "Feb", "2018-03-01" = "Mar", "2018-04-01" = "Apr", 
                                     "2018-05-01" = "May", "2018-06-01" = "Jun","2018-07-01" = "Jul")

levels(summaryAveMonX$Month) <- list("2017-09-01" =  "Sept", "2017-10-01" = "Oct", 
                                     "2017-11-01" = "Nov", "2017-12-01" = "Dec", "2018-01-01" = "Jan",
                                     "2018-02-01" = "Feb", "2018-03-01" = "Mar", "2018-04-01" = "Apr", 
                                     "2018-05-01" = "May", "2018-06-01" = "Jun","2018-07-01" = "Jul",
                                     "2018-08-01" = "Aug")

# Convert to proper date format
summaryAveLomX$Month <- as.Date(summaryAveLomX$Month)
summaryAveLomX$Month <- month(summaryAveLomX$Month, label = TRUE)
summary(summaryAveLomX$Month) # check
summaryAveMonX$Month <- as.Date(summaryAveMonX$Month)
summaryAveMonX$Month <- month(summaryAveMonX$Month, label = TRUE)
summary(summaryAveMonX$Month) # check

# Alright. So to get the triple axes we'll actually need to create two sets of figures and then mix
# them together in Powerpoint. 

levels(summaryAveLomX$Habitat) <- list(EH  = "DL1", IH = "DL2", RH = "RL")
levels(summaryAveMonX$Habitat) <- list(EM  = "DM1", IM = "DM2", RM = "RM")

GLX1 <- ggplot(data=summaryAveLomX, aes(x=Month, y=LeafLitter.g )) +
  geom_bar(stat="identity", aes(fill = Habitat), position="dodge", alpha = 0.8) + 
  xlab("") +
  geom_line(aes(Month, Irradiation*3, group = Habitat), color = "GoldenRod") +
  geom_point(aes(Month, Irradiation*3, group = Habitat), color = "Black", shape = "☼", size = 3) +
  geom_boxplot(aes(Month, Precipitation*2), color = "darkblue") + 
  scale_y_continuous(name = "Leaf Litter (g)", breaks = c(0,250,500,750,1000),
                     sec.axis = sec_axis(~.*0.5, name="Precipitation (mm)")) + 
  scale_fill_manual(values=c("#82C27B", "#43833C", "#23431F")) + 
  expand_limits(y = c(0, 1050)) +  
  guides(fill=guide_legend(title="Habitat")) + 
  theme_bw(base_size = 10) + theme(legend.title = element_text(color = "black"))

GMX1 <- ggplot(data=summaryAveMonX, aes(x=Month, y=LeafLitter.g )) +
  geom_bar(stat="identity", aes(fill = Habitat), position="dodge", alpha = 0.8) + 
  xlab("Month") +
  geom_line(aes(Month, Irradiation*3, group = Habitat), color = "GoldenRod") +
  geom_point(aes(Month, Irradiation*3, group = Habitat), color = "Black", shape = "☼", size = 3) +
  geom_boxplot(aes(Month, Precipitation*2), color = "darkblue") + 
  scale_y_continuous(name = "Leaf Litter (g)", breaks = c(0,250,500,750,1000),
                     sec.axis = sec_axis(~.*0.5, name="Precipitation (mm)")) + 
  scale_fill_manual(values=c("#FBBA84", "#CE803F", "#67360D")) + 
  expand_limits(y = c(0, 1050)) +   
  guides(fill=guide_legend(title="")) +
  theme(legend.key.size = unit(0.2, 'cm')) +
  theme_bw(base_size = 10) 

#ggarrange(GLX1+ rremove("xlab"), GMX1, #800 x 600
 #         align='h', labels = "auto", legend = "bottom",
  #        common.legend = F, nrow = 2, ncol = 1) 


combined1 <- GLX1 + GMX1 & theme(legend.position = "bottom")
Figure2P1 <-combined1 + plot_layout(guides = "collect", nrow = 2)

png("~/Downloads/Figure 2 (Litter x Rain x Sun).png", width = 127, height = 120, units = 'mm', res = 300)
Figure2P1
dev.off()

GLX2 <- ggplot(data=summaryAveLomX, aes(x=Month, y=LeafLitter.g )) +
  geom_bar(stat="identity", aes(fill = Habitat), position="dodge", alpha = 0.8) + 
  xlab("") +
  geom_line(aes(Month, Irradiation*3, group = Habitat), color = "GoldenRod") +
  geom_point(aes(Month, Irradiation*3, group = Habitat), color = "Black", shape = "☼", size = 3) +
  geom_boxplot(aes(Month, Precipitation*2), color = "darkblue") + 
  scale_y_continuous(name = "Leaf Litter (g)", 
                     sec.axis = sec_axis(~.*0.33, name= expression(Irradiation~(W~m~""^{2})))) + 
  scale_fill_manual(values=c("#82C27B", "#43833C", "#23431F")) + 
  expand_limits(y = c(0, 1050)) +   
  guides(fill=guide_legend(title="Habitat")) + 
  theme_bw(base_size = 10) + theme(legend.title = element_text(color = "black"))


GMX2 <- ggplot(data=summaryAveMonX, aes(x=Month, y=LeafLitter.g )) +
  geom_bar(stat="identity", aes(fill = Habitat), position="dodge", alpha = 0.8) + 
  xlab("Month") +
  geom_line(aes(Month, Irradiation*3, group = Habitat), color = "GoldenRod") +
  geom_point(aes(Month, Irradiation*3, group = Habitat), color = "Black", shape = "☼", size = 3) +
  geom_boxplot(aes(Month, Precipitation*2), color = "darkblue") + 
  scale_y_continuous(name = "Leaf Litter (g)",
                     sec.axis = sec_axis(~.*0.33, name= expression(Irradiation~(W~m~""^{2})))) + 
  scale_fill_manual(values=c("#FBBA84", "#CE803F", "#67360D")) + 
  expand_limits(y = c(0, 1050)) +   
  guides(fill=guide_legend(title="")) +
  theme_bw(base_size = 10) + theme(legend.title = element_text(color = "black"))

#ggarrange(GLX2+ rremove("xlab"), GMX2, #800 x 600
 #         align='h', labels = "auto",
  #        common.legend = F, nrow = 2, ncol = 1) 

combined2 <- GLX2 + GMX2 & theme(legend.position = "bottom")
Figure2P2 <- combined2 + plot_layout(guides = "collect", nrow = 2) + plot_annotation(tag_levels = c('a'))

png("~/Downloads/Figure 2 (Litter x Rain x Sun)B.png", width = 127, height = 120, units = 'mm', res = 300)
Figure2P2
dev.off()

########################### LITTERFALL X HABITAT X PRECIPITATION ########################### 

# Here we're going to include interactions between precipitation and month as well as habitat and month. 
anova(lm(sqrt(LeafLitter.g) ~ Succession* RainNow + Sample + Succession * Sample, data = Lomerio))
anova(lm(Branches.g^(1/3) ~ Succession* RainNow + Sample + Succession * Sample, data = Lomerio))
anova(lm(log(Flowers.g+0.05) ~ Succession* RainNow + Sample + Succession * Sample, data = Lomerio))
anova(lm(log(Fruits.g+0.05) ~ Succession* RainNow + Sample + Succession * Sample, data = Lomerio))
anova(lm(sqrt(Detritus.g) ~ Succession* RainNow + Sample + Succession * Sample, data = Lomerio))
anova(lm(sqrt(Total.g) ~ Succession* RainNow + Sample + Succession * Sample, data = Lomerio))
# Detritus has marginally sig interaction Succession:RainNow

LomeDL1 <- droplevels(subset(Lomerio, Lomerio$Succession == "DL1"))
LomeDL2 <- droplevels(subset(Lomerio, Lomerio$Succession == "DL2"))
LomeRL <- droplevels(subset(Lomerio, Lomerio$Succession == "RL"))
anova(lm(sqrt(Detritus.g) ~ RainNow + Sample, data = LomeDL1)) # 0.4863
anova(lm(sqrt(Detritus.g) ~ RainNow + Sample, data = LomeDL2)) # 0.002182
anova(lm(sqrt(Detritus.g) ~ RainNow + Sample, data = LomeRL)) # 0.01891

cor.test(sqrt(LomeDL1$Detritus.g), LomeDL1$RainNow, method = "pearson")
# t = 0.68973, df = 34, p-value = 0.495, 0.1174687 
cor.test(sqrt(LomeDL2$Detritus.g), LomeDL2$RainNow, method = "pearson")
# t = -3.1439, df = 70, p-value = 0.002446, r = -0.3517521 
cor.test(sqrt(LomeRL$Detritus.g), LomeRL$RainNow, method = "pearson")
# t = -2.4396, df = 46, p-value = 0.01862, r = -0.338464 

# These did not show interaction by habitat.
cor.test(sqrt(Lomerio$LeafLitter.g), Lomerio$RainNow, method = "pearson")
# t = -2.0661, df = 154, p-value = 0.04049, r = -0.1642317 
cor.test(Lomerio$Branches.g^(1/3), Lomerio$RainNow, method = "pearson")
# t = -1.1568, df = 154, p-value = 0.2491, r = -0.09281767
cor.test(log(Lomerio$Flowers.g+0.05), Lomerio$RainNow, method = "pearson")
# t = -1.2327, df = 154, p-value = 0.2196, r = -0.09884558
cor.test(log(Lomerio$Fruits.g+0.05), Lomerio$RainNow, method = "pearson")
# t = -0.74763, df = 154, p-value = 0.4558, r = -0.06013693 
cor.test(sqrt(Lomerio$Total.g), Lomerio$RainNow, method = "pearson")
# t = -2.5641, df = 154, p-value = 0.0113, r = -0.2023459 


anova(lm(sqrt(LeafLitter.g) ~ Succession* RainNow + Sample + Succession * Sample, data = Mountain, na.action=na.exclude))
anova(lm(Branches.g^(1/3) ~ Succession* RainNow + Sample + Succession * Sample, data =  Mountain, na.action=na.exclude))
anova(lm(log(Flowers.g+0.05) ~ Succession* RainNow + Sample + Succession * Sample, data = Mountain, na.action=na.exclude))
anova(lm(log(Fruits.g+0.05) ~ Succession* RainNow + Sample + Succession * Sample, data = Mountain, na.action=na.exclude))
anova(lm(sqrt(Detritus.g) ~ Succession* RainNow + Sample + Succession * Sample, data = Mountain, na.action=na.exclude))
anova(lm(sqrt(Total.g) ~ Succession* RainNow + Sample + Succession * Sample, data = Mountain, na.action=na.exclude))

cor.test(sqrt(Mountain$LeafLitter.g), Mountain$RainNow,  method = "pearson")
# t = -5.7577, df = 211, p-value = 2.982e-08, -0.3684846 
cor.test(Mountain$Branches.g^(1/3), Mountain$RainNow, method = "pearson")
# t = -0.35219, df = 211, p-value = 0.725, r = -0.02423852 
cor.test(log(Mountain$Flowers.g+0.05), Mountain$RainNow, method = "pearson")
# t = 0.1038, df = 211, p-value = 0.9174, r = 0.007145484 
cor.test(log(Mountain$Fruits.g+0.05), Mountain$RainNow, method = "pearson")
# t = 0.92591, df = 211, p-value = 0.3555, r = 0.06361336 
cor.test(sqrt(Mountain$Detritus.g), Mountain$RainNow,  method = "pearson")
# t = -0.91204, df = 211, p-value = 0.3628,  r= -0.06266386 
cor.test(sqrt(Mountain$Total.g), Mountain$RainNow,  method = "pearson")
# t = -4.0824, df = 211, p-value = 6.325e-05, -0.270562 

##################################### Pearson Correlation Analysis #################################### 
# Do the litterfall traits correlate with the tree community variables? 
# To do this we first need to match the dataframes so that they're on the same scale.

DFX1 <- aggregate(cbind(LeafLitter.Mg.ha.month, Branches.Mg.ha.month, Flowers.Mg.ha.month, Fruits.Mg.ha.month, Detritus.Mg.ha.month, Total.Mg.ha.month) ~ CodeZ,
                  data = LeafLitt2, FUN = sum, na.action=na.exclude)
DFX2 <- aggregate(cbind(RainNow, Rain1, Rain2, Rain3) ~ CodeZ,
                  data = LeafLitt2, FUN = mean, na.action=na.exclude)
DFX3 <- merge(DFX1, DFX2, by="CodeZ")
colnames(DFX3) <- c("CodeZ", "Leaf","Branch","Flower","Fruit","Detritus","Total",
                    "RainNow","Rain1","Rain2","Rain3")

# Merge the dataframes.
Struc <- merge(Struc, DFX3, by="CodeZ") # After merging we split by landscape.
StrucLome <- droplevels(subset(Struc, Struc$Landscape == "Lomerio"))
StrucMon <- droplevels(subset(Struc, Struc$Landscape == "Mountain"))

# Now we split the landscapes by habitat.
StrucDL1 <- droplevels(subset(Struc, Struc$Succession == "DL1")) # Lomerio
StrucDL2 <- droplevels(subset(Struc, Struc$Succession == "DL2"))
StrucRL <- droplevels(subset(Struc, Struc$Succession == "RL"))
StrucDM1 <- droplevels(subset(Struc, Struc$Succession == "DM1")) # Mountain
StrucDM2 <- droplevels(subset(Struc, Struc$Succession == "DM2"))
StrucRM <- droplevels(subset(Struc, Struc$Succession == "RM"))

##################################### CORRELATION LEAF LITTER X ATTRIBUTES #################################### 
# Pooled Together
cor.test(Struc$Leaf, Struc$TreesHA, method="spearman")
# S = 4735, p-value = 0.8085, rho = 0.04537205
cor.test(Struc$Leaf, Struc$BasalHA, method="spearman")
# S = 3743.3, p-value = 0.1835, rho = 0.2453097
cor.test(Struc$Leaf, Struc$Biomass, method="spearman")
# S = 3717.9, p-value = 0.1742, rho = 0.2504285
cor.test(Struc$Leaf, Struc$Richness, method="spearman")
# S = 3614.5, p-value = 0.1399, rho = 0.2712687
cor.test(Struc$Leaf, Struc$Simpson_1.D, method="spearman")
# S = 3361.6, p-value = 0.07706, rho= 0.3222555

# ANCOVAs (Do the ladnscapes differ in their relationship to the structural variables?)
anova(lm(sqrt(Leaf) ~ TreesHA * Landscape, data = Struc)) # Sig. Interaction
# TreesHA:Landscape  1 0.58993 0.58993 11.9476  0.001826 ** 
anova(lm(sqrt(Leaf) ~ BasalHA * Landscape, data = Struc)) # Sig. Interaction
# BasalHA:Landscape  1 0.48562 0.48562  8.6063  0.006756 ** 
anova(lm(sqrt(Leaf) ~ Biomass * Landscape, data = Struc)) # Sig. Interaction
# Biomass:Landscape  1 0.42948 0.42948  7.3476   0.01153 *  
anova(lm(sqrt(Leaf) ~ Richness * Landscape, data = Struc)) # Sig. Interaction
# Richness:Landscape  1 0.36213 0.36213  5.9382   0.02169 *  
anova(lm(sqrt(Leaf) ~ Simpson_1.D * Landscape, data = Struc)) # No relationship at all. 
# Simpson_1.D:Landscape  1 0.02094 0.02094  0.2513 0.6201945    

# Separated by Habitat (Lomerio)
cor.test(StrucLome$Leaf, StrucLome$TreesHA, method="spearman")
# S = 62, p-value = 0.0007779, rho = 0.8296703
cor.test(StrucLome$Leaf, StrucLome$Biomass, method="spearman")
# S = 68, p-value = 0.001239, rho = 0.8131868
cor.test(StrucLome$Leaf, StrucLome$Richness, method="spearman")
# S = 59.582, p-value = 0.0003667, rho = 0.8363144
cor.test(StrucLome$Leaf, StrucLome$Simpson_1.D, method="spearman")
# S = 78, p-value = 0.002341, rho = 0.7857143
cor.test(StrucLome$Leaf, StrucLome$BasalHA, method="spearman")
# S = 62, p-value = 0.0007779, rho = 0.8296703

# Separated by Habitat (Mountain)
cor.test(StrucMon$Leaf, StrucMon$TreesHA, method="spearman")
# S = 993.01, p-value = 0.9222, rho = - 0.02478059
cor.test(StrucMon$Leaf, StrucMon$Biomass, method="spearman")
# S = 973, p-value = 0.987, rho = -0.004130099
cor.test(StrucMon$Leaf, StrucMon$Richness, method="spearman")
# S = 907.84, p-value = 0.8035, rho = 0.06311454
cor.test(StrucMon$Leaf, StrucMon$Simpson_1.D, method="spearman")
# S = 950.8, p-value = 0.941, rho = 0.01878047
cor.test(StrucMon$Leaf, StrucMon$BasalHA, method="spearman")
# S = 989.06, p-value = 0.935, rho = -0.02070403 

##################################### CORRELATION DETRITUS LITTER X ATTRIBUTES #################################### 
# ANCOVAs (Do the ladnscapes differ in their relationship to the structural variables?)
anova(lm(sqrt(Detritus) ~ TreesHA * Landscape, data = Struc)) # Marg Sig. Interaction
#TreesHA:Landscape  1 0.17159 0.171589  3.6197 0.06782 .
anova(lm(sqrt(Detritus) ~ BasalHA * Landscape, data = Struc)) # Marg Sig. Interaction
#BasalHA:Landscape  1 0.14461 0.144614  2.9198 0.09897 .
anova(lm(sqrt(Detritus) ~ Biomass * Landscape, data = Struc)) # No relationship at all.
#Biomass:Landscape  1 0.12755 0.127550  2.5337 0.12308  
anova(lm(sqrt(Detritus) ~ Richness * Landscape, data = Struc)) # No relationship at all.
#Richness:Landscape  1 0.12128 0.121282  2.3621 0.1360
anova(lm(sqrt(Detritus) ~ Simpson_1.D * Landscape, data = Struc)) # No relationship at all. 
#Simpson_1.D:Landscape  1 0.01661 0.016609  0.3009 0.5878

cor.test(StrucLome$Detritus, StrucLome$TreesHA, method="spearman")
# S = 82, p-value = 0.002923, rho = 0.7747253 
cor.test(StrucMon$Detritus, StrucMon$TreesHA, method="spearman")
# S = 1053, p-value = 0.7322, -0.08673207 

cor.test(StrucLome$Detritus, StrucLome$BasalHA, method="spearman")
# S = 76, p-value = 0.002082, rho = 0.7912088 
cor.test(StrucMon$Detritus, StrucMon$BasalHA, method="spearman")
# S = 884.74, p-value = 0.7315, rho = 0.08695694 

cor.test(Struc$Detritus, Struc$Biomass, method="spearman") 
# S = 3034.8, p-value = 0.03095, rho =0.388144 

cor.test(Struc$Detritus, Struc$Richness, method="pearson") 
# t = 1.2937, df = 29, p-value = 0.206, rho = 0.2335835 

cor.test(Struc$Detritus, Struc$Simpson_1.D, method="spearman") 
# S = 3416.7, p-value = 0.08842, rho =0.3111432


##################################### CORRELATION BRANCH LITTER X ATTRIBUTES #################################### 
# ANCOVAs (Do the ladnscapes differ in their relationship to the structural variables?)
anova(lm(Branch^(1/3) ~ TreesHA * Landscape, data = Struc)) # Marg. Sig. Interaction
#TreesHA:Landscape  1 0.03880 0.038803  3.1975 0.084979 . 
anova(lm(Branch^(1/3) ~ BasalHA * Landscape, data = Struc)) # # No relationship at all.
#BasalHA:Landscape  1 0.03369 0.033695  2.5967 0.11872  
anova(lm(Branch^(1/3) ~ Biomass * Landscape, data = Struc)) # # No relationship at all.
#Biomass:Landscape  1 0.02876 0.028761  2.1670 0.15256  
anova(lm(Branch^(1/3) ~ Richness * Landscape, data = Struc)) # # No relationship at all.
#Richness:Landscape  1 0.03326 0.033262  2.4988 0.12558  
anova(lm(Branch^(1/3) ~ Simpson_1.D * Landscape, data = Struc)) # No relationship at all. 
#Simpson_1.D:Landscape  1 0.00881 0.0088123  0.5894 0.4493

cor.test(Struc$Branch, Struc$TreesHA, method="spearman") # But marg interactionn
# S = 2906.6, p-value = 0.0206, rho = 0.4139948 
cor.test(StrucLome$Branch, StrucLome$TreesHA, method="spearman")
# S = 72, p-value = 0.001624, rho = 0.8021978 
cor.test(StrucMon$Branch, StrucMon$TreesHA, method="spearman") 
# S = 860.94, p-value = 0.6596, rho = 0.1115127 

cor.test(Struc$Branch, Struc$BasalHA, method="spearman") 
# S = 2980.8, p-value = 0.02617, rho = 0.3990317
cor.test(Struc$Branch, Struc$Biomass, method="spearman") 
# S = 3024.8, p-value = 0.03002, rho =0.3901603 
cor.test(Struc$Branch, Struc$Richness, method="spearman") 
# S = 3359.2, p-value = 0.07659, rho = 0.3227371 
cor.test(Struc$Branch, Struc$Simpson_1.D, method="spearman") 
# S = 3926.8, p-value = 0.2608, rho = 0.2083043 

##################################### CORRELATION FRUIT LITTER X ATTRIBUTES #################################### 
# ANCOVAs (Do the ladnscapes differ in their relationship to the structural variables?)
anova(lm(log(Fruit+0.05) ~ TreesHA * Landscape, data = Struc)) # No relationship at all.
#TreesHA:Landscape  1 0.0245 0.02446  0.0826 0.7760
anova(lm(log(Fruit+0.05)~ BasalHA * Landscape, data = Struc)) # # No relationship at all.
#BasalHA:Landscape  1 0.0379 0.03794  0.1294 0.7219
anova(lm(log(Fruit+0.05) ~ Biomass * Landscape, data = Struc)) # # No relationship at all.
#Biomass:Landscape  1 0.0176 0.01759  0.0599 0.8085
anova(lm(log(Fruit+0.05) ~ Richness * Landscape, data = Struc)) # # No relationship at all.
#Richness:Landscape  1 0.0361 0.03607  0.1219 0.7296
anova(lm(log(Fruit+0.05) ~ Simpson_1.D * Landscape, data = Struc)) # No relationship at all. 
#Simpson_1.D:Landscape  1 0.1433 0.14329  0.5598 0.46083  

cor.test(Struc$Fruit, Struc$TreesHA, method="spearman") 
# S = 5340.1, p-value = 0.682, rho = -0.07663608 
cor.test(Struc$Fruit, Struc$BasalHA, method="spearman") 
# S = 4586.7, p-value = 0.6874, rho = 0.07525473 
cor.test(Struc$Fruit, Struc$Biomass, method="spearman") 
# S = 4494.9, p-value = 0.6159, rho = 0.09376891 
cor.test(Struc$Fruit, Struc$Richness, method="spearman") 
# S = 4607.6, p-value = 0.7041, rho =0.07105373 
cor.test(Struc$Fruit, Struc$Simpson_1.D, method="spearman") 
# S = 4586.2, p-value = 0.687, rho = 0.07536892 

##################################### CORRELATION FLOWER LITTER X ATTRIBUTES #################################### 
# ANCOVAs (Do the ladnscapes differ in their relationship to the structural variables?)
anova(lm(log(Flower+0.05) ~ TreesHA * Landscape, data = Struc)) # Marg. Sig. Interaction
#TreesHA:Landscape  1 0.31247 0.31247  3.0768 0.09076 .
anova(lm(log(Flower+0.05)~ BasalHA * Landscape, data = Struc)) # Marg. Sig. Interaction
#BBasalHA:Landscape  1 0.30206 0.302063  3.0275 0.09325 .
anova(lm(log(Flower+0.05) ~ Biomass * Landscape, data = Struc)) # Marg. Sig. Interaction
#Biomass:Landscape  1 0.29167 0.29167  2.9111 0.09945 .
anova(lm(log(Flower+0.05) ~ Richness * Landscape, data = Struc)) # No relationship at all.
#Richness:Landscape  1 0.18437 0.18437  2.0191 0.16678  
anova(lm(log(Flower+0.05) ~ Simpson_1.D * Landscape, data = Struc)) # No relationship at all. 
#Simpson_1.D:Landscape  1 0.00156 0.00156  0.0151 0.90318  

cor.test(StrucLome$Flower, StrucLome$TreesHA, method="spearman") 
# S = 81.222, p-value = 0.001784, rho = 0.7768625
cor.test(StrucMon$Flower, StrucMon$TreesHA, method="spearman") 
# S = 989.04, p-value = 0.9351, rho = -0.02068253 

cor.test(StrucLome$Flower, StrucLome$BasalHA, method="spearman") 
# S = 81.222, p-value = 0.001784, rho = 0.7768625
cor.test(StrucMon$Flower, StrucMon$BasalHA, method="spearman") 
# S = 964.48, p-value = 0.9853, rho = 0.004665636

cor.test(StrucLome$Flower, StrucLome$Biomass, method="spearman") 
# S = 73.2, p-value = 0.001054, rho = 0.7989011 
cor.test(StrucMon$Flower, StrucMon$Biomass, method="spearman") 
# S = 980.02, p-value = 0.9643, rho = -0.01137539

cor.test(Struc$Flower, Struc$Richness, method="spearman") 
# S = 2579.7, p-value = 0.006294, rho = 0.479899

cor.test(Struc$Flower, Struc$Simpson_1.D, method="spearman") 
# S = 2592.8, p-value = 0.006631, rho = 0.4772501 

aggregate(data = LeafLitt2, LeafLitter.g ~ Paisaje, FUN = mean, na.action=na.exclude)

aggregate(data = LeafLitt2, Flowers.g ~ Paisaje, FUN = mean, na.action=na.exclude)

DFX2 <- aggregate(cbind(RainNow, Rain1, Rain2, Rain3) ~ CodeZ,
                  data = LeafLitt2, FUN = mean, na.rm = TRUE)

##################################### CORRELATION TOTAL LITTER X ATTRIBUTES #################################### 
# ANCOVAs (Do the ladnscapes differ in their relationship to the structural variables?)
anova(lm(sqrt(Total) ~ TreesHA * Landscape, data = Struc)) #  Sig. Interaction
# TreesHA:Landscape  1 0.81612 0.81612  8.8852 0.0060222 ** 
anova(lm(sqrt(Total) ~ BasalHA * Landscape, data = Struc)) #  Sig. Interaction
# BasalHA:Landscape  1 0.68533 0.68533  6.7440 0.015040 * 
anova(lm(sqrt(Total) ~ Biomass * Landscape, data = Struc)) # Sig relationship at all.
# Biomass:Landscape  1 0.60231 0.60231  5.7439 0.023737 * 
anova(lm(sqrt(Total) ~ Richness * Landscape, data = Struc)) # Sig relationship at all.
# Richness:Landscape  1 0.53742 0.53742  4.9542 0.034569 * 
anova(lm(sqrt(Total) ~ Simpson_1.D * Landscape, data = Struc)) # No relationship at all. 
# Simpson_1.D:Landscape  1 0.0445 0.04455  0.3262 0.57263  

cor.test(StrucLome$Total, StrucLome$TreesHA, method="spearman")
# S = 56, p-value = 0.0004354, rho = 0.8461538 
cor.test(StrucMon$Total, StrucMon$TreesHA, method="spearman")
# S = 1120.1, p-value = 0.5367, rho = -0.1559112 

cor.test(StrucLome$Total, StrucLome$BasalHA, method="spearman")
# S = 50, p-value = 0.0001888, rho = 0.8626374 
cor.test(StrucMon$Total, StrucMon$BasalHA, method="spearman")
# S = 1020.2, p-value = 0.8352, rho = -0.05279529 

cor.test(StrucLome$Total, StrucLome$Biomass, method="spearman")
# S = 58, p-value = 0.0005378, rho = 0.8406593 
cor.test(StrucMon$Total, StrucMon$Biomass, method="spearman")
# S = 972, p-value = 0.9903, rho = -0.003097574 

cor.test(StrucLome$Total, StrucLome$Richness, method="spearman")
# S = 52.572, p-value = 0.0001915, rho = 0.8555716 
cor.test(StrucMon$Total, StrucMon$Richness, method="spearman")
# S = 1041.2, p-value = 0.7689, rho = -0.07449585 

cor.test(Struc$Total, Struc$Simpson_1.D, method="spearman")
# S = 3391.7, p-value = 0.08311, rho = 0.3161943 

##################################### PRINCIPAL COMPONENT ANALYSIS #################################### 
legend_title <- "Habitat" # Name an object whatever you want your legend title to be. 

StrucLome3 <- StrucLome[5:19]
StrucLome3 <- StrucLome3[ , -which(names(StrucLome3) %in% c("Shannon_H","Chao.1", "Family", "Biomass", "Total.Mg.ha.month", "Total"))]

PCAStrucL <- StrucLome
levels(PCAStrucL$Succession) <- list(EH  = "DL1", IH = "DL2", RH = "RL")

pca_res1 <- prcomp(StrucLome3, scale. = TRUE)
PCA1 <- autoplot(pca_res1, data = PCAStrucL, colour = "Succession", 
                 loadings = TRUE, loadings.colour = c('#AA54B4', '#AA54B4', '#AA54B4',
                                                      '#AA54B4', 
                                                      '#B0B315','#B0B315', '#B0B315',
                                                      '#B0B315', '#B0B315', '#3E99D9'),
                 loadings.label = TRUE, loadings.label.size = 3, frame = TRUE,
                 loadings.label.label = c('Tree Density', 'Basal Area', 'Species Density',
                                          'Diversity', 
                                          'Leaf Litter','Branch Litter', 'Flower Litter',
                                          'Fruit Litter', 'Detritus Litter', 'Precipitation'),
                 loadings.label.colour = c('#AA54B4', '#AA54B4', '#AA54B4',
                                           '#AA54B4', 
                                           '#B0B315','#B0B315', '#B0B315',
                                           '#B0B315', '#B0B315', '#3E99D9'),
                 loadings.label.repel=T) +
  guides(fill = "none") +
  scale_colour_manual(values=c("#82C27B", "#43833C", "#23431F"), legend_title) + 
  scale_fill_manual(values=c("#82C27B", "#43833C", "#23431F")) + 
  theme(legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid')) +
  theme_bw(base_size = 12) + theme(legend.title = element_text(color = "black")) +
  theme(legend.position = c(0.28, 0.83))

StrucMon3 <- StrucMon[5:19]
StrucMon3 <- StrucMon3[ , -which(names(StrucMon3) %in% c("Shannon_H","Chao.1", "Family", "Biomass", "Total.Mg.ha.month", "Total"))]

PCAStrucM <- StrucMon
levels(PCAStrucM$Succession) <- list(EM  = "DM1", IM = "DM2", RM = "RM")

pca_res <- prcomp(StrucMon3, scale. = TRUE)
PCA2 <- autoplot(pca_res, data = PCAStrucM, colour = "Succession", 
                 loadings = TRUE, loadings.colour = c('#AA54B4', '#AA54B4', '#AA54B4',
                                                      '#AA54B4', 
                                                      '#B0B315','#B0B315', '#B0B315',
                                                      '#B0B315', '#B0B315', '#3E99D9'),
                 loadings.label = TRUE, loadings.label.size = 3, frame = TRUE, 
                 loadings.label.label = c('Tree Density', 'Basal Area', 'Species Density',
                                          'Diversity', 
                                          'Leaf Litter','Branch Litter', 'Flower Litter',
                                          'Fruit Litter', 'Detritus Litter', 'Precipitation'),
                 loadings.label.colour = c('#AA54B4', '#AA54B4', '#AA54B4',
                                           '#AA54B4', 
                                           '#B0B315','#B0B315', '#B0B315',
                                           '#B0B315', '#B0B315','#3E99D9'),
                 loadings.label.repel=T) +
  guides(fill = "none") +
  scale_colour_manual(values=c("#FBBA84", "#CE803F", "#67360D"), legend_title) + 
  scale_fill_manual(values=c("#FBBA84", "#CE803F", "#67360D")) + 
  theme_bw(base_size = 12) + guides(color=guide_legend(title="")) +
  theme(legend.position = c(0.28, 0.83))  

#ggarrange(PCA1, PCA1, # 600 x 800
         # align='h', labels = 'auto', legend = "bottom",
          #common.legend = F, nrow = 2, ncol = 1)

combined3 <- PCA1 + PCA2 & theme(legend.position = "right")
Figure3 <- combined3 + plot_layout(guides = "collect", nrow = 2) + plot_annotation(tag_levels = c('a'))

png("~/Downloads/Figure 3 (PCA).png", width = 129, height = 200, units = 'mm', res = 300)
Figure3
dev.off()

# Obtain Eigenvalues
eigen(cor(StrucLome3))
eigen(cor(StrucMon3))

##################################### TREE COMMUNITY VARIABLES X HABITAT  #################################### 
summary(lm(TreesHA ~ Succession, data = StrucLome))
summary(lm(BasalHA ~ Succession, data = StrucLome))
summary(lm(Simpson_1.D ~ Succession, data = StrucLome))
summary(lm(Richness ~ Succession, data = StrucLome))

anova(lm(TreesHA ~ Succession, data = StrucMon))
# Succession  2   11916    5958  0.0834 0.9204
anova(lm(BasalHA ~ Succession, data = StrucMon))
# Succession  2  124.78  62.389  0.5231 0.6031
anova(lm(Simpson_1.D ~ Succession, data = StrucMon))
# Succession  2 0.06041 0.030206   0.928 0.4169
anova(lm(Richness ~ Succession, data = StrucMon))
# Succession  2  1272.4  636.22  0.5752 0.5745

# CONCLUSION: Differences in Lomerio, not in Mountain! 

