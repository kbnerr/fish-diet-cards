#Trying a PCA for diet analysis
#Goal: see if there is overlap (competition between predator species)

library(tidyr)
library(devtools)
library(Rcpp)
library(ggfortify)
library(cluster)
library(vegan)

gs4_deauth() #This says that the sheet is public and it's chill to use it
link <- "https://docs.google.com/spreadsheets/d/10_uuMLOqu3s5fbRgF3x_Jt1VsufmJ6hLokaZ0aHvOYk/edit?usp=sharing"
diet_data <- read_sheet(link)
head(diet_data)

#Calculate abundance
diet_data_abund <- diet_data %>%
  subset(Contents == "Y") %>%
  group_by(Fish_ID, Species) %>%
  tally()
head(diet_data_abund)

#Format abundance data into wide
diet_data_wide <- diet_data_abund %>%
  pivot_wider(names_from = Species,
              values_from = n,
              values_fn = list(n = sum))
diet_data_wide[is.na(diet_data_wide)] <- 0
head(diet_data_wide) #note - may need to remove fish IDs that have 0 associated species

#Splice in Predator_spec row
predspec <- diet_data %>%
  select(Fish_ID, Predator_spec)
head(predspec)
widepred <- merge(diet_data_wide, predspec, by = "Fish_ID")
head(widepred)

#Create sp and env dataframes
sp_wide <- subset(widepred, select = -c(Fish_ID, Predator_spec) )
head(sp_wide)
env_wide <- subset(widepred, select = c(Fish_ID, Predator_spec))
head(env_wide)

#Making nmds
nmds <- metaMDS(sp_wide, distance = "bray", k = 2, autotransform = TRUE)
nmds #Stress = 0.11, below 0.2 so this is good!

#For color
par(mfrow = c(1, 1))
ordiplot(nmds, type = "n")
orditorp(nmds, display = "sites", labels = F, pch = c(16),
         col = c("black", "light blue", "red")
         [as.factor(env_wide$Predator_spec)], cex = 1)
ordiellipse(nmds, groups = env_wide$Predator_spec, lty = 1,
            col = c("black", "light blue", "red"))
legend("bottomleft", legend = c(levels(as.factor(env_wide$Color_Simple))), pch = c(16),
       col = c("black", "light blue", "red"),
       bty="n", cex=.8)
title(outer=F,adj=1,main="b. Lepas",cex=1.1,col="black",font=2,line=-1)


