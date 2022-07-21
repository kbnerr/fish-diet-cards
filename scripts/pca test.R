#Trying a PCA for diet analysis
#Goal: see if there is overlap (competition between predator species)

library(tidyr)
library(devtools)
library(ggbiplot)
library(Rcpp)

install_github("vqv/ggbiplot")
library(ggbiplot)

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

#Making pca
pca <- prcomp(sp_wide)
ggbiplot(pca,
         ellipse = TRUE,
         groups = env_wide$Predator_spec,
         var.scale = 0.2,
         var.axes = FALSE) + #if you want species vectors present or not!
  theme_classic() +
  ggtitle("PCA for Lepas Presence")
