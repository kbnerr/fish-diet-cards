# -----------------------------------------------------------------------
# Title: Diet card map
# Creator: Chris Guo
# Date: 13 June 2022
# Purpose: Create map figures to print on the diet card handouts

# Notes -------------------------------------------------------------------


# Load packages -----------------------------------------------------------
library(rgdal)
library(sf)
library(tmap)
library(grid)
library(tidyverse)

# Define workflow paths ---------------------------------------------------

wd = getwd()
dir.output = file.path(wd, "output")
dir.figs = file.path(wd, "figs")
dir.data = file.path(wd,"data")
dir.R = file.path(wd,"R")

# Utility -----------------------------------------------------------------


# Read in data ------------------------------------------------------------

# The AK Coastline data held on external hard drive:
file = file.path("/Volumes/Seagate/spatial/shp/2022-adnr_ak-cst63xsi/Alaska_Coastline.shp")
ak.coast = readOGR(dsn = file)
proj4string(ak.coast) # Looks like we're in NAD83 datum

# Labels
locations = read_csv(file = file.path(dir.data, "map_locations.csv"))


# Bigger map --------------------------------------------------------------
big_bbox = st_bbox(c(xmin = -154.2774, xmax = -150.8606, # lon limits
                     ymin = 58.8766, ymax = 60.2755), # lat limits
                   crs = 4269) %>% # coordinate reference system
  st_as_sfc() # convert to sfc object

locations.coord = locations %>%
  filter(!Location_name %in% c('Flat Island', 'Pt. Pogibshi')) %>%
  st_as_sf(coords = c('Lon', "Lat"))
st_crs(locations.coord) = "+proj=longlat +datum=WGS84 +no_defs"

tm_shape(ak.coast, bbox = big_bbox) + 
  tm_fill(col = '#e5f5e0') +
  tm_borders(col = 'darkgrey') +
  tm_layout(bg.color = 'lightblue') +
  tm_shape(locations.coord) +
  tm_symbols(size = 0.25, col = "black") +
  tm_text("Location_name", size = 1, just = "left", xmod = 0.09, ymod = 0.1)
plot = grDevices::recordPlot()

# Save the plot using tiff() and dev.off()
tiff(filename = file.path(dir.output, "big_map.tiff"),
     res = 500, width = 140, height = 102.6667, units = "mm" , pointsize = 12)
plot
dev.off()

