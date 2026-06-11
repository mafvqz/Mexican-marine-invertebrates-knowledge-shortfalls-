###==========================================================================### 
###     Unveiling spatial and temporal gaps in the knowledge of marine       ###
###                      invertebrates in Mexico                             ###
### By:Marıa Fernanda Vazquez Flores, Angela P. Cuervo-Robayo, Nuno Simoes,  ###
###      Cristina Ronquillo, Joaquın Hortal, and Enrique Martınez-Meyer      ###
###==========================================================================###

### The following script contains the steps to perform an inventory completeness
### analysis using the ‘KnowBR’ package (Lobo et al., 2018)

# Load packages -----------------------------------------------------------------

library(data.table)  # Data manipulation
library(tidyverse)  # Data manipulation
library(KnowBR) # completeness analysis
library(patchwork) # Plots composition
library(sf)  # GIS 
library(rnaturalearth)  # GIS 
library(rnaturalearthdata)# GIS 

getwd()
setwd("Directory path")  # Set your working directory path

# Load shapefiles --------------------------------------------------------------

data(adworld) # knowBR needs add world polygon to work 

# Shapefile of the world to use in map plot

world <- ne_countries(scale = "medium", returnclass = "sf") 

# Load study area grid shapefile

grid <- st_read("grid.shp") # Grid extent and cell size based on your data 

grid <- sf:::as_Spatial(grid) # load again as sf for plot purposes

raster::plot(grid)

# Load previously processed records (script 1)

data <- fread('Echinodermata_data.csv', sep=",", header = TRUE)

# Filter dataset to only records with accepted species name and select necessary fields 
# (ScientificName_accepted, decimalLongitude, decimalLatitude) for KnowBR

data <- dplyr::select(data, "ScientificName_accepted", "decimalLatitude",
                           "decimalLongitude") %>% rename("lat" = "decimalLatitude",
                                                          "lon" = "decimalLongitude",
                                                          "species" = "ScientificName_accepted")

# Add new field of abundance for knowBR

data$abundance <- 1

# Set an specific working directory to save the output from knowBR

if(!dir.exists("./outputs")){
  print("Creating outputs folder")
  dir.create("outputs")
}

setwd('outputs') 

# Apply KnowBR inventory completeness function 


KnowBPolygon(data = data, 
             shape = grid, admAreas = TRUE,  # Use predefined grid as personalized polygons  
             shapenames = "id",  # Write here your unique "id" cell from the grid
             jpg = TRUE, dec = ".", colcon = "transparent")

# Check the output 

# Load output of estimators from knowBR analyses

est <- read.csv('Estimators.csv', header = TRUE, sep = ",")

# Plot completeness results establishing threshold of well-survey cells

plot <- function(var, x, xtitle){
  ggplot(est) +
    geom_point(aes(var, Completeness), pch = 19, size = 1) +
    geom_vline(xintercept = x, col = 'red', lwd = 1, lty = 2) +
    geom_hline(yintercept = 80, col = 'grey', lwd = 1, lty = 2) + # Here completeness threshold =80%
    theme_minimal() + 
    ylab('') +
    xlab(xtitle) +
    theme(strip.text.y = element_blank(),
          axis.text = element_text(size = 10, face = "bold"),
          axis.title = element_text(size = 10, face = "bold"))
}

# Distribution of completeness values by other estimators:

# Plot records value vs completeness (threshold set in 10):

(p1 <- plot(est$Records, 10, 'Records') + ylab('Completeness'))

# Plot ratio value vs completeness (threshold set min 2):

(p2 <- plot(est$Ratio, 3, 'Ratio'))

# Plot slope value vs completeness (threshold set in 0.1):  

(p3 <- plot(est$Slope, 0.1, 'Slope'))

completn <- p1|p2|p3

ggsave('completenessDistribution.png', completn)

### Now filter est dataset based on your chosen thresholds selecting WELL-SURVEY CELLS only:

estWS <- est %>% filter(Ratio >= 3) %>% 
                 filter(Slope <= 0.1) %>% 
                 filter(Records >= 10) %>% 
                 filter(Completeness >= 80)


# Map of WELL-SURVEY CELLS 

comp_shp <- merge(grid, estWS, by.x ='id', by.y = 'Area') # Write here 'by.x' = the unique identifier name of your grid shapefile

comp_shp2 <- comp_shp %>% st_as_sf(comp_shp) #%>%  st_transform(4326)  # Transform to WGS84 projection

(map <- ggplot(comp_shp2) +
        geom_sf(data = world, fill ='transparent', color = 'black') +
        geom_sf(aes(fill = Completeness), color = "transparent") +
        scale_fill_viridis_c(limits = c(1, 100), option = "plasma", name = "", direction = -1,na.value="transparent") +
        theme_minimal() +
        theme(plot.background = element_rect(fill = "white", color = "transparent"),
              legend.position = "left", 
              plot.title = element_text(size = 16, face = "bold", hjust = 1.1)) +
        xlim(-120,-85)+
        ylim(10,35)  #limits of Mexico
  )

ggsave('WSCMap.png', map, dpi = 500)
