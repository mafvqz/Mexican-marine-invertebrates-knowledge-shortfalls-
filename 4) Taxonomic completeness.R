###==========================================================================### 
###     Unveiling spatial and temporal gaps in the knowledge of marine       ###
###                      invertebrates in Mexico                             ###
### By:Marıa Fernanda Vazquez Flores, Angela P. Cuervo-Robayo, Nuno Simoes,  ###
###      Cristina Ronquillo, Joaquın Hortal, and Enrique Martınez-Meyer      ###
###==========================================================================###

### The following script contains the steps to calculate taxonomic completeness
### for each cell as an indicator of taxonomic resolution within the dataset. 
### This metric quantifies the proportion of occurrence records identified at
### species level relative to the total number of records, reflecting how 
### completely records from a given area are resolved to a precise taxonomic rank

# Load packages -----------------------------------------------------------------

library(data.table) # Data manipulation
library(tidyverse) # Data manipulation
library(dplyr) # Data manipulation
library(magrittr) # Data manipulation
library(sf) # GIS 
library(terra) # GIS
library(rnaturalearth)  # GIS 
library(rnaturalearthdata) # GIS 
library(ggspatial) # GIS
library(png) # Save images
library(tidyterra) # GIS
library(ggplot2) # Data visualization

getwd()
setwd("Directory path")  # Set your working directory path

# Load previously processed records (script 1) ---------------------------------

pts_data <- fread('Echinodermata_data.csv', encoding="UTF-8", header=TRUE)


# Merge all records (higher taxonomic levels + species level) with your grid cell

# Transform all records and grid shape into an sf object for plot purposes

pts_data <- st_as_sf(pts_data,
  coords = c("decimalLongitude", "decimalLatitude"),
  crs = 4326)  # CRS according to your data

grid50_sf <- st_as_sf(grid50)

pts_data <- st_join(pts_data, grid50_sf, join = st_within)

# count records per grid cell

conteo <- pts_data %>% 
  st_drop_geometry() %>% 
  count(id, name = "All_records")

# merge these values with your grid

grid_count <- grid50_sf %>% 
  left_join(conteo, by = "id") %>% 
     mutate(all_records = replace_na(All_records, 0))

# For plot purposes, merge these values with knowBr estimators 
est <- dplyr::select(grid_cont, "id", "All_records") %>% rename("Area" = "id") %>% 
              as.data.frame() %>% select("Area", "All_records")

est <- est %>% left_join(est, by = "Area") 

# Remove NA´s if necessary 

apply(X = is.na(est), MARGIN = 2, FUN = sum)

est <- drop_na(est, Records)

# Calculate taxonomic completeness index (species level records/higher taxonomic levels records)  

est <- est %>% mutate("TaxC" = Records/All_records)

# Map taxonomic completeness

countries <- rnaturalearth::ne_countries(scale = 50, returnclass = "sf") 

rm <- st_read("regionmarinamx.shp") # Shapefile of marine regions of Mexico (http://geoportal.conabio.gob.mx/metadatos/doc/html/regionmarinamx.html)

taxCmap <- merge(grid50_sf, est, by.x ='id', by.y = 'Area') # Merge TaxC with the grid, write here 'by.x' = the unique identifier name of your grid shapefile

taxCmap <- taxCmap %>% st_as_sf(taxCmap) #%>%  st_transform(4326)  # Transform to WGS84 projection

apply(X = is.na(taxCmapa), MARGIN = 2, FUN = sum) # Remove NA´s if necessary 


(TaxC <- ggplot(taxCmap) +
    geom_sf(aes(fill = TaxC), color = "transparent") +
    scale_fill_stepsn(n.breaks=10, colours = hcl.colors(10, "GnBu", rev = TRUE), limits=c(0,1),breaks = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1), # Change it according to your preferences
    na.value = "transparent", name = "Taxonomic completeness", guide = guide_colorsteps(barwidth = 10, barheight = .3, title.position = "bottom", title.hjust = 0.5))+
    geom_spatvector(data = rm, color = "darkcyan", fill = "transparent") +
    geom_spatvector(data = countries, fill ="transparent", color = 'black') +
    theme(panel.background = element_rect(fill = "white", colour = "black"),panel.border = element_rect(fill=NA),
          #legend.position = "none",
          legend.position = c(.5, .06),
          legend.justification = "bottom",
          legend.direction = "horizontal",
          legend.background = element_blank(),
          legend.title= element_text(family = "Arial Narrow", size = 6 ),
          legend.text = element_text(size = 4),
          #plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
          axis.text = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks = element_blank())+
    theme(text = element_text(family = "Arial Narrow"))+
    coord_sf(xlim=c(-125,-85), ylim=c(1.5,32.5))+
    annotation_scale(location = "br", width_hint = 0.1, height= unit(0.09, "cm"), text_cex = .3)+
    annotation_north_arrow(location = "tr",which_north = "false", 
    style = north_arrow_fancy_orienteering(text_size = 0), height = unit(.5, "cm"), width = unit(.5, "cm")) +
    NULL
)

ggsave('TaxCompleteness.png', TaxC, dpi = 500,limitsize = FALSE, width = 13,
       height = 10, units = "cm")


