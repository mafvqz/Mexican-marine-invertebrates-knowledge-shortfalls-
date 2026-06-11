###==========================================================================### 
###     Unveiling spatial and temporal gaps in the knowledge of marine       ###
###                      invertebrates in Mexico                             ###
### By:Marıa Fernanda Vazquez Flores, Angela P. Cuervo-Robayo, Nuno Simoes,  ###
###      Cristina Ronquillo, Joaquın Hortal, and Enrique Martınez-Meyer      ###
###==========================================================================###

### The following script contains the steps to create ignorance maps based on
### three dimensions of biodiversity knowledge (i.e., inventory completeness, 
### taxonomic completeness, and temporal coverage). Additionally, to assess the
### robustness of the ignorance index to weighting assumptions, we conducted a
### sensitivity analysis using three alternative weighting scenarios. 

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

# Load previously calculated values: Taxonomic and inventory completeness, 
# and age of records. In this case, we merged them into a single dataset ---------------------------------

ig_values <- fread('Ignorance_values.csv', sep=",", header = TRUE)

# All values were rescaled to range between 0 and 1 to ensure comparability. 
# Inventory and taxonomic completeness metrics were inverted (1—completeness) 
# so that higher values consistently reflected greater ignorance. Median record
# age was directly interpreted as an ignorance metric.


rescale01 <- function(x) {   #function to rescale
  (x - min(x, na.rm = TRUE)) /
    (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}


df_scaled <- ig_values %>%   #rescaled values
  mutate(
    Completeness_s = rescale01(Completeness),
    TaxC_s        = rescale01(TaxC),
    Age_s         = rescale01(age))

# ignorance values (close to 1 represent high ignorance and close to 0 low ignorance)

df_scaled <- df_scaled %>%    
  mutate(
    Ign_Comp = 1 - Completeness_s,
    Ign_TaxC = 1 - TaxC_s,
    Ign_Age  = Age_s)

# integrate these values into a single biodiversity ignorance index and rescaled to range between 0 and 1

df_scaled <- df_scaled %>%
  mutate(Ignorance_raw = Ign_Comp + Ign_TaxC + Ign_Age)


df_scaled <- df_scaled %>%
  mutate(Ignorance = rescale01(Ignorance_raw))


### Sensitivity analysis using three alternative weighting scenarios -----------

# rescale original values to range between 0 and 1 to ensure comparability and invert them

df_scaled_1 <- ig_values %>%
  mutate(
    Completeness_s = rescale01(Completeness),
    TaxC_s        = rescale01(TaxC),
    Age_s         = rescale01(age))


df_scaled_1 <- df_scaled_1 %>%
  mutate(
    Ign_Comp = 1 - Completeness_s,
    Ign_TaxC = 1 - TaxC_s,
    Ign_Age  = Age_s)

# Define the three alternative weighting scenarios, multiplicate the ignorance values
# to each weight and rescale the result to range between 0 and 1.

#| Scenario | Completeness | Age  | TaxC |
#| -------- | ------------ | ---- | ---- |
#| S1       | 0.7          | 0.15 | 0.15 |
#| S2       | 0.15         | 0.7  | 0.15 |
#| S3       | 0.15         | 0.15 | 0.7  |


df_scaled_1 <- df_scaled_1 %>%
  mutate(
    # Scenario 1: Completeness weighted 
    BII_S1 = (Ign_Comp * 0.7) +
      (Ign_Age * 0.15) +
      (Ign_TaxC * 0.15),
    
    # Scenario 2: Age weighted
    BII_S2 = (Ign_Comp * 0.15) +
      (Ign_Age * 0.7) +
      (Ign_TaxC * 0.15),
    
    # Scenario 3: TaxC weighted
    BII_S3 = (Ign_Comp * 0.15) +
      (Ign_Age * 0.15) +
      (Ign_TaxC * 0.7)) %>%
  mutate(
    Ignorance_S1 = rescale01(BII_S1),
    Ignorance_S2 = rescale01(BII_S2),
    Ignorance_S3 = rescale01(BII_S3))


# For plot purposes, merge all the ignorance indexes (equal weight and weighting scenarios) into a single dataset

df_scaled_1 <- df_scaled_1 %>%
  left_join(df_scaled %>% select(Area, Ignorance), by = "Area")


# map each ignorance index

countries <- rnaturalearth::ne_countries(scale = 50, returnclass = "sf") 

rm <- st_read("regionmarinamx.shp") # Shapefile of marine regions of Mexico (http://geoportal.conabio.gob.mx/metadatos/doc/html/regionmarinamx.html)

map_df <- merge(grid50, df_scaled_1, by.x ='id', by.y = 'Area') # Merge the ignorance index with the grid

map_df <- map_df %>% st_as_sf(map_df) #%>%  st_transform(4326)  # Transform it into an sf object for plot purposes

# Remove NA´s if necessary 

apply(X = is.na(map_df), MARGIN = 2, FUN = sum)

map_df <- drop_na(map_df, Ignorance)


# change "fill"  according to each ignorance index

ignorance_map <- (ggplot(map_df) +
         geom_sf(aes(fill = Ignorance), color = "transparent") +
         scale_fill_viridis_c(direction = -1,option = "G", limits = c(0,1), name = NULL) +
         geom_spatvector(data = rm, color = "darkcyan", fill = "transparent") +
         geom_spatvector(data = countries, fill ="transparent", color = 'black') +
         theme(panel.background = element_rect(fill = "white", colour = "black"),panel.border = element_rect(fill=NA),
               #legend.position = "none",
               legend.position = c(0.3, .05),
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
         coord_sf(xlim=c(-121,-86), ylim=c(10.5,32.5))+
         #annotation_scale(location = "br", width_hint = 0.1, height= unit(0.09, "cm"), text_cex = .3)+
         annotation_north_arrow(location = "tr",which_north = "false", 
         style = north_arrow_fancy_orienteering(text_size = 0), height = unit(.5, "cm"), width = unit(.5, "cm")) +
         NULL
         )

ggsave('IgnoranceMap.png', ignorance_map, dpi = 500,limitsize = FALSE, width = 13,
       height = 10, units = "cm")



### To facilitate interpretation of how these assumptions influence spatial prioritization patterns,
### grid cells were classified according to ignorance values as follows: 
### (1) high-priority areas (ignorance >0.5), 
### (2) moderate-priority areas (ignorance <= 0.5 and >0.2)
### (3) low-priority areas (ignorance <= 0.2)
                                                                                      

df_cat  <- df_scaled_1 %>%
  mutate(
    cat_ign = case_when(
      Ignorance <= 0.2 ~ "Low",
      Ignorance > 0.2 & Ignorance <= 0.5 ~ "Moderate",
      Ignorance > 0.5 ~ "High"
    ),
    cat_s1 = case_when(
      Ignorance_S1 <= 0.2 ~ "Low",
      Ignorance_S1 > 0.2 & Ignorance_S1 <= 0.5 ~ "Moderate",
      Ignorance_S1 > 0.5 ~ "High"
    ),
    cat_s2 = case_when(
      Ignorance_S2 <= 0.2 ~ "Low",
      Ignorance_S2 > 0.2 & Ignorance_S2 <= 0.5 ~ "Moderate",
      Ignorance_S2 > 0.5 ~ "High"
    ),
    cat_s3 = case_when(
      Ignorance_S3 <= 0.2 ~ "Low",
      Ignorance_S3 > 0.2 & Ignorance_S3 <= 0.5 ~ "Moderate",
      Ignorance_S3 > 0.5 ~ "High"
    )
  )



# function to count each categorized cell per scenario
conteo <- function(columna) {
  df_cat %>%
    count({{columna}}) %>%
    mutate(prop = n / sum(n))
}

conteo(cat_ign)
conteo(cat_s1)
conteo(cat_s2)
conteo(cat_s3)


# set the colors for each category and plot each map
colores <- c("High" = "#cc92c1",
             "Moderate" = "#c4c4c4",
             "Low" = "#5ac8c8")

grid_cat <- merge(grid50, df_cat, by.x ='id', by.y = 'Area') # Write here 'by.x' = the unique identifier name of your grid shapefile

grid_cat <- grid_cat %>% st_as_sf(grid_cat) #%>%  st_transform(4326)  # Transform to WGS84 projection

apply(X = is.na(grid_cat), MARGIN = 2, FUN = sum)

grid_cat <- drop_na(grid_cat, Ignorance)

# change "fill"  according to each ignorance index

ignorance_map_cat <- (ggplot(grid_cat) +
         geom_sf(aes(fill = cat_ign), color = "transparent") +
         scale_fill_manual(values = colores, name = "Priority") +       
         geom_spatvector(data = rm, color = "darkcyan", fill = "transparent") +
         geom_spatvector(data = countries, fill ="transparent", color = 'black') +
         theme(panel.background = element_rect(fill = "white", colour = "black"),panel.border = element_rect(fill=NA),
               #legend.position = "none",
               legend.position = c(0.3, .05),
               legend.justification = "bottom",
               legend.direction = "horizontal",
               legend.background = element_blank(),
               legend.title= element_text(family = "Arial Narrow", size = 6),
               legend.title.position = "top",
               legend.text = element_text(size = 4),
               #plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
               axis.text = element_blank(),
               axis.title.x = element_blank(),
               axis.title.y = element_blank(),
               axis.ticks = element_blank())+
         theme(text = element_text(family = "Arial Narrow"))+
         coord_sf(xlim=c(-121,-86), ylim=c(10.5,32.5))+
         annotation_scale(location = "br", width_hint = 0.1, height= unit(0.09, "cm"), text_cex = .3)+
         annotation_north_arrow(location = "tr",which_north = "false", 
                                style = north_arrow_fancy_orienteering(text_size = 0), height = unit(.5, "cm"), width = unit(.5, "cm")) +
         NULL
)

ggsave('IgnoranceMap_CAT.png', p0_1, dpi = 500,limitsize = FALSE, width = 13,
       height = 10, units = "cm")
