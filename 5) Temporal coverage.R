###==========================================================================### 
###     Unveiling spatial and temporal gaps in the knowledge of marine       ###
###                      invertebrates in Mexico                             ###
### By:Marıa Fernanda Vazquez Flores, Angela P. Cuervo-Robayo, Nuno Simoes,  ###
###      Cristina Ronquillo, Joaquın Hortal, and Enrique Martınez-Meyer      ###
###==========================================================================###

### The following script contains the steps to estimate per grid cell the age
### of the most recent record for every species and calculate the median age

# Load packages -----------------------------------------------------------------

library(data.table) # Data manipulation
library(tidyverse) # Data manipulation
library(terra) # GIS
library(dplyr) # Data manipulation
library(magrittr) # Data manipulation

getwd()
setwd("Directory path")  # Set your working directory path

# Load previously processed records (script 1) ---------------------------------

data <- fread('Echinodermata_data.csv', sep=",", header = TRUE)

# Select necessary fields 

sp_years <- data %>% dplyr::select(ScientificName_accepted, decimalLatitude, decimalLongitude, year) 

# Convert the records to vector in order to intersect them with the grid

sp_years <- vect(sp_years, geom = c("decimalLongitude", "decimalLatitude"), keepgeom = TRUE) 

# Load study area grid shapefile

grid <- terra::vect("grid.shp") 

# Intersect records with the grid

gridded_sp <- terra::intersect(sp_years,grid) 

gridded_sp <- as.data.frame(gridded_sp) %>%
              rename("CellID" = "id", "Accepted_Name" = "species") 


# Function to calculate the age of records

get_mostrecent_year <- function(x){
  x %>% pull(year) %>% .[which.max(.)] -> result
  return(result)
}

# Calculate the most recent year for each species per cell

gridded_sp %>% 
  dplyr::select(CellID,Accepted_Name,year) %>% 
  group_by(CellID,Accepted_Name) %>% 
  nest() -> test

test <- test %>% 
        ungroup() %>% 
        rowwise() %>% 
        mutate(MostRecent = (get_mostrecent_year(data)))

# Calculate the median age for each cell

test %>% 
    group_by(CellID) %>%
    summarise(age= 2024 - median(MostRecent,na.rm=T)) -> ages # Change the age according to your chosen year

# Merge the age data with the estimators from knowBR analyses (script 3) in 
# order to have all the metrics in the same dataframe

ages <- ages %>% rename("Area" = "CellID")

est <- est %>% left_join(ages) 

# Or merge the age data with the initial grid

grid_ages <- terra::merge(grid, ages, by = "CellID", all.x = TRUE)
