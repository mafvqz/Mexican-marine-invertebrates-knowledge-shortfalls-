###==========================================================================### 
###     Unveiling spatial and temporal gaps in the knowledge of marine       ###
###                      invertebrates in Mexico                             ###
### By:Marıa Fernanda Vazquez Flores, Angela P. Cuervo-Robayo, Nuno Simoes,  ###
###      Cristina Ronquillo, Joaquın Hortal, and Enrique Martınez-Meyer      ###
###==========================================================================### 

### The following script contains steps for downloading and cleaning occurrence 
### records from GBIF and OBIS using as example records from the phylum Echinodermata.
### The cleaning process involves filtering and joining data to validate the records
### through a cleaning protocol addressing taxonomic, geographical, and temporal inconsistencies.


# Load packages -----------------------------------------------------------------

library(rgbif)  # Download data
library(robis)  # Download data
library(rio)  # Data manipulation 
library(data.table)  # Data manipulation
library(tidyverse)  # Data manipulation
library(magrittr)  # Data manipulation
library(lubridate)  # Dates standardization 
library(CoordinateCleaner)  # Data cleaning

getwd()
setwd("Directory path")  # Set your working directory path


# Getting occurrence records and initial filters -------------------------------

# Download data from GBIF

# Search the taxon name and ID in the GBIF backbone taxonomy to download the available records

name <- name_backbone(name='ECHINODERMATA', rank='phyllum')
name[, c('usageKey', 'scientificName')]
occ_count(taxonKey = 50, country = 'MX') # MX means only records from Mexico 

occ_download(pred("taxonKey", 50),      # Saves the raw dataset
             pred("hasGeospatialIssue", FALSE),
             pred("occurrenceStatus","PRESENT"), 
             pred("country","MX"),
             format = "SIMPLE_CSV")


gbif <- occ_download_get('0006439-250525065834625') %>%
        occ_download_import() # Write your download key

# Select columns of interest from the dataset (Darwin Core Terms)

colnames(gbif)

gbif <- dplyr::select(gbif, "gbifID", "genus", "species", "infraspecificEpithet", 
                           "taxonRank", "scientificName", "countryCode", "locality",
                           "stateProvince", "decimalLatitude",	"decimalLongitude",
                           "coordinatePrecision", "coordinateUncertaintyInMeters", 
                           "eventDate", "day", "month", "year", "basisOfRecord", "institutionCode", 
                           "collectionCode", "catalogNumber", "recordedBy") %>% rename("ID" = "gbifID")

gbif$date <- paste(gbif$year, gbif$month, gbif$day, sep = '/')

# Filter only records with available coordinates

gbif_nocoordinates <- gbif %>% filter(is.na(decimalLatitude)) # Dataset of records missing coordinates

gbif <- gbif %>% filter(decimalLatitude != "NA" & decimalLongitude != "NA")

# Discard fossils and filter records with at least species-level identification

gbif %>% count(basisOfRecord) 

gbif <- gbif %>% filter(basisOfRecord != "FOSSIL_SPECIMEN") 

gbif %>% count(taxonRank) 

gbif <- gbif %>% filter(taxonRank %in% c("SPECIES","SUBSPECIES", "VARIETY")) 

# Divide the scientificName column into scientificName and scientificNameAuthorship
# in order to unify the datasets format and merge them 

sp <- gbif %>% filter(taxonRank == "SPECIES")%>%
  separate(scientificName,into=c("G","S","extra"),extra = "merge",sep=" ") %>%
  unite("scientificName",c(G,S),sep=" ", remove=T) 

ssp <- gbif %>% filter(taxonRank == "SUBSPECIES")%>%
  separate(scientificName,into=c("G","S","SE","extra"),extra = "merge",sep=" ")%>%
  unite("scientificName",c(G,S,SE),sep=" ",remove=T) 

var <- gbif %>% filter(taxonRank == "VARIETY")%>%
    separate(scientificName,into=c("G","S","var","SE","extra"),extra = "merge",sep=" ")%>%
    unite("scientificName",c(G,S,var,SE),sep=" ",remove=T) 

gbif[ , "extra"] <- NA 

gbif <- rbind(sp, ssp, var) %>% rename("scientificNameAuthorship" = "extra")

# Download data from OBIS

# Write the taxon ID (WoRMS AphiaID) and the OBIS area ID to download the available records

obis <- occurrence(taxonid = 1806, areaid = 146) # 1806 = Echinodermata taxon ID, 146 = Mexico area ID

export(obis, format = "csv", file = "Obis_raw_data.csv") # Saves the raw dataset

# Select columns of interest from the dataset (Darwin Core Terms)

colnames(obis)

obis <- dplyr::select(obis, "id", "genus", "species", "infraspecificEpithet", 
                            "taxonRank", "scientificName", "scientificNameAuthorship", "countryCode", "locality",
                            "stateProvince", "decimalLatitude",	"decimalLongitude",
                            "coordinatePrecision", "coordinateUncertaintyInMeters", 
                             "eventDate","day","month", "year", "basisOfRecord", "institutionCode", 
                            "collectionCode", "catalogNumber", "recordedBy") %>% rename("ID" = "id")

obis$date <- paste(obis$year, obis$month, obis$day, sep = '/')

# Filter only records with available coordinates

obis <- obis %>% filter(decimalLatitude != "NA" & decimalLongitude != "NA")

# Discard fossils and filter records with at least species-level identification

obis %>% count(basisOfRecord) 

obis <- obis %>% filter(basisOfRecord != "FOSSIL_SPECIMEN") 

obis %>% count(taxonRank) 

obis <- obis %>% filter(taxonRank %in% c("Species","Subspecies","Variety","species",NA)) %>% 
                  filter(species != "NA")

# Merge GBIF and OBIS datasets to check the taxonomy, coordinates and dates of all species occurrence records

data <- rbind(gbif, obis) 


# Taxonomic validation ---------------------------------------------------------

# Create a checklist with unique scientific names and export it to check the
# current status of each name with the taxonomic validation tool of the World 
# Register of Marine Species. Then, compare the lists of scientific accepted 
# names with the Catalogue of Taxonomic Authorities of Flora and Fauna Species 
# Distributed in Mexico to retain only valid species reported from Mexico


length(unique(data$scientificName))
    
checklist <- as_tibble(unique(data$scientificName)) %>% rename("scientificName" = "value")

export(checklist, format = "csv", file = "scientificName_checklist.csv")

# Import the curated species checklist and filter records with accepted scientific names

curated_checklist <-import(file = "curated_checklist.csv", format = "csv", encoding = "UTF-8" ) %>% as_tibble()

curated_checklist %>% count(`Taxon status`) 

curated_checklist <- curated_checklist %>% filter(`Taxon status` == "accepted")

# Using the curated species checklist, harmonize the taxonomic classification of all the records
# and remove records with unaccepted scientific names

curated_checklist <- dplyr::select(curated_checklist,"scientificName","ScientificName_accepted","Authority_accepted","Class")

data <- data %>% left_join(curated_checklist)

data <- data[!is.na(data$ScientificName_accepted),] 


# Geographic validation --------------------------------------------------------

# Remove records with zero coordinates, identical latitude and longitude,
# and using ‘CoordinateCleaner’ (Zizka et al., 2019) discard records within a
# 100 m radius of biodiversity institutions (e.g., aquariums, museums, universities,
# and research centres) and geographical centroids

data <- data %>% filter(decimalLatitude!= 0 & decimalLongitude!= 0) %>% 
        filter(decimalLatitude!= decimalLongitude) 

data <- clean_coordinates(x = data,
                          lon = "decimalLongitude",
                          lat = "decimalLatitude",
                          tests = c("centroids","institutions")) # Select them according to your data


flagged_records <- data[!data$.summary,] # Dataset of invalid records according to ‘CoordinateCleaner’

data <- data[data$.summary,] # Records taxonomically and geographically valid


# Temporal validation ----------------------------------------------------------

# Remove records that lacked collection year data 

data_nodate <- filter(data, is.na(year)) # Dataset of records missing collection date

data <- data[!is.na(data$year),] # Records taxonomically, geographically and temporally valid


# Final filters and final dataset ----------------------------------------------

# Remove duplicate records, i.e. two or more records sharing the same accepted 
# scientific name, catalog number and collection date

nrow(data[duplicated(data$catalogNumber), ]) # Number of duplicated catalog numbers

length(unique(data$catalogNumber)) # Number of unique catalog numbers

data <- data %>% distinct(scientificName, catalogNumber, date,.keep_all = TRUE)

# Select columns of interest from the final dataset (Darwin Core Terms)

colnames(data)

data <- dplyr::select(data, "ID", "basisOfRecord", "taxonRank","Class",
                                    "species", "infraspecificEpithet", "scientificName",
                                    "ScientificName_accepted", "Authority_accepted",
                                    "countryCode","stateProvince","locality", 
                                    "decimalLatitude","decimalLongitude", "depth", "day",
                                    "month", "year", "date", "institutionCode", 
                                    "collectionCode", "catalogNumber", "recordedBy")

# Save the final dataset 

export(data, format = "csv", file = "Echinodermata_data.csv") 
