# Unveiling spatial and temporal gaps in the knowledge of marine invertebrates in Mexico

This repository contains all scripts used to assess taxonomic, spatial, and temporal knowledge gaps in marine invertebrates across the Mexican Exclusive Economic Zone (MEEZ). The analyses are based on occurrence records from GBIF and OBIS for four major taxonomic groups: Porifera, Cnidaria, Echinodermata, and Mollusca.

## Data sources

Occurrence records were obtained from:

- GBIF
- OBIS

## Workflow

### 1) Data cleaning

Quality control and standardization of occurrence records to generate the curated datasets used throughout the study.

### 2) Taxonomic coverage

Historical accumulation of documented species and records.

### 3) Inventory completeness

Estimation of inventory completeness across spatial grid cells and identification of well-surveyed areas.

### 4) Taxonomic completeness

Assessment of the proportion of records identified to species level within each grid cell.

### 5) Temporal coverage

Evaluation of record age and temporal knowledge gaps.

### 6) Ignorance maps

Integration of inventory completeness, taxonomic completeness and record age into a composite biodiversity ignorance index.

### 7) Figures

Generation of manuscript figures.

## Reproducing the analyses

Run scripts in the following order:

1. `01_Data_cleaning.R`
2. `02_Taxonomic_coverage.R`
3. `03_Inventory_completeness.R`
4. `04_Taxonomic_completeness.R`
5. `05_Temporal_coverage.R`
6. `06_Ignorance_maps.R`

This scripts is optional

`07_Figures.R` 
