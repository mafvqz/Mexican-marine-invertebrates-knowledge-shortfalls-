###==========================================================================### 
###     Unveiling spatial and temporal gaps in the knowledge of marine       ###
###                      invertebrates in Mexico                             ###
### By:Marıa Fernanda Vazquez Flores, Angela P. Cuervo-Robayo, Nuno Simoes,  ###
###      Cristina Ronquillo, Joaquın Hortal, and Enrique Martınez-Meyer      ###
###==========================================================================###

### The following script contains the steps to create the maps and histograms 
### using the estimators from knowBR analyses and the age data

# Load packages -----------------------------------------------------------------

library(data.table) # Data manipulation
library(tidyverse)  # Data manipulation
library(sf) # GIS 
library(terra) # GIS
library(rnaturalearth)  # GIS 
library(rnaturalearthdata) # GIS 
library(ggspatial) # GIS
library(RColorBrewer) # Data visualization
library(png) # Save images
library(tidyterra) # GIS
library(MetBrewer) # Data visualization
library(ggplot2) # Data visualization
library(colorRamps) # Data visualization

getwd()
setwd("D:/REGISTROS_INVERTEBRADOS/Codes")


# Load shapefiles to use in map plot --------------------------------------------

countries <- rnaturalearth::ne_countries(scale = 50, returnclass = "sf") 

rm <- st_read("regionmarinamx.shp") # Shapefile of marine regions of Mexico (http://geoportal.conabio.gob.mx/metadatos/doc/html/regionmarinamx.html)

maps <- merge(grid, est, by.x ='id', by.y = 'Area') # Merge the estimator and the age data with the grid

maps <- maps %>% st_as_sf(maps) #%>%  st_transform(4326)  # Transform it into an sf object for plot purposes

# Remove NA´s if necessary 

apply(X = is.na(maps), MARGIN = 2, FUN = sum)

maps <- drop_na(maps, Records)

histograms <- sf::st_intersection(maps, rm) # Merge each cell from estimators with the marine regions

histograms <- histograms %>% distinct(id,.keep_all = TRUE) # Remove duplicated cells

# Create your color palettes

mypal <- colorRampPalette(brewer.pal(10 , "GnBu"))

mypal(10)

mycolors <- c("#DEF2D9","#C5E8C2","#ABDEB6","#8BD2BE",
               "#6AC2C9","#4AAFD1","#3193C1","#1778B4","#085DA0","#084081")

mycolors2 <- c("#DEF2D9","#D6EFD1","#CFECC9","#C5E8C2","#B8E3BC","#ABDEB6","#9BD8B9","#8BD2BE","#7BCCC4",
               "#6AC2C9","#5AB9CE","#4AAFD1","#3EA1C9","#3193C1","#2485BA","#1778B4","#0B6BAD","#085DA0","#084E90","#084081")


# COMPLETENESS -----------------------------------------------------------------

# Map

(completeness <- ggplot(maps) +
    geom_sf(aes(fill = Completeness), color = "transparent") +
    scale_fill_stepsn(n.breaks=10, colours = hcl.colors(10, "GnBu", rev = TRUE), limits=c(0,100),breaks = c(10,20,30,40,50,60,70,80,90,100), # Change it according to your data
    na.value = "transparent", name = "Completeness", guide = guide_colorsteps(barwidth = 10, barheight = .3, title.position = "top", title.hjust = 0.5))+
    geom_spatvector(data = rm, color = "darkcyan", fill = "transparent") +
    geom_spatvector(data = countries, fill ="transparent", color = 'black') +
    theme(panel.background = element_rect(fill = "white", colour = "black"),panel.border = element_rect(fill=NA),
          legend.position = "none",
          #legend.justification = "bottom",
          #legend.direction = "horizontal",
          #legend.background = element_blank(),
          #legend.title= element_text(family =  "Arial Narrow", size = 6 ),
          #legend.text = element_text(size = 4),
          #plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
          axis.text = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks = element_blank()) +
    theme(text = element_text(family =  "Arial Narrow" )) +
    coord_sf(xlim=c(-125,-85), ylim=c(1.5,32.5))+
    ggspatial::annotation_scale(height =  unit(0.05, "cm"), text_cex  = .3, width_hint = 0.1) +
   NULL
    )

ggsave('Completeness.png', completeness, dpi = 500,limitsize = FALSE, width = 13,
       height = 10, units = "cm")

# Histogram

total <- histograms %>% filter(!is.na(Completeness)) %>%  nrow(.)

comp_hist <- ggplot(histograms, aes(Completeness)) +
  geom_histogram( binwidth = 5, boundary = 0, color ="white", fill = hcl.colors(20,"GnBu", rev = TRUE)) +
  stat_bin(binwidth = 5, center = 2.5, geom="text", color= "black", size = 3, family = "Arial Narrow",
  aes(label = after_stat(if_else (condition = round(..count../total*100,1) > 0, as.character(round(..count../total*100,1)),""))), vjust = -1)+
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10))+
  coord_cartesian(ylim = c(0, 45))+
  scale_y_continuous(expand = c(0,0))+
  theme(panel.background = element_blank(),
        axis.line.x = element_line(),
        axis.line.y = element_blank(),
        legend.position = "none",
        axis.title.y = element_text(family="Serif",size=12, color = "black"),
        axis.title.x = element_text(family="Serif",size=12, color = "black"),
        axis.text.x =  element_text(family="Serif",size = 8, color = "black"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())+
  labs(x="Completeness",y="% grid cells") +
  NULL

comp_hist

ggsave('hist_completeness.png', comp_hist, dpi = 500,limitsize = FALSE, width = 40,
       height = 20, units = "cm", bg = "transparent")


# OBSERVED RICHNESS ------------------------------------------------------------

# Map

(richness <- ggplot(maps) +
    geom_sf(aes(fill = Observed.richness), color = "transparent") +
    scale_fill_stepsn(n.breaks=10, colours = mycolors, limits=c(0,100),breaks = c(10,20,30,40,50,60,70,80,90,100), # Change it according to your data
    na.value = "transparent", name = "Observed richness", guide = guide_colorsteps(barwidth = 10, barheight = .3, title.position = "top", title.hjust = 0.5))+
    geom_spatvector(data = rm, color = "darkcyan", fill = "transparent") +
    geom_spatvector(data = countries, fill ="transparent", color = 'black') +
    theme(panel.background = element_rect(fill = "white", colour = "black"),panel.border = element_rect(fill=NA),
         legend.position = "none",
         #legend.justification = "bottom",
         #legend.direction = "horizontal",
         #legend.background = element_blank(),
         #legend.title= element_text(family =  "Arial Narrow", size = 6 ),
         #legend.text = element_text(size = 4),
         #plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
         axis.text = element_blank(),
         axis.title.x = element_blank(),
         axis.title.y = element_blank(),
         axis.ticks = element_blank()) +
    theme(text = element_text(family =  "Arial Narrow" )) +
    coord_sf(xlim=c(-125,-85), ylim=c(1.5,32.5)) +
    ggspatial::annotation_scale(height =  unit(0.05, "cm"), text_cex  = .3, width_hint = 0.1)+
    NULL
)


ggsave('Richness.png', richness, dpi = 500,limitsize = FALSE, width = 13,
       height = 10, units = "cm")

# Histogram

total <- histograms %>% filter(!is.na(Observed.richness)) %>%  nrow(.)

hist_richness <- ggplot(histograms, aes(Observed.richness)) +
  geom_histogram( binwidth = 5, boundary = 0, color ="white", fill = mycolors2) +
  stat_bin(binwidth = 5, center = 2.5, geom="text", color= "black", size = 3, family = "Serif",
  aes(label = after_stat(if_else (condition = round(..count../total*100,1) > 0, as.character(round(..count../total*100,1)),""))), vjust = -1)+
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10))+
  coord_cartesian(ylim = c(0, 170))+
  scale_y_continuous(expand = c(0,0))+
  theme(panel.background = element_blank(),
        axis.line.x = element_line(),
        axis.line.y = element_blank(),
        legend.position = "none",
        axis.title.y = element_text(family="Serif",size=12, color = "black"),
        axis.title.x = element_text(family="Serif",size=12, color = "black"),
        axis.text.x =  element_text(family="Serif",size = 8, color = "black"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())+
  labs(x="Observed richness",y="% grid cells") +
  NULL

hist_richness

ggsave('hist_richness.png', hist_richness, dpi = 500,limitsize = FALSE, width = 40,
       height = 20, units = "cm", bg = "transparent")


# RECORDS ----------------------------------------------------------------------

# Map

(records <- ggplot(maps) +
    geom_sf(aes(fill = Records), color = "transparent") +
    scale_fill_stepsn(n.breaks=10, colours = mycolors, limits=c(0,2000),breaks = c(200,400,600,800,1000,1200,1400,1600,1800,2000), # Change it according to your data
    na.value = "transparent", name = "Number of records", guide = guide_colorsteps(barwidth = 10, barheight = .3, title.position = "top", title.hjust = 0.5))+
    geom_spatvector(data = rm, color = "darkcyan", fill = "transparent") +
    geom_spatvector(data = countries, fill ="transparent", color = 'black') +
    theme(panel.background = element_rect(fill = "white", colour = "black"),panel.border = element_rect(fill=NA),
         legend.position = "none",
         #legend.justification = "bottom",
         #legend.direction = "horizontal",
         #legend.background = element_blank(),
         #legend.title= element_text(family =  "Arial Narrow", size = 6 ),
         #legend.text = element_text(size = 4),
         #plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
         axis.text = element_blank(),
         axis.title.x = element_blank(),
         axis.title.y = element_blank(),
         axis.ticks = element_blank())+
    theme(text = element_text(family =  "Arial Narrow" ))+
    coord_sf(xlim=c(-125,-85), ylim=c(1.5,32.5))+
    ggspatial::annotation_scale(height =  unit(0.05, "cm"), text_cex  = .3, width_hint = 0.1) +
   NULL
)

ggsave('Records.png', records, dpi = 500,limitsize = FALSE, width = 13,
       height = 10, units = "cm")

# Histogram

total <- histograms %>% filter(!is.na(Records)) %>%  nrow(.)

hist_records <- ggplot(histograms, aes(Records)) +
  geom_histogram( binwidth = 100, boundary = 0, color ="white", fill = mycolors2) +
  stat_bin(binwidth = 100, center = 50, geom="text", color= "black", size = 3, family = "Serif",
  aes(label = after_stat(if_else (condition = round(..count../total*100,1) > 0, as.character(round(..count../total*100,1)),""))), vjust = -1)+
  scale_x_continuous(limits = c(0, 2000), breaks = seq(0, 2000, by = 200))+
  coord_cartesian(ylim = c(0, 400))+
  scale_y_continuous(expand = c(0,0))+
  theme(panel.background = element_blank(),
        axis.line.x = element_line(),
        axis.line.y = element_blank(),
        legend.position = "none",
        axis.title.y = element_text(family="Serif",size=12, color = "black"),
        axis.title.x = element_text(family="Serif",size=12, color = "black"),
        axis.text.x =  element_text(family="Serif",size = 8, color = "black"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())+
  labs(x="Number of records",y="% grid cells") +
  NULL

hist_records

ggsave('hist_records.png', hist_records, dpi = 500,limitsize = FALSE, width = 40,
       height = 20, units = "cm", bg = "transparent")


# MEDIAN AGE OF  RECORDS -------------------------------------------------------

# Map

(ages <- ggplot(maps) +
    geom_sf(aes(fill = age), color = "transparent") +
    scale_fill_stepsn(n.breaks=10, colours = mycolors, limits=c(0,160), breaks = c(16,32,48,64,80,96,112,128,144,160), # Change it according to your data
    na.value = "transparent", name = "Age of records", guide = guide_colorsteps(barwidth = 10, barheight = .3, title.position = "top", title.hjust = 0.5))+
    geom_spatvector(data = rm, color = "darkcyan", fill = "transparent") +
    geom_spatvector(data = countries, fill ="transparent", color = 'black') +
    theme(panel.background = element_rect(fill = "white", colour = "black"),panel.border = element_rect(fill=NA),
          legend.position = "none",
          #legend.justification = "bottom",
          #legend.direction = "horizontal",
          #legend.background = element_blank(),
          #legend.title= element_text(family = "Arial Narrow", size = 6 ),
          #legend.text = element_text(size = 4),
          #plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
          axis.text = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks = element_blank())+
    theme(text = element_text(family = "Arial Narrow"))+
    coord_sf(xlim=c(-125,-85), ylim=c(1.5,32.5))+
    ggspatial::annotation_scale(height =  unit(0.05, "cm"), text_cex  = .3, width_hint = 0.1) +
    NULL
)


ggsave('Ages.png', ages, dpi = 500,limitsize = FALSE, width = 13,
       height = 10, units = "cm")

# Histogram

total <- histograms %>% filter(!is.na(age)) %>%  nrow(.)

hist_ages <- ggplot(histograms, aes(age)) +
  geom_histogram( binwidth = 8, boundary = 0, color ="white", fill = mycolors2) +
  stat_bin(binwidth = 8, center = 4, geom="text", color= "black", size = 3, family = "Arial Narrow",
  aes(label = after_stat(if_else (condition = round(..count../total*100,1) > 0, as.character(round(..count../total*100,1)),""))), vjust = -1)+
  scale_x_continuous(limits = c(0, 160), breaks = seq(0, 160, by = 16))+
  coord_cartesian(ylim = c(0, 72))+
  scale_y_continuous(expand = c(0,0))+
  theme(panel.background = element_blank(),
        axis.line.x = element_line(),
        axis.line.y = element_blank(),
        legend.position = "none",
        axis.title.y = element_text(family="Serif",size=12, color = "black"),
        axis.title.x = element_text(family="Serif",size=12, color = "black"),
        axis.text.x =  element_text(family="Serif",size = 8, color = "black"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())+
  labs(x="Median age of records",y="% grid cells") +
  NULL

hist_ages

ggsave('hist_ages.png', hist_ages, dpi = 500,limitsize = FALSE, width = 40,
       height = 20, units = "cm", bg= "transparent")


#HISTOGRAMS BY REGIONS ----------------------------------------------

# Median of each estimator

region_medians <- histograms %>% group_by(REGIONMAR) %>% 
  summarise(median.rec = median(Records), median.spp = median(Observed.richness), median.age = median(age), median.comp = round(median(Completeness, na.rm = TRUE),2))


# Completeness histograms

region_completeness <- ggplot(histograms, aes(x = Completeness, fill = REGIONMAR)) +
  geom_histogram(binwidth = 5, boundary = 0, color = "white") +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 50))+
  coord_cartesian(ylim = c(0, 20))+
  scale_y_continuous(expand = c(0,0))+
  geom_vline(data = region_medians, mapping = aes(xintercept = median.comp)) +
  theme(panel.background = element_blank(),
        panel.spacing = unit(2, "lines"),
        axis.line.x = element_line(),
        axis.ticks.length.x = unit(0.2, "cm"),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.title.x = element_text(family="Arial Narrow",size=22, color = "black"),
        axis.text.x =  element_text(family="Arial Narrow",size = 18, color = "black"),
        axis.text.y = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(family="Arial Narrow",size=22, color = "black"),
        panel.spacing.x = unit(20, 'points'))+
  facet_wrap(~REGIONMAR,nrow = 1, strip.position = "top", labeller = labeller(REGIONMAR = c(`Golfo de California` = "Gulf of California", `Golfo de México` = "Gulf of Mexico", 
                                                                                            `Mar Caribe` = "Caribbean Sea", `Pacífico Tropical` = "Tropical Pacific",`Pacífico Noroeste` = "Northeast Pacific")))+
  scale_fill_met_d(name = "Hokusai3", direction = 1) +
  labs(x="Completeness",y="Number of cells") +
  NULL

region_completeness

ggsave('hist_regioncomp.png', region_completeness, dpi = 500,limitsize = FALSE, width = 40,
       height = 20, units = "cm", bg= "transparent")

# Ages histograms

region_age <- ggplot(histograms, aes(x = age, fill = REGIONMAR)) +
  geom_histogram(binwidth = 8, boundary = 0, color = "white") +
  scale_x_continuous(limits = c(0, 160), breaks = seq(0, 160, by = 80))+
  coord_cartesian(ylim = c(0, 45))+
  scale_y_continuous(expand = c(0,0))+
  geom_vline(data = region_medians, mapping = aes(xintercept = median.age)) +
  theme(panel.background = element_blank(),
        panel.spacing = unit(2, "lines"),
        axis.line.x = element_line(),
        axis.ticks.length.x = unit(0.2, "cm"),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.title.x = element_text(family="Arial Narrow",size=22, color = "black"),
        axis.text.x =  element_text(family="Arial Narrow",size = 18, color = "black"),
        axis.text.y = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(family="Arial Narrow",size=22, color = "black"),
        panel.spacing.x = unit(20, 'points'))+
  facet_wrap(~REGIONMAR,nrow = 1, strip.position = "top", labeller = labeller(REGIONMAR = c(`Golfo de California` = "Gulf of California", `Golfo de México` = "Gulf of Mexico", 
                                                                                            `Mar Caribe` = "Caribbean Sea", `Pacífico Tropical` = "Tropical Pacific",`Pacífico Noroeste` = "Northeast Pacific")))+
  scale_fill_met_d(name = "Hokusai3", direction = 1) +
  labs(x="Median age of records",y="Number of cells") +
  NULL

region_age

ggsave('hist_regionages.png', region_age, dpi = 500,limitsize = FALSE, width = 40,
       height = 20, units = "cm", bg= "transparent")
