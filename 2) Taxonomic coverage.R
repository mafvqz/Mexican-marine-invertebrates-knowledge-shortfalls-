###==========================================================================### 
###     Unveiling spatial and temporal gaps in the knowledge of marine       ###
###                      invertebrates in Mexico                             ###
### By:Marıa Fernanda Vazquez Flores, Angela P. Cuervo-Robayo, Nuno Simoes,  ###
###      Cristina Ronquillo, Joaquın Hortal, and Enrique Martınez-Meyer      ###
###==========================================================================###

### The following script contains the steps to quantify the historical accumulation 
### of documented species and the number of records collected each year 

# Load packages -----------------------------------------------------------------

library(data.table) # Data manipulation
library(tidyverse) # Data manipulation

getwd()
setwd("Directory path")  # Set your working directory path

# Load previously processed records (script 1) ---------------------------------

data <- fread('Echinodermata_data.csv', encoding="UTF-8", header=TRUE)

# Select necessary fields 

data <- data[,c('ScientificName_accepted', 'year')]

data <- data[!is.na(year),] # Exclude records missing collection date

# Prepare data for plotting Counts x year

data <- data[order(data$year),] # Sort the year in ascending order 

data$duplicated <- duplicated(data$ScientificName) # Assign a false to each new species 

data$specacum <- ifelse(data$duplicated == FALSE, 1, 0) # Assign "1" to each new species and 0 to duplicated

data$recs <- as.integer(1) # Assign 1 to each occurrence record

occ_year <- aggregate(recs ~ year, data, sum) # Sum species by year

acum_year <- data[,c('ScientificName_accepted','year','specacum')][specacum == 1,]

acum_year$Counts <- as.integer(1) # Assign 1 to each occurrence record

acum_year2 <- aggregate(Counts ~ year, acum_year, sum) # Sum species by year

acum_year2 <- aggregate(cbind(Counts, specacum)  ~ year, acum_year, sum) # Groups by year

acum_year2$acumulado <- cumsum(acum_year2$specacum) # Sums the number of species documented each year

occ_year <- merge(occ_year, acum_year2, by = 'year', all.x = TRUE)

# Plot: Historical accumulation of documented species ---------------------------

ploty <- function(x, y, color){
  ggplot() + 
    geom_line(mapping = aes(x = x, y = y), linewidth = 1, color = color) + 
    scale_x_continuous(name= "Year", breaks = seq(1750, 2025, by = 30)) + # Change it according to your data
    ylab("Observed number of species") +
    theme(axis.line = element_line(colour = "gray20", linewidth = 0.5, linetype = "solid"), 
          panel.background = element_rect(fill = "white"),
          axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")), 
          axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm")),
          axis.text = element_text(size = 10)
    )
}

sacs <- occ_year[!is.na(occ_year$acumulado),]

(sacPlot <- ploty(x = sacs$year, y = sacs$acumulado, 'black'))

#ggsave('sac.png', sacPlot, width = 20, height = 10, units = "cm")

# Plot: Historical accumulation of records collected each year -----------------

ploty2 <- function(x, y, color){
  ggplot() + 
    geom_bar(mapping = aes(x = x, y = y), stat = "identity", fill = color) + 
    scale_x_continuous(name = "Year", breaks = seq(1750, 2025, by = 30)) + # Change it according to your data
    ylab("Number of records") +
    theme(axis.line = element_line(colour = "gray20", linewidth = 0.5, linetype = "solid"), 
          panel.background = element_rect(fill = "white"),
          axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")), 
          axis.text.y.right = element_text(margin = unit(c(0, 5, 0, 0), "mm")),
          axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm")),
          axis.text = element_text(size = 10)
    )
}

(occPlot <- ploty2(x = occ_year$year, y = occ_year$recs, 'black'))

#ggsave('occPlot.png', occPlot, width = 20, height = 10, units = "cm")


# Double Plot: Historical accumulation of documented species + number of records by year ----

prop <- round(max(occ_year$recs)/max(sacs$acumulado), 1)

plotyDouble <- function(data, x, y, z, color1, color2, temprange){
  ggplot() + 
    geom_bar(mapping = aes(x = x, y = y*1), stat = "identity", fill = 'gray50', alpha =0.7) +
    geom_line(data, 
              mapping = aes(x = year, y = acumulado*prop), 
              linewidth = 0.5, color = 'black') +
    
    scale_x_continuous(name= "Year", breaks = seq(1750, 2030, by = 30)) + # Change it according to your data
    scale_y_continuous(name = "Number of records",breaks = seq(0, 1500, by = 500), # Change it according to your data
                       sec.axis = sec_axis(~ ./prop, name = "Observed number of species", 
                                           breaks = seq(0, max(y), by = 100))) + # Change it according to your data
    theme(text = element_text(family = "Arian Narrow"),
          axis.line = element_line(colour = "black", linewidth = 0.5, linetype = "solid"), 
          panel.background = element_rect(fill = "white")) + 
    theme(text = element_text(family = "Arial Narrow"),
          axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")), 
          axis.text.y.right = element_text(margin = unit(c(0, 5, 0, 0), "mm")),
          axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm"))) +
    theme(text = element_text(family = "Arial Narrow"),
          plot.subtitle = element_text(vjust = 1), 
          plot.caption = element_text(vjust = 1), 
          axis.title = element_text(size = 26,  colour = "black"), 
          axis.text = element_text(size = 22, colour = "black"), 
          plot.title = element_text(size = 24))+
    geom_vline(xintercept=c(1900,1930,1980), linetype="dashed") # Historical periods of species diversity knowledge, change it according to your data
}

(sacRecs <- plotyDouble(sacs, 
                        x = occ_year$year, 
                        y = occ_year$recs, 
                        z = occ_year$acumulado,
                        "gray50", "black",
                        c(1900,1930,1980) # Historical periods of species diversity knowledge
))

 ggsave('SAC.png', sacRecs, width = 40, height = 20, units = "cm", dpi = 500)
