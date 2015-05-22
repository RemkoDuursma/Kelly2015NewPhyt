library(doBy)
library(plantecophys)
library(broom)
library(reporttools)

source("R/functions.R")
source("R/figures.R")

# Set up project structure if needed
cd <- function(x)if(!dir.exists(x))dir.create(x)
cd("output/figures")
cd("output/data")
cd("cache")


# Read raw data.

#----- Spot gas exchange
spot <- read.csv("data/Gas exchange for paper 1 data.csv")
spot <- droplevels(subset(spot, M == "B"))
spot$species <- as.factor(substr(as.character(spot$ST),1,3))

spot$treatment <- substr(as.character(spot$ST), 4, nchar(as.character(spot$ST)))
spot$CO2_treatment <- as.factor(substr(spot$treatment,1,1))
spot$Water_treatment <- as.factor(substr(spot$treatment, 2, nchar(spot$treatment)))

spotagg <- summaryBy(Photo + Ci ~ CO2_treatment + Water_treatment + species, data=spot,
                     FUN=c(mean,se))

pilspot <- spot[grep("PIL",spot$ST),]
popspot <- spot[grep("POP",spot$ST),]

#----- A-Ci curves
pilaci <- read.csv("data/EUC_ACI_CURVES_PIL.csv")
popaci <- read.csv("data/EUC_ACI_CURVES_POP.csv")

pilaci <- subset(pilaci, Ci < 800)
popaci <- subset(popaci, Ci < 800)

pilaci <- subset(pilaci,Month=="OCTOBER")
popaci <- subset(popaci,Month=="JANUARY")


#----- Climate data for envelope figure.
piluclim <- read.csv("data/climate data Pilularis.csv")
popuclim <- read.csv("data/climate data Populnea.csv")
climdat <- rbind(piluclim, popuclim)
climdat$id <- NULL
names(climdat) <- c("species","Tmax","MAP")


#---- Euc biomass, WUE, etc.
EUCBIOMASS <- read.csv("data/EUC_BIOMASSdata.csv")

#---- Weekly transpiration
PILBIOMASS <- read.csv("data/PILTRANSAA.csv")
POPBIOMASS <- read.csv("data/POPTRANSAA.csv")


#---- g1 and height.
eucg1h <- read.csv("data/EUC_g1_HEIGHT.csv")


#---- Biomass with ITE
eucbiomite <- read.csv("data/EUC_BIOMASSdata_wue_ite_added.csv")
