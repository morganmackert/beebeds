#-------------------------------------------------------------------#
#                           SUMMARY STATS                           #
#-------------------------------------------------------------------#

#Research Question: Are bee beds (totes filled with sand/soil mixture) capable of providing nesting habitat for ground-nesting bees?

#Objectives:
#Determine availability of bee habitat resources (bare ground (%), vegetation coverage (%), number of floral ramets, number of flowers, floral species richness) within transects at bee bed sites
#Determine availability of bee habitat resources (bare ground (%), vegetation coverage (%), height of tallest plant, number of floral ramets, number of flowers, floral species richness) near bee beds

#Start ####
#Clear environment and set working directory
rm(list=ls())
setwd("~/UIUC/Data/USDA Bee Beds")

#Load libraries
library(lubridate)
library(tidyr)
library(plyr)
library(dplyr)
library(plotrix)

#Read in data
Veg <- read.csv("Vegetation/USDA Bee bed transect vegetation.csv", na.strings = c("", "NA"))
bbVeg <- read.csv("Vegetation/USDA Bee bed vegetation.csv", na.strings = c("", "NA"))
Bees <- read.csv("Bees/USDA Bee bed bees.csv", na.strings = c("", "NA"))

#Change column headings in Veg and bbVeg so they're not so weird
colnames(Veg)[which(names(Veg) == "Bare.Ground....")] <- "BareGround"
colnames(Veg)[which(names(Veg) == "Vegetation....")] <- "Vegetation"
colnames(Veg)[which(names(Veg) == "No..Ramets")] <- "No.Ramets"
colnames(Veg)[which(names(Veg) == "No..Flowers")] <- "No.Flowers"
colnames(bbVeg)[which(names(bbVeg) == "Bare.Ground....")] <- "BareGround"
colnames(bbVeg)[which(names(bbVeg) == "Vegetation....")] <- "Vegetation"
colnames(bbVeg)[which(names(bbVeg) == "Tallest.Plant..cm.")] <- "Tallest.Plant"
colnames(bbVeg)[which(names(bbVeg) == "No..Ramets")] <- "No.Ramets"
colnames(bbVeg)[which(names(bbVeg) == "No..Flowers")] <- "No.Flowers"

#Transect Bare Ground ####
#-------------------------------------------------------------------#
#                Transect Bare Ground Availability                  #
#-------------------------------------------------------------------#
#Calculate total bare ground in each quadrat/site; indicate [1] to take only the first value from each quadrat (meaning exclude multiple entries for the same quadrat due to multiple floral species observed)
bareground <- Veg %>%
  group_by(Site, Transect, Quadrat) %>%
  summarise(total.bareground = BareGround[1])

#Calculate average +/- standard error bare ground cover for each site and calculate the number of quadrats 
avg.bareground <- bareground %>%
  group_by(Site) %>%
  summarise(avg.bareground = mean(total.bareground),
            se.bareground = std.error(total.bareground),
            number.quadrats = length(total.bareground))
#All sites have 25 quadrats, which is correct for 5 transects of 5 quadrats each

#Transect Vegetation Coverage ####
#-------------------------------------------------------------------#
#                   Transect Vegetation Coverage                    #
#-------------------------------------------------------------------#
#Calculate total vegetation in each quadrat/site; indicate [1] to take only the first value from each quadrat (meaning exclude multiple entries for the same quadrat due to multiple floral species observed)
vegetation <- Veg %>%
  group_by(Site, Transect, Quadrat) %>%
  summarise(total.vegetation = Vegetation[1])

#Calculate average vegetation coverage for each site and calculate the number of quadrats 
avg.veg <- vegetation %>%
  group_by(Site) %>%
  summarise(avg.veg = mean(total.vegetation),
            se.veg = std.error(total.vegetation),
            number.quadrats = length(total.vegetation))
#All sites/dates have 25 quadrats, which is correct for 5 transects of 5 quadrats each

#Transect Floral Ramets ####
#-------------------------------------------------------------------#
#                  Transect Number of Floral Ramets                 #
#-------------------------------------------------------------------#
#Fill NAs with 0 in Veg$No.Ramets
Veg$No.Ramets[is.na(Veg$No.Ramets)] <- 0

#Calculate total number of ramets in bloom for each site
ramets <- Veg %>%
  group_by(Site) %>%
  summarise(total.ramets = sum(No.Ramets))

#Transect Flowers ####
#-------------------------------------------------------------------#
#                    Transect Number of Flowers                     #
#-------------------------------------------------------------------#
#Convert Veg$No.Flowers to numeric
Veg$No.Flowers <- as.numeric(Veg$No.Flowers)

#Fill NAs with 0 in Veg$No.Flowers
Veg$No.Flowers[is.na(Veg$No.Flowers)] <- 0

#Calculate total number of flowers in bloom for each site
flowers <- Veg %>%
  group_by(Site) %>%
  summarise(total.flowers = sum(No.Flowers))

#Transect Floral Species Richness ####
#-------------------------------------------------------------------#
#                   Transect Floral Species Richness                #
#-------------------------------------------------------------------#
#Determine number of floral species in bloom for each site
floralspp <- Veg %>%
  filter(!is.na(Blooming.Species)) %>%
  group_by(Site) %>%
  summarise(no.floralspp = n_distinct(Blooming.Species))

#Determine total number of floral species in bloom during 2019
floralspp_total <- Veg %>%
  filter(!is.na(Blooming.Species)) %>%
  summarise(no.floralspp = n_distinct(Blooming.Species))

#Transect Total Habitat Resources ####
#-------------------------------------------------------------------#
#                  Transect Total Habitat Resources                 #
#-------------------------------------------------------------------#
#Join all habitat resource dataframes together: avg.bareground, avg.veg, floralspp, ramets, and flowers
habi <- left_join(avg.bareground, avg.veg, by = "Site")
habit <- left_join(habi, floralspp, by = "Site")
habita <- left_join(habit, ramets, by = "Site")
habitat <- left_join(habita, flowers, by = "Site")

#Export as .csv file
#write.csv(habitat, "C:/Users/Morgan/Documents/UIUC/Analyses/beebeds/Data/Vegetation/Bee Beds Total Habitat Resources.csv", row.names = FALSE)

#Bee Beds Bare Ground ####
#-------------------------------------------------------------------#
#                Bee Beds Bare Ground Availability                  #
#-------------------------------------------------------------------#
#Calculate total bare ground in each quadrat/site; indicate [1] to take only the first value from each quadrat (meaning exclude multiple entries for the same quadrat due to multiple floral species observed)
bb.bareground <- bbVeg %>%
  group_by(Site, Cluster, Bed.Color) %>%
  summarise(total.bareground = BareGround[1])

#Calculate average +/- standard error bare ground cover for each site and calculate the number of quadrats 
bb.avg.bareground <- bb.bareground %>%
  group_by(Site) %>%
  summarise(avg.bareground = mean(total.bareground),
            se.bareground = std.error(total.bareground),
            number.quadrats = length(total.bareground))
#All sites have 9 quadrats, which is correct for 3 clusters of 3 beds

#Bee Beds Vegetation Coverage ####
#-------------------------------------------------------------------#
#                   Bee Beds Vegetation Coverage                    #
#-------------------------------------------------------------------#
#Calculate total vegetation in each quadrat/site; indicate [1] to take only the first value from each quadrat (meaning exclude multiple entries for the same quadrat due to multiple floral species observed)
bb.vegetation <- bbVeg %>%
  group_by(Site, Cluster, Bed.Color) %>%
  summarise(total.vegetation = Vegetation[1])

#Calculate average vegetation coverage for each site and calculate the number of quadrats 
bb.avg.veg <- bb.vegetation %>%
  group_by(Site) %>%
  summarise(avg.veg = mean(total.vegetation),
            se.veg = std.error(total.vegetation),
            number.quadrats = length(total.vegetation))
#All sites have 9 quadrats, which is correct for 3 clusters of 3 beds

#Bee Beds Floral Ramets ####
#-------------------------------------------------------------------#
#                  Bee Beds Number of Floral Ramets                 #
#-------------------------------------------------------------------#
#Fill NAs with 0 in Veg$No.Ramets
bbVeg$No.Ramets[is.na(bbVeg$No.Ramets)] <- 0

#Calculate total number of ramets in bloom for each site
bb.ramets <- bbVeg %>%
  group_by(Site) %>%
  summarise(total.ramets = sum(No.Ramets))

#Bee Beds Flowers ####
#-------------------------------------------------------------------#
#                    Bee Beds Number of Flowers                     #
#-------------------------------------------------------------------#
#Convert Veg$No.Flowers to numeric
bbVeg$No.Flowers <- as.numeric(bbVeg$No.Flowers)

#Fill NAs with 0 in Veg$No.Flowers
bbVeg$No.Flowers[is.na(bbVeg$No.Flowers)] <- 0

#Calculate total number of flowers in bloom for each site
bb.flowers <- bbVeg %>%
  group_by(Site) %>%
  summarise(total.flowers = sum(No.Flowers))

#Bee Beds Floral Species Richness ####
#-------------------------------------------------------------------#
#                   Bee Beds Floral Species Richness                #
#-------------------------------------------------------------------#
#Determine number of floral species in bloom for each site
bb.floralspp <- bbVeg %>%
  filter(!is.na(Blooming.Species)) %>%
  group_by(Site) %>%
  summarise(no.floralspp = n_distinct(Blooming.Species))

#Determine total number of floral species in bloom during 2019
bb.floralspp_total <- bbVeg %>%
  filter(!is.na(Blooming.Species)) %>%
  summarise(no.floralspp = n_distinct(Blooming.Species))

#Bee Beds Total Habitat Resources ####
#-------------------------------------------------------------------#
#                  Bee Beds Total Habitat Resources                 #
#-------------------------------------------------------------------#
#Join all habitat resource dataframes together: avg.bareground, avg.veg, floralspp, ramets, and flowers
bb.habi <- left_join(avg.bareground, avg.veg, by = "Site")
bb.habit <- left_join(habi, floralspp, by = "Site")
bb.habita <- left_join(habit, ramets, by = "Site")
bb.habitat <- left_join(habita, flowers, by = "Site")

#Export as .csv file
#write.csv(habitat, "C:/Users/Morgan/Documents/UIUC/Analyses/beebeds/Data/Vegetation/Bee Beds Total Habitat Resources.csv", row.names = FALSE)
