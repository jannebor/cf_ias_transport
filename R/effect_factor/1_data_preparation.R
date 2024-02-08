# Load libraries
library(readxl)
library(sf)
library(rnaturalearth)
library(dplyr)
library(countrycode)

# Load prepared species datasets
load("Data/output/intermediate/species/threats_v21-3")
load("Data/output/intermediate/species/rl_species_v21-3")
load("Data/output/intermediate/species/evals")
load("Data/output/intermediate/species/species_movement_v21-3")
load("Data/output/intermediate/species/df_native")
load("Data/output/intermediate/species/afrdb_df4")
df4$code3<-df4$iso

# Only consider species for which threat severity has been assessed
rl_threats<-subset(rl_threats, !is.na(rl_threats$severity))
rl_threats<-subset(rl_threats, rl_threats$severity != "Unknown")

# Add Red List category to RL threat data
rl_species<-rl_species[which(is.na(rl_species$rank)),]
cats<-c("DD", "LC", "NT", "VU", "EN", "CR", "EW", "EX", "LRlc", "LRnt", "LRcd")
rl_threats$category<-NA
for(i in 1:length(unique(cats))){
  smsub<-subset(rl_species, rl_species$category==cats[i])
  rl_threats$category[which(rl_threats$species %in% smsub$scientific_name)]<-cats[i]
}

# Group "lower risk" categories to Least Concern: LC
rl_threats$category[which(rl_threats$category=="LRlc")]<-"LC"
rl_threats$category[which(rl_threats$category=="LRnt")]<-"LC"
rl_threats$category[which(rl_threats$category=="LRcd")]<-"LC"

# Select species affected by exotic species, i.e., code 8.1
threatss<-subset(rl_threats, rl_threats$code %in% c("8.1","8.1.1","8.1.2"))
# Remove species that are not severely affected
threatss<-subset(threatss,threatss$severity !="No decline")
threatss<-subset(threatss,threatss$severity !="Negligible declines")
threatss<-subset(threatss,threatss$severity !="Causing/Could cause fluctuations")
# Only select species threatened or extinct
threatss<-threatss[which(threatss$category %in% c("DD","VU","EN","CR","EX")),]

# Only select relevant species group in dataset
species_movement<-species_movement[which(tolower(species_movement$species) %in% tolower(evals$binomial)),]
species_movement$count<-1

# Select native species
native<-df_native
native$count<-1
# Select native species for which threats & severity have been assessed
N<-species_movement[which(species_movement$origin=="Native"),]
N<-N[which(tolower(N$species) %in% tolower(rl_threats$species)),]
N$count<-1

# Select species that are threatened by exotic species
potentiallyDisappeared<-N[which(tolower(N$species) %in% tolower(threatss$species)),]

# Add RL category to data frame
cats<-c("DD", "LC", "NT", "VU", "EN", "CR", "EW", "EX", "LRlc", "LRnt", "LRcd")
potentiallyDisappeared$category<-NA
for(i in 1:length(unique(cats))){
  smsub<-subset(rl_species, rl_species$category==cats[i])
  potentiallyDisappeared$category[which(potentiallyDisappeared$species %in% smsub$scientific_name)]<-cats[i]
}

# Select exotic species
exotic<-df4

# Assemble data
# Assure only 1 count per species
N<-aggregate(count ~  code3+species, data=N, FUN=min)
potentiallyDisappeared<-aggregate(count ~ code3 + species + category, data=potentiallyDisappeared, FUN=min)
exotic<-aggregate(count ~ code3+TaxonName, data=exotic, FUN=min)
native<-aggregate(count ~ country+species, data=native, FUN=min)

# Only consider Data Deficient species that were predicted to be threatened previously
# Select all Data Deficient species
dd_species<-tolower(unique(potentiallyDisappeared$species[which(potentiallyDisappeared$category=="DD")]))
# Remove weights for all Data Deficient species
potentiallyDisappeared$count[which(potentiallyDisappeared$category=="DD")]<-0
# Retrieve predictions for Data Deficient species from https://doi.org/10.1038/s42003-022-03638-9
dd_preds<-read.csv("https://github.com/jannebor/dd_forecast/blob/main/dataframes/Partition1/predictions/dd_predictions.csv?raw=true")
for(i in 1:length(dd_species)){
  # Check if prediction exists for Data Deficient species i 
  if(length(dd_preds$threatened[which(tolower(dd_preds$binomial)==dd_species[i])])>0){
    # Add weights to Data Deficient species i when predicted to be threatened
    if(dd_preds$predict[which(tolower(dd_preds$binomial)==dd_species[i])] == "threatened"){
      potentiallyDisappeared$count[which(tolower(potentiallyDisappeared$species)==dd_species[i])]<-1
    } 
  } 
}

# Check number of data deficient species in dataset and its percentage
length(unique(dd_preds$binomial[which(tolower(dd_preds$binomial)%in% tolower(potentiallyDisappeared$species))]))
length(unique(potentiallyDisappeared$species[which(potentiallyDisappeared$category=="DD")]))/
  length(unique(potentiallyDisappeared$species))

# Count all native species for which threats & severity have been assessed per ecoregion
N<-aggregate(count ~ code3, data=N, FUN=sum)
# Count all native species threatened by exotic species per ecoregion
potentiallyDisappeared<-aggregate(count ~ code3, data=potentiallyDisappeared, FUN=sum)
# Count all exotic species per ecoregion
exotic<-aggregate(count ~ code3, data=exotic, FUN=sum)
# Count all native species per ecoregion
native<-aggregate(count ~ country, data=native, FUN=sum)
native$code3<-native$country

# Retrieve administrative borders shapefile
sf_use_s2(FALSE)
un_map <- ne_countries(scale="large",returnclass = "sf")
# Calculate area of each feature
un_map$area<-sf::st_area(un_map)

# Initialize empty data frame
ecs<-data.frame(
  code3=unique(c(species_movement$code3)),
  cont=NA,
  exotic=0, native=0, N=0, potentiallyDisappeared=0,
  Island=0, 
  area=NA)

# Add values to data frame
for(i in 1:length(unique(ecs$code3))){
  # Region
  ecs$cont[which(ecs$code3==unique(ecs$code3)[i])]<-countrycode::countrycode(unique(ecs$code3)[i], origin = "iso3c", destination = "un.region.name")
  # Area
  if(length(which(un_map$su_a3 == unique(ecs$code3)[i]))>0){
    ecs$area[which(ecs$code3==unique(ecs$code3)[i])] <-  un_map$area[which(un_map$su_a3==unique(ecs$code3)[i])]
  }
  if(length(which(un_map$adm0_a3 == unique(ecs$code3)[i]))>0){
    ecs$area[which(ecs$code3==unique(ecs$code3)[i])] <-  un_map$area[which(un_map$adm0_a3==unique(ecs$code3)[i])]
  }
  
  # Native species for which threats have been assessed
  if(length(which(N$code3==unique(ecs$code3)[i]))>0){
    ecs$N[which(ecs$code3==unique(ecs$code3)[i])] <-  N$count[which(N$code3==unique(ecs$code3)[i])]
  }
  # Affected species
  if(length(which(potentiallyDisappeared$code3==unique(ecs$code3)[i]))>0){
    ecs$potentiallyDisappeared[which(ecs$code3==unique(ecs$code3)[i])] <-  potentiallyDisappeared$count[which(potentiallyDisappeared$code3==unique(ecs$code3)[i])]
  }
  # Alien species
  if(length(which(exotic$code3==unique(ecs$code3)[i]))>0){
    ecs$exotic[which(ecs$code3==unique(ecs$code3)[i])] <-  exotic$count[which(exotic$code3==unique(ecs$code3)[i])]
  }
  # Native species
  if(length(which(native$code3==unique(ecs$code3)[i]))>0){
    ecs$native[which(ecs$code3==unique(ecs$code3)[i])] <-  native$count[which(native$code3==unique(ecs$code3)[i])]
  }
}

# Convert area to km2
ecs$area<-ecs$area/10^6
# Calculate alien species fraction (ASF)
ecs$ASF<-ecs$exotic/(ecs$exotic+ecs$native)
# calculate potentially disappered fraction (PDF) of native species
ecs$PDF<-ecs$potentiallyDisappeared/ecs$N
# Only countries that contain data in all consolidated databases
ecs<-subset(ecs, ecs$native>=1)
ecs<-subset(ecs, ecs$exotic>=1)
ecs<-ecs[which(ecs$code3 %in% unique(df2$code3)),]

# Save data
#save(ecs, file="Data/output/intermediate/models/ecs")
