# Load libraries
library(tidyr)
library(doSNOW)

# Load previously prepared transportation flows
load("Data/output/intermediate/transport/baci_cow")
# Load distanced between countries
load("Data/output/intermediate/transport/country_distance")
# Load modified alien first records database
load("Data/output/intermediate/species/afrdb_df4")
# Load native species list
load("Data/output/intermediate/species/df_native")

# Create a new data frame 'baci_ext' from selected columns of an existing data frame 'baci_cow'.
baci_ext<-data.frame(period=baci_cow$period, TradeQuantity=baci_cow$TradeQuantity, imp3ISO=baci_cow$imp3ISO, exp3ISO=baci_cow$exp3ISO,
                     predicted=baci_cow$predicted)

# Replace NA values in 'TradeQuantity' with corresponding values from 'predicted'.
baci_ext$vals<-baci_ext$TradeQuantity
baci_ext$vals[which(is.na(baci_ext$vals))]<-baci_ext$predicted[which(is.na(baci_ext$vals))]

# Create a new column 'vals_pred' initialized with NA. Then, fill it with 'predicted' values where 'TradeQuantity' is NA.
baci_ext$vals_pred<-NA
baci_ext$vals_pred[which(is.na(baci_ext$TradeQuantity))]<-baci_ext$predicted[which(is.na(baci_ext$TradeQuantity))]

# Copy 'baci_ext' to a new data frame 'df_transport'.
df_transport<-(baci_ext)

# Use 'complete' from the tidyr package to expand 'df_transport', filling in missing combinations of 'imp3ISO', 'exp3ISO', and 'period' (from 1870 to 2019) with 'vals' set to 0. Convert the result back to a data frame.
df_transport <- df_transport %>%
  complete(imp3ISO, exp3ISO, period = 1870:2019, 
           fill = list(vals=0)) %>%
  as.data.frame()

# Convert 'imp3ISO' and 'exp3ISO' columns to factors with levels based on their unique values in 'df_transport'.
df_transport$imp3ISO<-factor(df_transport$imp3ISO, levels = unique(df_transport$imp3ISO))
df_transport$exp3ISO<-factor(df_transport$exp3ISO, levels = unique(df_transport$exp3ISO))

# Add new columns initialized with specific values (0 or NA)
df_transport$transportedSpecies<-0
df_transport$transportedSpecies_acc<-0
df_transport$speciesPool<-0
df_transport$speciesPool_i<-0
df_transport$nativeSpecies<-0
df_transport$distance<-NA

# Define all combinations of importer to exporter
isomov<-unique(paste(df_transport$imp3ISO,df_transport$exp3ISO,sep=""))

# Setup parallel computing with 6 nodes
cl <- makeCluster(6)
registerDoSNOW(cl)
iterations <- length(unique(isomov))
pb <- txtProgressBar(max = iterations, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

# Parallel processing of each importer-exporter combination
df_transport<-foreach(r = 1:length(unique(isomov)),.combine = "rbind",
                      .options.snow = opts) %dopar% {
                        
                        # Filter df_transport for current importer-exporter combination based on ISO codes
                        df_t<-df_transport[which(df_transport$imp3ISO==substr(isomov[r],1,3)),]
                        df_t<-df_t[which(df_t$exp3ISO==substr(isomov[r],4,6)),]
                        
                        # Order by period and calculate cumulative trade quantity
                        df_t<-df_t[order(df_t$period),]
                        df_t<-within(df_t, TradeQuantity_acc <- cumsum(df_t$vals))
                        
                        # Select native species data for importer
                        sp_native<-subset(df_native, df_native$country==substr(isomov[r],1,3))
                        df_t$nativeSpecies<-length(unique(sp_native$species))
                        # Additional species data selection for importer
                        sp_intr<-df4[which(df4$iso == substr(isomov[r],1,3)),]
                        
                        # Select native species data for exporter and importer for species pool calculation
                        sp_pool<-subset(df_native, df_native$country==substr(isomov[r],4,6))
                        sp_pool_i<-subset(df_native, df_native$country==substr(isomov[r],1,3))
                        
                        # Bridgehead species data selection for both importer and exporter
                        sp_bridgehead<-df4[which(df4$iso == substr(isomov[r],4,6)),]
                        sp_bridgehead_i<-df4[which(df4$iso == substr(isomov[r],1,3)),]
                        
                        # Loop through unique years in the data
                        for(y in unique(df_t$period)){
                          
                          sp_bh_s<-sp_bridgehead[which(sp_bridgehead$FirstRecord<y),]
                          sp_bh_s_i<-sp_bridgehead_i[which(sp_bridgehead_i$FirstRecord<y),]
                          
                          # combine to form the species pool of the exporting country
                          speciesPool<-unique(c(sp_pool$species, sp_bh_s$TaxonName))
                          speciesPool_i<-unique(c(sp_pool_i$species, sp_bh_s_i$TaxonName))
                          
                          # Update species pool sizes and transported species count for each period
                          df_t$speciesPool[which(df_t$period==y)]<-length(unique(speciesPool))
                          df_t$speciesPool_i[which(df_t$period==y)]<-length(unique(speciesPool_i))
                          sp_int_s<-sp_intr[which(sp_intr$FirstRecord>=1870),]
                          sp_int_s<-sp_int_s[which(sp_int_s$FirstRecord<=y),]
                          df_t$transportedSpecies_acc[which(df_t$period==y)]<-length(unique(sp_int_s$TaxonName[which(sp_int_s$TaxonName %in% speciesPool)]))
                          sp_int_s<-sp_intr[which(sp_intr$FirstRecord==y),]
                          df_t$transportedSpecies[which(df_t$period==y)]<-length(unique(sp_int_s$TaxonName[which(sp_int_s$TaxonName %in% speciesPool)]))
                        }
                        
                        # Add distance between exporter and importer to the data frame
                        if(length(which(names(poly_dist)==substr(isomov[r],1,3)))>0){
                          if(length(which(rownames(poly_dist)==substr(isomov[r],4,6)))>0){
                            df_t$distance <- round(as.numeric(poly_dist[which(
                              rownames(poly_dist)==substr(isomov[r],4,6)),
                              which(names(poly_dist)==substr(isomov[r],1,3))
                            ]),0)
                          }
                        }
                        # Return the modified data frame
                        return(df_t)
                      }
# Stop the parallel cluster after processing to free resources
stopCluster(cl)

# Add region names to dataset
df_transport$impCont<-NA
df_transport$expCont<-NA
countries<-unique(c(df_transport$imp3ISO,df_transport$exp3ISO))
# Loop through each unique country in dataset
for(i in 1:length(countries)){
  # Select country i
  iso <- as.character(countries[i])
  # Fetch regions
  df_transport$impCont[which(df_transport$imp3ISO==iso)] <-
    countrycode::countrycode(iso, origin = "iso3c", destination = "region23")
  df_transport$expCont[which(df_transport$exp3ISO==iso)] <-
    countrycode::countrycode(iso, origin = "iso3c", destination = "region23")
}

# Save the data
#save(df_transport, file="Data/output/intermediate/models/df_transport")
