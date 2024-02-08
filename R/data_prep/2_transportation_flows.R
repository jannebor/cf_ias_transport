# Load libraries
library(doSNOW)
library(countrycode)
library(tidyr)

# Set working directory
setwd("")

# Prepare parallel binding of baci data
cl <- makeCluster(12)
registerDoSNOW(cl)
iterations <- length(unique(1995:2019))
pb <- txtProgressBar(max = iterations, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

# Run iterations in parallel
baci_trade<-foreach(y = 1995:2019,.combine = "rbind",
                    .options.snow = opts) %dopar% {
                      
                      # Required libraries
                      library(stringr)
                      # Load baci data for year y
                      load(paste("Data/input/ignore/BACI_HS92_V202102/converted/BACI_HS92_Y",y,"_V202102",sep=""))
                      
                      # Change names of data frame
                      names(baci)<-c("period","i","j", "k", "TradeValue", "TradeQuantity", "code",
                                     "sitc3","imp3ISO","exp3ISO","expCont","impCont")
                      
                      # Data manipulations
                      baci$sitc3<-stringr::str_sub(baci$sitc3, 1, 1)
                      baci$TradeQuantity<-as.numeric(baci$TradeQuantity)
                      baci$sitc3<-as.factor(baci$sitc3)
                      baci$j<-as.character(baci$j)
                      baci$i<-as.character(baci$i)
                      
                      # Aggregate data to sum per commodity group
                      trade_null<-aggregate(TradeQuantity ~ period + sitc3 + i + j,
                                            data=baci, FUN=sum)
                      # Return data frame for year y
                      return(trade_null)
                      
                    }
# Stop the parallel cluster
stopCluster(cl)

# Aggregate to sum across entire period (1995 - 2019)
baci_trade<-aggregate(TradeQuantity ~ period + i + j,
                      data=baci_trade, FUN=sum)

# Initiate empty columns
baci_trade$imp3ISO<-NA
baci_trade$exp3ISO<-NA
baci_trade$expCont<-NA
baci_trade$impCont<-NA

# Read table with country conversion codes
baci_countries<-read.csv("Data/input/BACI/country_codes_V202102.csv")

# Define all countries in the dataset
countries<-unique(c(baci_trade$i,baci_trade$j))

# Add iso codes for each country
for(i in 1:length(countries)){
  
  # Fetch data from conversion table and add to matching rows
  iso<-baci_countries$iso_3digit_alpha[which(baci_countries$country_code==countries[i])]
  baci_trade$exp3ISO[which(baci_trade$i==countries[i])] <- iso
  baci_trade$imp3ISO[which(baci_trade$j==countries[i])] <- iso
  
  # Add regions
  baci_trade$impCont[which(baci_trade$imp3ISO==iso)] <-
    countrycode::countrycode(iso, origin = "iso3c", destination = "region23")
  baci_trade$expCont[which(baci_trade$exp3ISO==iso)] <-
    countrycode::countrycode(iso, origin = "iso3c", destination = "region23")
}

baci_trade$expCont<-factor(baci_trade$expCont)
baci_trade$impCont<-factor(baci_trade$impCont)
baci_trade$ContMov<-factor(baci_trade$ContMov)
baci_trade$imp3ISO<-factor(baci_trade$imp3ISO)
baci_trade$exp3ISO<-factor(baci_trade$exp3ISO)

# Load trade flows from correlates of war project
cow_trade<-read.csv("Data/input/ignore/COW_Trade_4.0/COW_Trade_4.0/Dyadic_COW_4.0.csv")
cow_cs<-read.csv("Data/input/ignore/COW_Trade_4.0/COW_Trade_4.0/Dyadic_COW_4.0.csv")
# Define all countries in dataset
countries<-unique(cow_cs$StateNme)

# Add standardized names to CoW conversion table
for(i in 1:length(countries)){
  # Fetch 3-letter iso codes and add to corresponding row
  iso <- countrycode::countrycode(countries[i], origin = "country.name", destination = "iso3c")
  cow_cs$iso3[which(cow_cs$StateNme==countries[i])] <- iso
  # Add regions
  cow_cs$expCont[which(cow_cs$StateNme==countries[i])] <-
    countrycode::countrycode(iso, origin = "iso3c", destination = "region23")
}

# Define all countries in dataset
countries<-unique(c(cow_trade$importer1,cow_trade$importer2))
# Initiate empty columns
cow_trade$imp1<-NA
cow_trade$imp2<-NA

# Add names to CoW dataset
for(i in 1:length(countries)){
  # Fetch 3-letter iso code from conversion table and add to relevant rows
  iso<-cow_cs$iso3[which(cow_cs$StateNme==countries[i])]
  cow_trade$imp1[which(cow_trade$importer1 %in% countries[i])]
  cow_trade$imp1[which(cow_trade$importer1 %in% countries[i])] <- iso
  cow_trade$imp2[which(cow_trade$importer2 %in% countries[i])] <- iso
}

# Re-organize data frame
# Set up parallel cluster
cl <- makeCluster(14)
registerDoSNOW(cl)
iterations <- length(countries)
pb <- txtProgressBar(max = iterations, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

# Run iterations across countries in parallel
cow_trade<-foreach(i = 1:iterations,
                   .combine = "rbind",
                   .options.snow = opts) %dopar% {
                     
                     # Initialize empty data frame to store data
                     cow_s_return<-data.frame()
                     
                     # Check if the current country (countries[i]) is in the 'importer1' field and process trade data accordingly
                     if(length(cow_trade$importer1[which(cow_trade$importer1 %in% countries[i])])>0){
                       
                       # Subset 'cow_trade' data frame for rows where the 'importer1' matches the current country
                       cow_s<-cow_trade[which(cow_trade$importer1 %in% countries[i]),]
                       # Iterate over each unique 'importer2' within the subset 'cow_s'
                       for (e in 1:length(unique(cow_s$importer2))) {
                         
                         # Further subset 'cow_s' for rows where 'importer2' matches the current unique value
                         cow_s_s<-cow_s[which(cow_s$importer2 == unique(cow_s$importer2)[e]),]
                         
                         # Prepare a data frame representing trade from importer1 to importer2
                         cow_s1<-data.frame(year=cow_s_s$year,
                                            importer=cow_s_s$importer1,
                                            exporter=cow_s_s$importer2,
                                            imp1=cow_s_s$imp1,
                                            exp1=cow_s_s$imp2,
                                            flow=cow_s_s$flow1,
                                            smoothflow=cow_s_s$smoothflow1)
                         # Prepare a data frame representing reverse trade from importer2 back to importer1
                         cow_s2<-data.frame(year=cow_s_s$year,
                                            importer=cow_s_s$importer2,
                                            exporter=cow_s_s$importer1,
                                            imp1=cow_s_s$imp2,
                                            exp1=cow_s_s$imp1,
                                            flow=cow_s_s$flow2,
                                            smoothflow=cow_s_s$smoothflow2)
                         
                         # Combine the two data frames and append to the return variable for this iteration
                         cow_s_s<-rbind(cow_s1, cow_s2)
                         cow_s_return<-rbind(cow_s_return, cow_s_s)
                       }
                     }
                     
                     # Repeat the similar process for 'importer2' matching the current country, indicating bidirectional trade consideration
                     if(length(cow_trade$importer2[which(cow_trade$importer2 %in% countries[i])])>0){
                       
                       cow_s<-cow_trade[which(cow_trade$importer2 %in% countries[i]),]
                       for (e in 1:length(unique(cow_s$importer1))) {
                         
                         cow_s_s<-cow_s[which(cow_s$importer1 == unique(cow_s$importer1)[e]),]
                         
                         cow_s1<-data.frame(year=cow_s_s$year,
                                            importer=cow_s_s$importer1,
                                            exporter=cow_s_s$importer2,
                                            imp1=cow_s_s$imp1,
                                            exp1=cow_s_s$imp2,
                                            flow=cow_s_s$flow1,
                                            smoothflow=cow_s_s$smoothflow1)
                         cow_s2<-data.frame(year=cow_s_s$year,
                                            importer=cow_s_s$importer2,
                                            exporter=cow_s_s$importer1,
                                            imp1=cow_s_s$imp2,
                                            exp1=cow_s_s$imp1,
                                            flow=cow_s_s$flow2,
                                            smoothflow=cow_s_s$smoothflow2)
                         
                         cow_s_s<-rbind(cow_s1, cow_s2)
                         cow_s_return<-rbind(cow_s_return, cow_s_s)
                       }
                     }
                     
                     # Return the compiled data for this iteration
                     return(cow_s_return)
                   }
# Stop the parallel cluster
stopCluster(cl)

# Remove duplicated records
cow_trade<-cow_trade[!duplicated(cow_trade),]

# Give each combination of trade partners per year a unique identifier
cow_trade$isomovyr<-paste(cow_trade$exp1, cow_trade$imp1, cow_trade$year, sep="")

# Polynomial regression for extrapolating quantities
# Create working copy
cow_trade2<-cow_trade

# Expand cow_trade2 to include all combinations of imp1, exp1, and years from 1870 to 2019, filling missing 'flow' values with NA
cow_trade2 <- cow_trade2 %>%
  complete(imp1, exp1, year = 1870:2019, 
           fill = list(flow=NA)) %>%
  as.data.frame()

# Keep only the relevant columns (imp1, exp1, year, flow, smoothflow) and rows where imp1 and exp1 are not NA
cow_trade2<-cow_trade2[which(names(cow_trade2) %in% c("imp1","exp1","year","flow","smoothflow"))]
cow_trade2<-cow_trade2[which(!is.na(cow_trade2$imp1)),]
cow_trade2<-cow_trade2[which(!is.na(cow_trade2$exp1)),]

# Create a unique identifier for each trade movement by concatenating exporter, importer, and year
cow_trade2$isomovyr<-paste(cow_trade2$exp1, cow_trade2$imp1, cow_trade2$year, sep="")

# Prepare the baci dataset similarly to cow_trade2
baci_cow<-baci_trade
baci_cow <- baci_cow %>%
  complete(imp3ISO, exp3ISO, period = 1870:2019, 
           fill = list(TradeQuantity=NA)) %>%
  as.data.frame()
baci_cow<-baci_cow[which(names(baci_cow) %in% c("imp3ISO","exp3ISO","period","TradeQuantity"))]
baci_cow<-baci_cow[which(!is.na(baci_cow$imp3ISO)),]
baci_cow<-baci_cow[which(!is.na(baci_cow$exp3ISO)),]
baci_cow$isomovyr<-paste(baci_cow$exp3ISO, baci_cow$imp3ISO, baci_cow$period, sep="")

# Combine the cow_trade2 data into baci_cow based on the matching 'isomovyr' identifier
baci_cow<-cbind(baci_cow,cow_trade2[match(baci_cow$isomovyr,cow_trade2$isomovyr),])

# Align imp1 and exp1 in baci_cow with imp3ISO and exp3ISO for consistency
baci_cow$imp1<-baci_cow$imp3ISO
baci_cow$exp1<-baci_cow$exp3ISO

# Create a combined ISO code identifier for unique pairs of exporter and importer
baci_cow$isomov<-paste(baci_cow$exp3ISO, baci_cow$imp3ISO, sep="")
exps<-unique(baci_cow$isomov)
# Set 'flow' values that are less than or equal to 0 to NA to clean the data for analysis
baci_cow$flow[which(baci_cow$flow<=0)]<-NA

# Initialize a parallel computing cluster with 14 nodes
cl <- makeCluster(14)
registerDoSNOW(cl)
# Set the number of iterations based on the length of the unique exporter-importer combinations
iterations <- length(exps)
pb <- txtProgressBar(max = iterations, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

# Perform the polynomial regression in parallel for each unique exporter-importer pair
baci_cow<-foreach(e = 1:iterations,
                  .combine = "rbind",
                  .options.snow = opts) %dopar% {
                    
                    # Function to calculate the R-squared value
                    rsq <- function (x, y) cor(x, y) ^ 2
                    
                    # Subset the data for the current exporter-importer pair
                    d<-baci_cow[which(baci_cow$isomov==exps[e]),]
                    # Further filter the subset for rows with non-NA 'flow' values
                    d2<-d[which(!is.na(d$flow)),]
                    
                    # Proceed if there are rows to analyze
                    if(nrow(d)>0){
                      # Check if there are non-NA TradeQuantity values to work with
                      if(length(which(!is.na(d$TradeQuantity))>0)){
                        
                        # Ensure there are more than three unique 'flow' values for a valid polynomial regression
                        if(length(unique(d2$flow[which(!is.na(d2$TradeQuantity))]))>3){
                          
                          # Fit a polynomial regression model without an intercept (indicated by -1) to predict TradeQuantity based on flow and its square
                          m<-lm(TradeQuantity ~ flow+I(flow^2)-1, data=d)
                          
                          # Predict trade quantities using the fitted model
                          d$predicted<-predict(m, d)
                          # Calculate R-squared and RMSE for the model
                          s<-summary(m)
                          d$r2<-s$r.squared
                          d$rmse<-sqrt(mean((d$TradeQuantity[which(!is.na(d$TradeQuantity))]-d$predicted[which(!is.na(d$TradeQuantity))])^2))
                          
                          return(d)
                          
                        } else {
                          # If there are not enough unique 'flow' values, mark predictions and metrics as NA
                          d$predicted<-NA
                          d$r2<-NA
                          d$rmse<-NA
                          return(d)
                        }
                      }
                    }
                  }
# Stop the cluster
stopCluster(cl)
# Save the data frame
#save(baci_cow, file="Data/output/intermediate/transport/baci_cow")
