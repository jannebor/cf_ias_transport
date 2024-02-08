library(readxl)
library(stringr)
library(taxize)
library(rvest)
library(raster)
library(countrycode)
library(doSNOW)
library(rredlist)
library(doParallel)
library(parallel)
# Define WGSRPD functions
pow_wgsrpd <- function(species, type){
  ppow <- NULL
  fnames <- NULL
  
  while(length(ppow)<1){
    t0<-proc.time()
    tryCatch({
      ppow<-get_pow(species, accepted = TRUE, rows = 1, messages=FALSE)
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    
    Sys.sleep(2)
    t<-t0-proc.time()
    
    if(abs(t[3])>30){
      break
    }
    
  }
  
  if(!is.na(ppow[1])){
    
    ppow_data<-pow_lookup(ppow[1])
    suppressWarnings(url<-read_html(paste("http://plantsoftheworldonline.org/taxon/",ppow[1],sep="")))
    
    selector.type<-"#distribution-listing > h3:nth-child(1)"
    fype<-html_nodes(x=url, css=selector.type) %>%
      html_text(trim=TRUE)
    if((length(grep(tolower(type),tolower(fype)))>0)){
      selector.name<-"#distribution-listing > p:nth-child(2)"
      
      fnames<-html_nodes(x=url, css=selector.name) %>%
        html_text(trim=TRUE)
    } else {
      selector.type<-"#distribution-listing > h3:nth-child(3)"
      fype<-html_nodes(x=url, css=selector.type) %>%
        html_text(trim=TRUE)
      if((length(grep(tolower(type),tolower(fype)))>0)){
        selector.name<-"#distribution-listing > p:nth-child(4)"
        
        fnames<-html_nodes(x=url, css=selector.name) %>%
          html_text(trim=TRUE)
      } else { selector.type<-"#distribution-listing > h3:nth-child(5)"
      fype<-html_nodes(x=url, css=selector.type) %>%
        html_text(trim=TRUE)
      if((length(grep(tolower(type),tolower(fype)))>0)){
        selector.name<-"#distribution-listing > p:nth-child(6)"
        
        fnames<-html_nodes(x=url, css=selector.name) %>%
          html_text(trim=TRUE)
      }
      }
    }
    
    if(length(fnames)>0){
      
      #distribution-listing > p:nth-child(2)
      fnames<-gsub("\n","", fnames)
      fnames<-gsub("\r","", fnames)
      fnames<-gsub(" ","", fnames)
      fnames<-str_split(fnames, pattern=",")
      fnames<-unlist(fnames)
      
      if(length(fnames)>0){
        for (t in 1:length(fnames)) {
          
          if (fnames[t]=="Panam???"){
            fnames[t]<-"Panama"
            
          }
          if (fnames[t]=="NorthwestTerritorie"){
            fnames[t]<-"NorthwestTerritori"
            
          }
          
          
          
        }
        
        return(fnames)
        
      }
    } else {
      message(paste("no regions reported as", type))
    }
  }
}
# Takes the output of pow_wgsrpd as input
wgsrpd_conversion <-function(wgsrpd_regions, format){
  
  if(length(wgsrpd_regions)>0){
    for (t in 1:length(wgsrpd_regions)) {
      
      if (wgsrpd_regions[t]=="Panam???"){
        wgsrpd_regions[t]<-"Panama"
      }
    }
    

    tdwg<-read.csv("https://raw.githubusercontent.com/jannebor/supporting_files/master/tdwg.csv",fileEncoding = "ISO-8859-1")
    
    tdwg$ISO<-as.character(tdwg$ISO)
    
    tdwg$Continent<-gsub(" ","", tdwg$Continent)
    tdwg$Continent<-substr(tdwg$Continent, 1, 18)
    
    tdwg$Sub_cont<-gsub(" ","", tdwg$Sub_cont)
    tdwg$Sub_cont<-substr(tdwg$Sub_cont, 1, 18)
    
    tdwg$Region<-gsub(" ","", tdwg$Region)
    tdwg$Region<-substr(tdwg$Region, 1, 18)
    
    tdwg$Country<-gsub(" ","", tdwg$Country)
    tdwg$Country<-substr(tdwg$Country, 1, 18)
    
    
    sub_df<-subset(tdwg, tdwg$Country==wgsrpd_regions[1])
    if(nrow(sub_df)==0){
      sub_df<-subset(tdwg, tdwg$Region==wgsrpd_regions[1])
      if(nrow(sub_df)==0){
        sub_df<-subset(tdwg, tdwg$Sub_cont==wgsrpd_regions[1])
        if(nrow(sub_df)==0){
          sub_df<-subset(tdwg, tdwg$Continent==wgsrpd_regions[1])
        }
      }
    }
    
    
    
    
    if(length(wgsrpd_regions)>1){
      for (l in 2:length(wgsrpd_regions)){
        sub_add<-NA
        sub_add<-subset(tdwg, tdwg$Country==wgsrpd_regions[l])
        
        if(nrow(sub_add)<1){
          
          sub_add<-subset(tdwg, tdwg$Region==wgsrpd_regions[l])
          
          if(nrow(sub_add)<1){
            sub_add<-subset(tdwg, tdwg$Sub_cont==wgsrpd_regions[l])
            
            if(nrow(sub_add)<1){
              sub_add<-subset(tdwg, tdwg$Continent==wgsrpd_regions[l])
            }
          }
        }
        
        sub_df<-rbind(sub_df, sub_add)
        
      }
      
    }
    
    if(nrow(sub_df)>0){
      
      for (k in 1:nrow(sub_df)){
        
        if (sub_df$ISO[k]=="Na"){
          sub_df$ISO[k]<-"NA"
          
        }
      }
    }
    
    if(tolower(format)=="continent code"){
      country_list<-unique(sub_df$Cont_code)
    }
    if(tolower(format)=="continent"){
      country_list<-unique(sub_df$Continent)
    }
    if(tolower(format)=="sub continent code"){
      country_list<-unique(sub_df$Sub_cont_code)
    }
    if(tolower(format)=="sub continent"){
      country_list<-unique(sub_df$Sub_cont)
    }
    if(tolower(format)=="region"){
      country_list<-unique(sub_df$Region)
    }
    if(format=="isocode5"){
      country_list<-unique(sub_df$X5Letter)
    }
    if(tolower(format)=="country"){
      country_list<-unique(sub_df$Country)
    }
    if(format=="isocode2"){
      country_list<-unique(sub_df$ISO)
    }
    
    
    return(country_list)
    
  }
}

# API key required to access IUCN data portal
iucn_key_n <- rstudioapi::askForPassword("IUCN API key")

# Retrieve full list of species from IUCN
# Define all Red List categories
cats<-c("DD", "LC", "NT", "VU", "EN", "CR", "EW", "EX", "LRlc", "LRnt", "LRcd")
# Set up parallel backend
cl <- makeCluster(8)
registerDoSNOW(cl)
iterations <- length(cats)
pb <- txtProgressBar(max = iterations, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)
# Retrieve for each Red List category all species listed at IUCN
rl_species<-foreach(i = 1:iterations,.combine = "rbind",
                    .options.snow = opts,
                    .packages = c("rredlist")) %dopar% {
                      
                      rlco<-rredlist::rl_sp_category(cats[i], iucn_key_n)
                      rlco<-rlco$result
                      rlco$category<-cats[i]
                      return(rlco)   

                    }
stopCluster(cl)
# Save data
#save(rl_species, file="Data/output/intermediate/species/rl_species_v21-3")

# retrieve IUCN threat classification for each species
cl <- makeCluster(8)
registerDoSNOW(cl)
iterations <- length(unique(rl_species$scientific_name))
pb <- txtProgressBar(max = iterations, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)
rl_threats<-foreach(i = 1:iterations, .combine = "rbind",
                    .options.snow = opts,
                    .packages = c("rredlist")) %dopar% {
                      
                      threats_s<-NULL
                      t0<-Sys.time()
                      # while loop to avoid missing values due to connection timeout
                      while(length(threats_s)<1){
                        tryCatch({
                          threats_s<-rredlist::rl_threats(unique(rl_species$scientific_name)[i], key = iucn_key_n)
                        }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
                        t1<-Sys.time()
                        print((t1-t0)[[1]])
                        Sys.sleep(2)
                        if((t1-t0)[[1]]>30){
                          break
                        }
                      }
                      
                      threats_s<-threats_s$result
                      threats_s$species<-unique(rl_species$scientific_name)[i]
                      if(length(threats_s)==8){
                        
                        return(threats_s)  
                        
                      }
                    }

stopCluster(cl)
#save(rl_threats, file="Data/output/intermediate/species/threats_v21-3")

# Retrieve countries of occurrence for all species
evals<-unique(rl_species$scientific_name)
cl <- makeCluster(14)
registerDoSNOW(cl)
iterations <- length(evals)
pb <- txtProgressBar(max = iterations, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)
# Run in parallel for each species
species_movement<-foreach(i = 1:iterations,.combine = "rbind",
                          .options.snow = opts,
                          .packages = c("rredlist")) %dopar% {
                            
                            occ_countries<-NULL
                            
                            while(is.null(occ_countries)) {
                              t0<-proc.time()
                              
                              tryCatch({
                                occ_countries<-rredlist::rl_occ_country(evals[i], key = iucn_key_n)
                              }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
                              if(is.null(occ_countries)){
                                Sys.sleep(1)
                              }
                              
                              t<-t0-proc.time()
                              
                              if(abs(t[3])>10){
                                break
                              }
                            }
                            
                            if(length(occ_countries$result)==5){
                              occ_countries<-occ_countries$result
                              occ_countries$species<-evals[i]
                              
                              return(occ_countries)  
                              
                            }
                          }
stopCluster(cl)

# Add iso country codes and region names
species_movement$cont<-NA
species_movement$code3<-NA
cs<-unique(c(species_movement$code))
for(i in 1:length(cs)){
  
  reg <- countrycode::countrycode(cs[i], origin = "iso2c", destination = "region23")
  iso <- countrycode::countrycode(cs[i], origin = "iso2c", destination = "iso3c")
  
  species_movement$cont[which(species_movement$code==cs[i])] <- reg
  species_movement$code3[which(species_movement$code==cs[i])] <- iso
  
}
# Save data
#save(species_movement, file="Data/output/intermediate/species/species_movement_v21-3")

# Retrieve species lists for comprehensively assessed groups
# mammals, birds, amphibians
evals<-rbind(data.frame(binomial=rl_comp_groups('mammals', iucn_key_n)$result$scientific_name,
                        bnam="MAMMALS",class="mammalia",kingdom="animalia"),
             data.frame(binomial=rl_comp_groups('birds', iucn_key_n)$result$scientific_name,
                        bnam="BIRDS",class="aves",kingdom="animalia"),
             data.frame(binomial=rl_comp_groups('amphibians', iucn_key_n)$result$scientific_name,
                        bnam="AMPHIBIANS",class="amphibia",kingdom="animalia"))
#save(evals, file="Data/output/intermediate/species/evals")

# Read alien first records database and select only vasc. plants
df<-readxl::read_xlsx("Data/input/ignore/GlobalAlienSpeciesFirstRecordDatabase_v2.xlsx", sheet=2)
df<-df[which(df$LifeForm %in% c("Vascular plants")),]
df$count<-1

# Define path to downloaded dataset: https://doi.org/10.5061/dryad.qbzkh18h9
path <- "Data/input/ignore/doi_10.5061_dryad.qbzkh18h9__v3"
dataset <- "full"
format <- "default"
# Metadata of the dataset
metadata_full<-read.csv(paste(path,"/",dataset,"/","metadata_",format,".csv",sep=""))

# Get all species names for retrieving native countries
species<-c(unique(as.character(metadata_full$scientificName)), unique(df$TaxonName))
# Get number of unique species in dataset
iterations <- length(unique(species))
# Initialize empty data frame to store data
df_native<-data.frame()
# Loop through all unique species
for(i in 1:iterations){
  
  # Retrieve native countries for taxon i
  wgsrpd <- pow_wgsrpd(species[i], type="Native")
  
  # Check if any native countries were found
  if(length(wgsrpd)>0){
    
    # Convert the output of `pow_wgsrpd` to 2-letter isocodes using a custom function
    countrylist <- wgsrpd_conversion(wgsrpd, format="isocode2")
    
    # Check if the conversion yielded any country codes
    if(length(countrylist)>0){
      
      # Convert the two-letter ISO country codes to three-letter ISO country codes
      countrylist<-countrycode(countrylist, origin = "iso2c", destination = "iso3c")
      
      # Create a dataframe of countries and the current species
      occ_countries<-data.frame(country=countrylist,
                                species=species[i])
      
      # Append the current species' country data to the accumulating dataframe
      df_native<-rbind(df_native, occ_countries)
    }
  }
}

#save(df_native,file="Data/output/intermediate/species/df_native")
load("Data/output/intermediate/species/df_native")

# Fix region names to ensure consistency and for matching to other data used in this study
# Correct instances where the region is labeled "USACanada"
df_s1<-df[which(df$Region=="USACanada"),] # Subset df for rows where Region is "USACanada"
df_s1$Region[which(df_s1$Region=="USACanada")]<-"Canada" # Change "USACanada" to "Canada" in the subset
df$Region[which(df$Region=="USACanada")]<-"United States" # Change "USACanada" to "United States" in the original df
df<-rbind(df, df_s1) # Append the modified subset back to the original dataframe

# Standardize names for regions associated with specific countries, handling islands and special cases
# Assign "Spain" to Canary Islands and Balearic Islands
df$Region[which(df$Region %in% c("Canary Islands","Balearic Islands"))]<-"Spain"
# Assign "United States" to Hawaiian Islands and Alaska
df$Region[which(df$Region %in% c("Hawaiian Islands","Alaska"))]<-"United States"
# Assign "Portugal" to Madeira and Azores
df$Region[which(df$Region %in% c("Madeira","Azores"))]<-"Portugal"
# Assign "Greece" to Crete
df$Region[which(df$Region %in% c("Crete"))]<-"Greece"
# Assign "United Kingdom" to Channel Islands and Ascension
df$Region[which(df$Region %in% c("Channel Islands","Ascension"))]<-"United Kingdom"
# Assign "Mauritius" to Chagos Archipelago
df$Region[(df$Region %in% c("Chagos Archipelago"))]<-"Mauritius"
# Assign "Italy" to Sicily and Sardinia
df$Region[which(df$Region %in% c("Sicily","Sardinia"))]<-"Italy"
# Assign "Australia" to Tasmania and Lord Howe Island
df$Region[which(df$Region %in% c("Tasmania","Lord Howe Island"))]<-"Australia"
# Assign "Tanzania" to Zanzibar Island
df$Region[which(df$Region %in% c("Zanzibar Island"))]<-"Tanzania"
# Assign "Ecuador" to Galapagos
df$Region[which(df$Region %in% c("Galapagos"))]<-"Ecuador"
# Assign "Svalbard and Jan Mayen" to Amsterdam Island
df$Region[which(df$Region %in% c("Amsterdam Island"))]<-"Svalbard and Jan Mayen"
# Assign "France" to Kerguelen Islands and Corse
df$Region[which(df$Region %in% c("Kerguelen Islands","Corse"))]<-"France"
# Assign "New Zealand" to Kermadec Islands
df$Region[which(df$Region %in% c("Kermadec Islands"))]<-"New Zealand"
# Assign "Yemen" to Socotra Island
df$Region[which(df$Region %in% c("Socotra Island"))]<-"Yemen"

# Note on unmatched regions:
# "Palestine, State of", "Serbia", and "Scattered Islands" do not match any of the modifications above.

# Set up parallel computing
cl <- makeCluster(8)
registerDoSNOW(cl)
iterations <- length(unique(df$Region))
pb <- txtProgressBar(max = iterations, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)
# Perform parallel processing to map country names to ISO codes
df2<-foreach(i = 1:iterations,
             .packages = "countrycode",
             .options.snow = opts,
             .combine = "rbind") %dopar% {
               # Select country i and initiate empty column
               df_s<-df[which(df$Region %in% unique(df$Region)[i]),]
               df_s$iso<-NA
               # Add 3-letter isocode to relevant column and return
               df_s$iso<-countrycode(unique(df_s$Region), "country.name", "iso3c")
               return(df_s)
             }
# Stop parallel backend
stopCluster(cl)

df4<-df2[which(!is.na(df2$iso)),]
df4<-df4[which(names(df4) %in% c("TaxonName","Region","FirstRecord","count","iso"))]
df4<-df4[!duplicated(df4),]
# Save data
#save(df4, file="Data/output/intermediate/species/afrdb_df4")
