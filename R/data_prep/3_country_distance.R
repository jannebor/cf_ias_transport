# Load libraries
library(sf)
library(rnaturalearth)
library(doSNOW)

# Retrieve administrative borders shapefile
un_map <- ne_countries(scale="large",returnclass = "sf")
un_map$iso_a3[which(is.na(un_map$iso_a3))]<-un_map$adm0_a3[which(is.na(un_map$iso_a3))]
un_map<-st_simplify(un_map, dTolerance = 0.01)

# Calculate distance between countries based on shapefile
# Set up parallel cluster
cl <- makeCluster(12)
registerDoSNOW(cl)
iterations <- length(unique(un_map$iso_a3))
cs<-unique(un_map$iso_a3)
pb <- txtProgressBar(max = iterations, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)
# Run distance calculations in parallel
poly_dist<-foreach(i = 1:iterations,
                   .combine = "cbind",
                   .packages = "sf",
                   .inorder = TRUE,
                   .options.snow = opts) %dopar% {
                     # Select country i
                     a<-subset(un_map, un_map$iso_a3==cs[i])
                     # Calculate distance of country i to all other countries
                     val<-st_distance(a, un_map, by_element = T)
                     # Convert to results to data frame and return
                     val<-as.data.frame(val)
                     rownames(val)<-cs
                     colnames(val)<-cs[i]
                     return(val)
                     
                   }
# Stop parallel computation
stopCluster(cl)
# Save results
save(poly_dist, file="Data/output/intermediate/transport/country_distance")
