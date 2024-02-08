# Load libraries
library(xlsx)

# Load prepared datasets
load("Data/output/results/Revision/ias_transport3")
load("Data/output/results/Revision/zz")
ias_transport<-ias_transport3

# Add effect factor to data frame
ias_transport$EF<-zz$estimate[2]

# Calculate regional CF
# Average
ias_transport$CFregional_avg<-ias_transport$FF_avg*ias_transport$EF
# Marginal
ias_transport$CFregional_mrg<-ias_transport$FF_mrg*ias_transport$EF
ias_transport$CFglobal_avg<-NA
ias_transport$CFglobal_mrg<-NA

# Effect factor represents regional species loss
# Multiply by global extinction probabilities to convert to global loss
# Global extinction probabilities retrieved from source: https://doi.orsg/10.1016/j.ecolind.2022.109204
gep<-xlsx::read.xlsx("Data/input/ignore/GEP/gep_casestudy.xlsx", sheetIndex =1)

# Calculate average across considered taxonomic groups
gep$avg <- rowMeans(gep[which(names(gep) %in% c("Mammals","Birds","Amphibians"))], na.rm = T)

# Add GEP to data
ias_transport$gep<-NA
for(i in 1:length(unique(ias_transport$importer))){
  print(i)
  if(length(which(gep$ISO3CD==unique(ias_transport$importer)[i]))>0){
    ias_transport$gep[which(ias_transport$importer==unique(ias_transport$importer)[i])]<-gep$avg[which(gep$ISO3CD==unique(ias_transport$importer)[i])]
  }
}

# Multiply by global extinction probabilities to convert to global loss
ias_transport$CFglobal_avg<-ias_transport$CFregional_avg*ias_transport$gep
ias_transport$CFglobal_mrg<-ias_transport$CFregional_mrg*ias_transport$gep

# Separate data frame for average characterisation factors
ES_Transportation_avg<-data.frame(importer=ias_transport$importer, exporter=ias_transport$exporter,
                                  CFglobal=ias_transport$CFglobal_avg, CFregional=ias_transport$CFregional_avg)

# Separate data frame for marginal characterisation factors
ES_Transportation_mrg<-data.frame(importer=ias_transport$importer, exporter=ias_transport$exporter,
                                  CFglobal=ias_transport$CFglobal_mrg, CFregional=ias_transport$CFregional_mrg)

# Save CFs as csv and xlsx
write.csv(ias_transport, file="Data/output/results/ias_transport_full.csv", row.names = F)
write.csv(ES_Transportation_avg, file="Data/output/results/CF_average.csv", row.names = F)
write.csv(ES_Transportation_mrg, file="Data/output/results/CF_marginal.csv", row.names = F)

write.xlsx(ES_Transportation_avg, 
           file="Data/output/results/CharacterisationFactors.xlsx", 
           sheetName = "Average",
           row.names = FALSE,
           showNA = FALSE)
write.xlsx(ES_Transportation_mrg, 
           file="Data/output/results/CharacterisationFactors.xlsx", 
           sheetName = "Marginal",
           row.names = FALSE,
           showNA = FALSE,
           append = TRUE)
