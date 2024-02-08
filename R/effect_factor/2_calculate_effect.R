# Load libraries
library(quantreg)
library(dplyr)
library(countrycode)
library(tidyverse)
library(sf)

# Load prepared dataset
load("Data/output/intermediate/models/ecs")

# Quantile regression
# Through the 5th percentile
summary(rq(PDF~ASF, tau=0.05,  
           weights = log10(area),
           data=ecs))
# Through the 10th percentile
summary(rq(PDF~ASF, tau=0.1,  
           weights = log10(area),
           data=ecs))
# Through the 15th percentile
summary(rq(PDF~ASF, tau=0.15,  
           weights = log10(area),
           data=ecs))

# Fit quantile regression through 5th percentile
m_qr<-rq(PDF~ASF, tau=0.05,
         weights = log10(area),
         data=ecs)
# Store fit as data frame and save
zz<-broom::tidy(m_qr)
# Save sf feature collection, model, and coefficients
save(zz, file="Data/output/results/zz")
zz<-as.data.frame(zz)
write.csv(zz, file="Data/output/results/effect_factor_coef.csv", row.names = F)
save(m_qr, file="Data/output/intermediate/models/m_qr")
