# Load libraries
library(lme4)
library(AICcmodavg)
library(doSNOW)

# Load previously generated data and models
load("lfs/models/mod")
load("Data/output/intermediate/models/z")
load("Data/output/intermediate/models/Formulas")
load("Data/output/intermediate/models/df_transport")

# Add a unique identifier for each importer-exporter pair
df_transport$isomov<-paste(df_transport$imp3ISO,df_transport$exp3ISO,sep="")

# Calculates the rolling sum of `transportedSpecies` and `vals` over a window of 10 observations, aligned to the right. This is done for each unique importer-exporter pair.
df_transport<-df_transport %>% 
  group_by(isomov) %>% 
  mutate(ES_rolling = RcppRoll::roll_sum(transportedSpecies, 10, fill = NA, align="right"),
         TR_rolling = RcppRoll::roll_sum(vals, 10, fill = NA, align="right"))

# This filters `df_transport` to include only the records within a specified range of years, at ten-year intervals.
df_transport<-df_transport[which(df_transport$period %in% seq.int(1869,2019,10)),]

# Convert distance to kilometers and adjust trade quantities
df_transport$distancekm<-df_transport$distance/1000
df_transport$TradeQuantity_acc <- df_transport$TradeQuantity_acc*1000
df_transport$TR_rolling <- df_transport$TR_rolling*1000

# Log transformation and renaming of variables for easier interpretation
df_transport$TR<-log(df_transport$TradeQuantity_acc+1)
df_transport$P<-log(df_transport$speciesPool+1)
df_transport$D<-df_transport$distancekm
df_transport$S<-log(df_transport$nativeSpecies+1)
df_transport$importingCountry<-df_transport$imp3ISO
df_transport$exportingCountry<-df_transport$exp3ISO
df_transport$donor_to_recipient<-paste(df_transport$expCont, df_transport$impCont, sep="_")
df_transport$donor_to_recipient<-as.factor(df_transport$donor_to_recipient)

# Select best model
m3<-mod[[which(Formulas %in% z[1,1])]]

# Clone data frames and manipulate transported quantity for the year 2019
df_transport_null<-df_transport[which(df_transport$period %in% c(2019)),]
df_transport_avg<-df_transport[which(df_transport$period %in% c(2019)),]
df_transport_marginal<-df_transport[which(df_transport$period %in% c(2019)),]

# For average fate calculation, set transported quantity to 0
df_transport_null$TradeQuantity_acc<-0

# For marginal calculation, add the annual average quantity to total transported quantity
annual_kg<-df_transport_marginal$TR_rolling/length(2010:2019)
df_transport_marginal$TradeQuantity_acc<-df_transport_marginal$TradeQuantity_acc+annual_kg

# Log transformation of new transport values
df_transport_null$TR<-log(df_transport_null$TradeQuantity_acc+1)
df_transport_marginal$TR<-log(df_transport_marginal$TradeQuantity_acc+1)

# Generate predictions for new data
pred_transportedSpecies <- predict(m3,df_transport_avg, type="response", allow.new.levels = T)
pred_transportedSpecies_at0kg <- predict(m3,df_transport_null, type="response", allow.new.levels = T)
pred_transportedSpecies_marginal<- predict(m3,df_transport_marginal, type="response", allow.new.levels = T)

# Average fate: calculate difference between 0kg and current transported quantity
df_transport_avg$delta_transportedSpecies_avg <- pred_transportedSpecies - pred_transportedSpecies_at0kg
# Marginal fate: calculate difference between current and future transported quantity
df_transport_avg$delta_transportedSpecies_mrg <- pred_transportedSpecies_marginal - pred_transportedSpecies

df_transport_avg$FF_mrg<-NA
df_transport_avg$FF_avg<-NA

# Average fate factor: weighted by present species over transported quantity and time
df_transport_avg$FF_avg<-(df_transport_avg$delta_transportedSpecies_avg/df_transport_avg$speciesPool_i)/
  (df_transport_avg$TradeQuantity_acc/length(1870:2019))

# Marginal fate factor: weighted by present species over transported quantity and time
df_transport_avg$FF_mrg<-(df_transport_avg$delta_transportedSpecies_mrg/df_transport_avg$speciesPool_i)/
  (annual_kg/length(2020))

# Reduce dataframe for analysis
ias_transport<-data.frame(importer=df_transport_avg$imp3ISO, 
                          exporter=df_transport_avg$exp3ISO, 
                          impCont=df_transport_avg$impCont,
                          expCont=df_transport_avg$expCont,
                          kg = df_transport_avg$TradeQuantity_acc, 
                          distance = df_transport_avg$distance, 
                          nativeSpecies = df_transport_avg$nativeSpecies,
                          speciesPool = df_transport_avg$speciesPool, 
                          speciesPool_i = df_transport_avg$speciesPool_i, 
                          observed = df_transport_avg$transportedSpecies_acc, 
                          predicted = pred_transportedSpecies,
                          transported=df_transport_avg$delta_transportedSpecies_avg,
                          transported_future=df_transport_avg$delta_transportedSpecies_mrg,
                          FF_avg = df_transport_avg$FF_avg, FF_mrg = df_transport_avg$FF_mrg)

# Save fate values
#save(ias_transport, file="Data/output/results/fate_values")
#write.csv2(ias_transport, file = "Data/output/results/fate_values.csv", row.names = F)
load("Data/output/results/fate_values")

# Add continents for figure
# Reduce dataframe
ias_transport <- subset(ias_transport, ias_transport$importer!="ANT")
ias_transport <- subset(ias_transport, ias_transport$exporter!="ANT")
ias_transport <- subset(ias_transport, ias_transport$nativeSpecies>0)
ias_transport <- ias_transport[which(!is.na(ias_transport$distance)),]

# Only consider combinations of trading partners with on average more than 100kg per year
ias_transport3<-subset(ias_transport, ias_transport$kg>=100*length(1870:2019))
#save(ias_transport3, file="Data/output/results/ias_transport3")
load(file="Data/output/results/ias_transport3")
