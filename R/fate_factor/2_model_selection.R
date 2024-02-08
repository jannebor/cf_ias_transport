# Load libraries
library(dplyr)
library(RcppRoll)
library(lme4)
library(doSNOW)
library(AICcmodavg)
library(MuMIn)
library(broom.mixed)
library(scales)

# Load prepared dataset
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

# Preparing the training dataset
df_transport_train<-df_transport[which(names(df_transport) %in% c("importingCountry","exportingCountry",
                                                                  "donor_to_recipient",
                                                                  "impCont","expCont",
                                                                  "period",
                                                                  "transportedSpecies_acc",
                                                                  "TR","D","P","S","bt",
                                                                  "proactive"))]

# Ensures that the training dataset excludes records where the importing country is the same as the exporting country.
df_transport_train <- df_transport_train[df_transport_train$importingCountry != df_transport_train$exportingCountry,]
# Removes observations with NA values from the training dataset to prepare for statistical analysis.
df_transport_train<-na.exclude(df_transport_train)

# Correlation analysis
cor(df_transport_train$TR, df_transport_train$P, method="spearman")
cor(df_transport_train$TR, df_transport_train$D, method="spearman")
cor(df_transport_train$TR, df_transport_train$S, method="spearman")
cor(df_transport_train$D, df_transport_train$P, method="spearman")
cor(df_transport_train$D, df_transport_train$S, method="spearman")
cor(df_transport_train$P, df_transport_train$S, method="spearman")
# No variables correlated

# Define all possible combinations of fixed effects
Cols <- c("TR", "D", "P", "S")
n <- length(Cols)
id <- unlist(
  lapply(1:n,
         function(i)combn(1:n,i,simplify=FALSE)
  )
  ,recursive=FALSE)
Formulas <- sapply(id,function(i)
  paste("transportedSpecies_acc ~",paste(Cols[i],collapse="+"), " + (1|period/importingCountry) + (1|period/exportingCountry)")
)
Formulas<-c(Formulas, "transportedSpecies_acc ~ 1 + (1|period/importingCountry) + (1|period/exportingCountry)")
Formulas<-unique(Formulas)
#save(Formulas, file="Data/output/intermediate/models/Formulas")
load("Data/output/intermediate/models/Formulas")

# Set up parallel environment
cl <- makeCluster(6)
registerDoSNOW(cl)
iterations <- length(Formulas)
pb <- txtProgressBar(max = iterations, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)
# Fit glmms with all possible combinations of fixed effects in parallel
mod<-foreach(y = 1:length(Formulas),.combine = "c",
             .packages = "lme4",
             .options.snow = opts) %dopar% {
               
               # Fit a generalized linear mixed-effects model (GLMM) using the negative binomial distribution
               m<-glmer.nb(Formulas[y],
                           control=glmerControl(optCtrl=list(maxfun=2000000)),
                           verbose = T,
                           nAGQ = 0,
                           data = df_transport_train)
               # The model uses a negative binomial distribution to handle overdispersion in the count data.
               # `control` adjusts the optimization control, allowing for a larger number of function evaluations (maxfun) during fitting.
               # `verbose = TRUE` enables detailed output during model fitting.
               # `nAGQ = 0` specifies the number of adaptive Gauss-Hermite quadrature points for the approximation, with 0 indicating that only the Laplace approximation is used.
               
               # Return the fitted model and append to output (mod)
               return(m)
             }
# Stop parallel backend
stopCluster(cl)
# Save list of generated models
#save(mod, file="lfs/models/mod")
load("lfs/models/mod")

# Define names based on specified formulas
Modnames<- paste(Formulas)
# Create ranking of fitted models based on AICc
z<-aictab(cand.set=mod,modnames=Modnames,second.ord=TRUE,nobs=NULL,sort=TRUE)

# Save ranking
z$AICc<-round(z$AICc, 1)
z$AICcWt<-round(z$AICcWt, 2)
write.csv(z, file="Data/output/results/aicc_model_selection.csv", row.names = F)
#save(z, file="Data/output/intermediate/models/z")
load("Data/output/intermediate/models/z")

# Select null model
m0<-mod[[length(Formulas)]]
# Select top model
m3<-mod[[which(Formulas %in% z[1,1])]]
# This line fits a model predicting accumulated transported species (transportedSpecies_acc) as a function of 
# log-transformed trade volume (TR), distance in kilometers (D), log-transformed species pool (P), and 
# log-transformed native species count (S), with random effects for period nested within importing country 
# and exporting country to account for non-independence of observations within these groups.

# Compare selected vs null model
anova(m0, m3)
# Perform an analysis of variance (ANOVA) test on the fitted model
anova(m3)
# Display a summary of the fitted model
summary(m3)
# Calculate R-squared for GLMM
r.squaredGLMM(m3)
# Computes the R-squared value for the GLMM, a measure of the proportion of variance explained by the fixed effects.

# Get slope
t<-tidy(m3,conf.int=TRUE,effects="fixed")
scientific(t$estimate)
