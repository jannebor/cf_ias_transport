library(stringr)
library(doSNOW)
library(xlsx)

setwd("")
baci_conversion<-xlsx::read.xlsx("Data/input/BACI/baci_conversion.xlsx", 
                                 sheetIndex = 1)

for(y in 1995:2019){
  
  print(y)
  baci<-read.csv(paste("Data/input/ignore/BACI_HS92_V202102/BACI_HS92_Y",y,"_V202102.csv",sep=""))
  baci$k<-as.character(baci$k)
  
  baci$k[which(str_length(baci$k)<6)]<-str_c("0",baci$k[which(str_length(baci$k)<6)])
  
  baci$code<-as.character(baci$k)
  
  baci$sitc3<-NA
  
  baci$imp3ISO<-NA
  baci$exp3ISO<-NA
  baci$expCont<-NA
  baci$impCont<-NA
  

  cl <- makeCluster(8)
  registerDoSNOW(cl)
  iterations <- length(unique(baci$code))
  #iterations<-4
  pb <- txtProgressBar(max = iterations, style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  
  baci<-foreach(j = 1:iterations,.combine = "rbind",
                .options.snow = opts) %dopar% {
                  
                  baci_sub<-baci[which(baci$code==unique(baci$code)[j]),]
                  

                    baci_sub$sitc3<-baci_conversion$S3[which(baci_conversion$HS92==unique(baci$code)[j])]
                    
                    return(baci_sub)  
                    
                  
                  
                }
  
  
  stopCluster(cl)
  
  save(baci, file=paste("Data/input/ignore/BACI_HS92_V202102/converted/BACI_HS92_Y",y,"_V202102",sep=""))
  
}
