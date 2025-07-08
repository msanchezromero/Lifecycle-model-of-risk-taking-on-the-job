using RCall

# This file contains the code for figures 4, 5, 6, 7 and tables 2, 3, 4, 5 in the paper

ParM01 = mutable_struct( 1,SampleSize,ParU)
ParM11 = mutable_struct(11,SampleSize,ParU)

sigmaG01 = ParM01.σC
sigmaG11 = ParM11.σC
 
@rput sigmaG01
@rput sigmaG11
R"""
library("dplyr")

Sigma  <- c(paste0(sigmaG01),paste0(sigmaG11))
Lambda <- c("_Lambda612.84","_Lambda710.769996","_Lambda779.67996","_Lambda1175.69004")

Simulations <- c("benchmark",
                   "hretage70",
                   "hretben50",
                   "sLEhigh",
                   "TwoGroups",
                   "TwoGroupsS2",
                   "TwoGroupsS5",
                   "TwoGroupsS6")

Simu        <- c("Benchmark",
                    "Exp1",
                    "Exp2",
                    "Exp3",
                    "Exp5", # This simulation is the benchmark case for two skill groups
                    "Exp6", # This simulation is Exp1 with two skill groups
                    "Exp7", # This simulation is Exp2 with two skill groups
                    "Exp8") # This simulation is Exp3 with two skill groups

path <-"../results/Results_Sigma"

dfT  <- c()
for (i in 1:length(Simulations)){
  # -----------------------------------------------------------------------------------------------------------
  # Uncomment if the file is saved as CSV  
  # df      <- read.csv(file=paste0(path,ifelse(i>4,Sigma[2],Sigma[1]),Lambda[1],"_",Simulations[i],".csv"))
  # -----------------------------------------------------------------------------------------------------------
  df      <- readRDS(file=paste0(path,ifelse(i>4,Sigma[2],Sigma[1]),Lambda[1],"_",Simulations[i],".rds"))    
  df$Simu <- Simu[i]
  dfT     <- rbind(dfT,df)
}

dfT  <- dfT %>% mutate(Age2=20+(Age-1)/12)
"""
@rget dfT;