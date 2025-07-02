using RCall

# This file contains the code for figures 4, 5, 6, 7 and tables 2, 3, 4, 5 in the paper

R"""
library("dplyr")
Sigma  <- c("0.8685")
Lambda <- c("_Lambda612.84","_Lambda710.769996","_Lambda779.67996","_Lambda1175.69004")

Simulations <- c("benchmark",
                   "hretage70",
                   "hretben50",
                   "sLEhigh",
                   "TwoGroups",
                   "TwoGroupsS2",
                   "TwoGroupsS5",
                   "TwoGroupsS6")
Simu <- c("Benchmark","Exp1","Exp2","Exp3","Exp5","Exp6","Exp7","Exp8")

path <-"../results/Results_Sigma"

dfT  <- c()
for (i in 1:length(Simulations)){
  df      <- read.csv(file=paste0(path,Sigma[1],Lambda[1],"_",Simulations[i],".csv"))
  df$Simu <- Simu[i]
  dfT     <- rbind(dfT,df)
}
dfT  <- dfT %>% mutate(Age2=20+(Age-1)/12)
"""
@rget dfT;