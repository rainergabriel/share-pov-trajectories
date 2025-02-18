

# Libraries ---------------------------------------------------------------
# 
# install.packages("TraMineR")
# install.packages("TraMineRextras")

library(tidyverse)
library(TraMineR)
library(TraMineRextras)
library(WeightedCluster)



# Clear everything  -------------------------------------------------------

rm(list = ls())
load(file="data_step4-out_pov-STS.Rdata")
load(file="data_step4-out_trajectories-STS.Rdata")
load(file="data_covariates-pov-sts.Rdata")
load( file="data_covariates-tra-sts.Rdata")



# all sequences -----------------------------------------------------------


tra.seqdist <- seqdist(tra.seq,  method="DHD")#clustering using OM distance to emphasize the timing of occurence of poverty

trawc <- wcKMedRange(tra.seqdist,kvals=2:20)
trawc
summary(trawc, max.rank=2)
plot(trawc )

seqdplot(tra.seq, group= trawc$clustering$cluster5,  with.legend=FALSE )
seqdplot(tra.seq,  sort="from.end", group= trawc$clustering$cluster11 )

seqIplot(tra.seq,  sort="from.end", group= trawc$clustering$cluster11 )
seqIplot(tra.seq,  sort="from.end", group= trawc$clustering$cluster5,  with.legend=FALSE )



# poverty only ------------------------------------------------------------



pov.seqdist <- seqdist(pov.seq, method="DHD")#clustering using OM distance to emphasize the timing of occurence of poverty

pov.wc <- wcKMedRange(pov.seqdist,kvals=2:20)
pov.wc
summary(pov.wc, max.rank=2)
plot(pov.wc )

seqdplot(pov.seq, group= pov.wc$clustering$cluster5
         ,  with.legend=FALSE
         )

seqIplot(pov.seq, sort="from.end", group= pov.wc$clustering$cluster5
         ,  with.legend=FALSE
)

seqiplot(pov.seq, group= pov.wc$clustering$cluster5)
