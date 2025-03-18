

# Libraries ---------------------------------------------------------------
# 
# install.packages("TraMineR")
# install.packages("TraMineRextras")

library(tidyverse)
library(TraMineR)
library(TraMineRextras)
library(WeightedCluster)
library(RColorBrewer)



# Clear everything  -------------------------------------------------------

rm(list = ls())
load(file="data_step4-out_pov-STS.Rdata")
load(file="data_step4-out_trajectories-STS.Rdata")
load(file="data_covariates-pov-sts.Rdata")
load( file="data_covariates-tra-sts.Rdata")

  

# all sequences -----------------------------------------------------------

cpal(tra.seq) <- c("#DE2D26",  "#FC9272" ,"#FEE0D2", "#006D2C", "#FFFFFF", "#F0F0F0")


# Compute OM distance with transition-based substitution costs


trans_cost <- seqsubm(tra.seq, method = "TRATE")  # Transition rate-based costs

tra.seqdist <- seqdist(tra.seq, method="OM", sm = trans_cost)#clustering using OM distance to emphasize the timing of occurence of poverty

# perform clustering using weighted cluster library 

trawc <- wcKMedRange(tra.seqdist,kvals=2:20)
trawc
plot(trawc)
table <- trawc
table <- as_data_frame(table)

write.csv(table, file="table_cluster-diagnostics-15-clusters.csv")
# plot(trawc )

# # plot results 
# 
seqdplot(tra.seq, group= trawc$clustering$cluster15)
# seqdplot(tra.seq,  sort="from.end", group= trawc$clustering$cluster )
# seqistatd(tra.seq )

# rename clusters based on results 



clustresults <- trawc$clustering$cluster15






filter <- which (clustresults == 13 |
                   clustresults == 20 |
                   clustresults == 3726 |
                   clustresults == 3734 |
                   clustresults == 3735 |
                   clustresults == 52 |
                   clustresults == 65 |
                   clustresults == 85 )
clustresults[filter] <- "Mainly missing / not observed"

seqdplot(tra.seq, group= clustresults)




clustresults[clustresults=="2123"] <- "Mainly protected poor"
filter <- which (clustresults=="244"| 
                clustresults=="42"|
                  clustresults=="47" |
                  clustresults=="630" |
                    clustresults=="706"|
                  clustresults=="3739")
clustresults[filter] <- "Mainly non-poor and missing"

seqdplot(tra.seq, group= clustresults)


clustresults[clustresults=="3712"] <- "Mainly economically vulnerable"


seqdplot(tra.seq, group= clustresults, with.legend=FALSE)

seqiplot(tra.seq, group= clustresults, with.legend=FALSE)

table(clustresults)



# Generate a dummy plot with the legend but suppress the main plot
seqdplot(tra.seq, group = clustresults, with.legend = TRUE, plot = FALSE)

plot.new()  # Create an empty plotting space
# Extract legend colors and labels
state_colors <- attr(tra.seq, "cpal")
state_labels <- attr(tra.seq, "labels")
legend("center", legend = state_labels, fill = state_colors, cex = 1.2, bty = "n")


# 
seqiplot(tra.seq,  sort="from.end", group= clustresults )
# seqmtplot(tra.seq,  sort="from.end", group= clustresults )




# poverty only ------------------------------------------------------------

cpal(pov.seq) <- c("#DE2D26",  "#FC9272" ,"#FEE0D2", "#006D2C", "#FFFFFF", "#F0F0F0")


# Compute OM distance with transition-based substitution costs


trans_cost <- seqsubm(pov.seq, method = "TRATE")  # Transition rate-based costs


pov.seqdist <- seqdist(pov.seq, method="OM", sm = trans_cost)#clustering using OM distance to emphasize the timing of occurence of poverty

pov.wc <- wcKMedRange(pov.seqdist,kvals=2:20)
pov.wc
summary(pov.wc, max.rank=2)
# plot(pov.wc )
# 
seqdplot(pov.seq, group= pov.wc$clustering$cluster16
         ,  with.legend=FALSE
         )


# 
# 
# seqistatd(pov.seq)
# 
# seqIplot(pov.seq, sort="from.end", group= pov.wc$clustering$cluster5
#          ,  with.legend=FALSE
# )

# seqiplot(pov.seq, group= pov.wc$clustering$cluster5)



# saving the data as a variable  ------------------------------------------

ids <- rownames(tra.seq) 

poverty.trajectories.clusters <- cbind(ids, clustresults)

poverty.trajectories.clusters <- as_tibble(poverty.trajectories.clusters)
names(poverty.trajectories.clusters) <- c("mergeid", "poverty.trajectories.clusters")

head(poverty.trajectories.clusters)
levels(as.factor(poverty.trajectories.clusters$poverty.trajectories.clusters))

save(poverty.trajectories.clusters, file="data_poverty.trajectories.clusters.Rdata")

