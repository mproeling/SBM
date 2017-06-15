###################################
##
##  Oxford University
##  Mark Patrick Roeling
##  May 2017
##
###################################
rm(list = ls(all = TRUE))
library(igraph)
library(blockmodels)
# purpose of this script is to create the SBM covariates for every node

setwd("/Users/mproeling/Downloads/ISOT_Botnet_DataSet_2010/")

input.data = read.table("simulationdata_edges_r.csv", sep = ";", h = T)
subdata.matrix.combined = as.matrix(get.adjacency(graph.edgelist(as.matrix(input.data), directed=TRUE)))

output.combined = BM_bernoulli("SBM", subdata.matrix.combined,
                               verbosity=6,
                               plotting=character(0),
                               exploration_factor=1.5,
                               explore_min=2,
                               explore_max=10,
                               autosave = 'SBMsimulation',
                               ncores=2)

#output.combined = readRDS("SBMsimulation")
output.combined$estimate()
which.max(output.combined$ICL)
Q = which.max(output.combined$ICL)

# get best model
best.model = output.combined$memberships[[which.max(output.combined$ICL)]]
best.model$plot()
output.combined$plot_parameters(Q)
# memberships 
day1.memberships = as.data.frame(cbind(row.names(subdata.matrix.combined), best.model$Z))
write.table(day1.memberships, "simulation_postprob.csv", sep = ";", quote = F, col.names = F, row.names = F)

day1.memberships$class = 0
# get class membership based on highest posterior probability to belong to class Q
for(i in 1:nrow(day1.memberships)){
  point =  as.data.frame(cbind(seq(1,Q,1), t(day1.memberships[i, c(2:(Q+1))])))
  point = point[order(point[,2], decreasing = T),]
  day1.memberships[i,]$class = as.integer(paste(point[1,1]))
}
rm(point)

# adjust to Q
colnames(day1.memberships) = c("Id", "Q1", "Q2", "Q3", "Q4", "Q5", "class")

write.table(day1.memberships, "simulation_postprob.csv", sep = ";", quote = F, col.names = T, row.names = F)

day1.memberships = read.table("simulation_postprob.csv", sep = ";", h = T)

