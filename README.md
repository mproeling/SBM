# SBM

These files belong the SBM project of Roeling and Nicholls and are used for the simulation study.

The R script reads in the edgelist [simulationdata_edges_r.csv] and generates two files: 

1. simulation_postprob.csv = the file with the posterior probability to belong to a class for every node. Note that there is an extra variable called labels, with the original labels, just for convenience, this is not normally generated by R but can be created by merging the output with the node label file [simulation_nodelist_R.csv]. I have already merged it on the fly just for your convenience. 

2. SBMsimulation = the complete SBM output from the blockmodels package. Even without the raw data this can be read in R with the readRDS("SBMsimulation") function (R script line 27). So you can check the raw output.


simulationdata_edges_r.csv (edge file), simulation_nodelist_R.csv (node file), and simulation_postprob.csv (node file) can be read in by Gephi (graph software) to visualize the network. All variable names are already prepared for easy read-in. The .png files are the plots generated by Gephi for the raw labels and class membership models.



