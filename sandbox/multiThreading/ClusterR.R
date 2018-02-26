library(ClusterR)

data(mushroom)

X = mushroom[, -1]

y = as.numeric(mushroom[, 1])            # convert the labels to numeric

gwd = FD::gowdis(X)           # calculate the 'gower' distance for the factor variables

gwd_mat = as.matrix(gwd)                 # convert the distances to a matrix

cm = Cluster_Medoids(gwd_mat, clusters = 2, swap_phase = TRUE, verbose = F)
