posthoc_analysis <- function(heatmap, nb_grp, X) {

  # Recovery of the dendrogram
  dendroHC <- as.hclust(heatmap$rowDendrogram)

  cutted <- cutree(dendroHC, nb_grp)

  # Assignment of groups from the dendrogram to individuals.
  X$Group <- paste0("G", cutted)

  # ggpairs on the groups in our dataset.
  ggpairs_grp <- ggpairs(X, aes_string(color = "Group"))

  #Initializing a dataset list
  groups <- list()

  # For each group we specifically store the individuals in the dataset list.
  for (i in 1:nb_grp) {
    groups[[i]] <- X[X$Groupe == paste0("G", i), ]
  }

  return(list(ggpairs_grp, sum, sd, X))
}
