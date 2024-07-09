

#' Fonction d'analyse
#'
#' anlyse la Heatmap
#'
#' @param heatmap Heatmap avec dendrograme générée par la fonction heatmap.
#' @param nb_grp le nombre de groupe que l'on souhaite analyser
#' @param X le jeu de donnée néttoyé utilisé dans la fonction RF
#' @return Les analyses
#' @import dplyr gplots ggplot2 GGally
#' @export


analyse <- function(heatmap,nb_grp,X) {

  if (!is.null(dev.list())) dev.off()                                           # On assure la bonne utilisation de ggpair

  dendroHC <- as.hclust(heatmap$rowDendrogram)                                  # Récupération du dendrograme

  cutted <- cutree(dendroHC, nb_grp)

  X$Groupe <- paste0("G", cutted)                                            # Attribution des groupes issus du dendrograme aux individus.

  ggpairs_grp <- ggpairs(X, aes(color = Groupe))                             # ggpairs sur les groupes de notre jeu de donnée.

  sum <- summary(X)                                                     # summary du JdD

  sd <- sapply(X, function(x) if (is.numeric(x)) sd(x, na.rm = TRUE) else NA) # Calcul des écart types pour chaque colonnes numériques


  groups <- list()                                                              #Initialisation d'une liste de dataset

  for (i in 1:nb_grp) {
    groups[[i]] <- X[X$Groupe == paste0("G", i), ]                        # Pour chaque groupe on stock spécifiquement les individus dans la liste de dataset.
  }
  # On pourra effectuer des analyses automatiques sur chacuns de ces groupes.



  return(list(ggpairs_grp, sum, sd, X))
}
