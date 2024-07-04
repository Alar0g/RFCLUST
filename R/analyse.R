library(roxygen2)

#' Fonction d'analyse
#'
#' anlyse la Heatmap
#'
#' @param heatmap Heatmap avec dendrograme générée par la fonction heatmap.
#' @param NbGrp le nombre de groupe que l'on souhaite analyser
#' @param Data le jeu de donnée néttoyé utilisé dans la fonction RF
#' @return Les analyses
#' @import dplyr ggplots ggplot2 GGally
#' @export


Analyse <- function(heatmap,NbGrp,Data) {

  if (!is.null(dev.list())) dev.off()                                           # On assure la bonne utilisation de ggpair

  dendroHC <- as.hclust(heatmap$rowDendrogram)                                  # Récupération du dendrograme

  Cutted <- cutree(dendroHC, NbGrp)

  Data$Groupe <- paste0("G", Cutted)                                            # Attribution des groupes issus du dendrograme aux individus.

  GGpairs_grp <- ggpairs(Data, aes(color = Groupe))                             # ggpairs sur les groupes de notre jeu de donnée.

  Sum <- summary(Data_Full)                                                     # summary du JdD

  SD <- sapply(Data, function(x) if (is.numeric(x)) sd(x, na.rm = TRUE) else NA) # Calcul des écart types pour chaque colonnes numériques


  groups <- list()                                                              #Initialisation d'une liste de dataset

  for (i in 1:NbGrp) {
    groups[[i]] <- Data[Data$Groupe == paste0("G", i), ]                        # Pour chaque groupe on stock spécifiquement les individus dans la liste de dataset.
  }
  # On pourra effectuer des analyses automatiques sur chacuns de ces groupes.


  DataGRP <- Data_Full
  return(list(GGpairs_grp, Sum, SD, DataGRP))
}
