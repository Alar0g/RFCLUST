
#'
#' Merge toutes les matrices
#'
#' @param x le résultats de la fonction apply de sur la fonction RF ( tree )
#' @param ... more parameters if required
#' @return Fourni les 2 type de heatmap + la matrice de similarité cumulée de la RF
#' @import dplyr gplots ggplot2 GGally
#' @export


sum.rfclust <- function(x, ...){

  matrices_asso <- lapply(x,'[[',1)
  som_asso <- Reduce('+',matrices_asso)
  class(matrices_asso)
  som_asso <- as.data.frame(som_asso)                                           # SOMME de toutes les matrices asso

  matrices_diss <- lapply(x,'[[',2)
  som_diss <- Reduce('+',matrices_diss)
  som_diss <- as.data.frame(som_diss)                                           # SOMME de toutes les matrices diss

  addi <- list(som_diss,som_asso)
  appartion_paire <- Reduce('+',addi)                                           #Somme des occurence des paires dans les forêts.


  matrice_finale <- som_asso
  sum(is.na(matrice_finale))


  if(dim(matrice_finale)[1] == dim(appartion_paire)[1] && dim(matrice_finale)[2] == dim(appartion_paire)[2]){ # Verifie que les matrices correspondes.
    for( i in 1:nrow(matrice_finale)){

            A = matrice_finale[i,j]
            matrice_finale[i,j] = A / appartion_paire[i,j]
    }
  }

  # Diag must be 1
  diag[matrice_finale] <- 1

  # Force .matrix in case of bug
  matrice_finale <- as.matrix(matrice_finale)


  class(matrice_finale) <- "rfclust"

  #############

  sum <- summary(X)                                                     # summary du JdD

  sd <- sapply(X, function(x) if (is.numeric(x)) sd(x, na.rm = TRUE) else NA) # Calcul des écart types pour chaque colonnes numériques



  return(list(matrice_finale = "Matrix", sum = "Summary", sd = "Standard Deviation"))

  }

