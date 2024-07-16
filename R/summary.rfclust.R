
#'
#' Merge all matrices and some analysis
#'
#' @param x the results of the apply function on the RF function (tree)
#' @param ... more parameters if required
#' @return Provides the cumulative similarity matrix and some analysis
#' @import dplyr gplots ggplot2 GGally
#' @export


summary.rfclust <- function(x, ...){

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

  #Basic summary
  sum <- summary(X)

  #Standard deviation for X's numerical variables
  sd <- sapply(X, function(x) if (is.numeric(x)) sd(x, na.rm = TRUE) else NA)



  return(list(matrice_finale = "Matrix", sum = "Summary", sd = "Standard Deviation"))

  }

