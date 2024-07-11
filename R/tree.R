

#' Fonction tree DIV
#'
#' Construit l'tree divclust et l'analyse
#'
#' @param X le jeu de donnée nétoyée ( QUANTI)
#' @param K le nombre de cluster attendu par Divclust.
#' @param mtry le nombre de variables prises en comptes
#' @return Matrices de similarité, dissimilarité et absentence.
#' @import dplyr pbapply divclust progress
#' @export


tree <- function(X, K, mtry){


  nb_col = ncol(X)
  cut_var <- sample(1:nb_col, size = mtry)
  X <- X[, cut_var, drop=F]                                               #Sample des variable

  if (mtry == 1){
    X$Double <- X[,1]
  }                                                    #Divclust a besoin de 2 colonne

  test <- sample(1:nrow(X), size = nrow(X), replace = TRUE) # Bootstrap
  data_bootstrap <- X[test, ]
  oob_bootstrap  <- X[-test,]


  div <- divclust::divclust(data_bootstrap, K)                                          # Divclust sur l'échantillon

  rn <- rownames(X)

  occu <- matrix(0, nrow(X), nrow(X), dimnames = list(rn, rn))                                     #Initialisation des matrices de stockages
  absent <- matrix(0, nrow(X), nrow(X), dimnames = list(rn, rn))


  clus_indiv_unik <- sapply(div$clusters, function(x){unique(sapply(strsplit(x, ".", fixed = TRUE), "[", 1))})


  # On regarde dans chaque clusters

  for(k in 1:K){
    for(i in clus_indiv_unik[k]){
      for(j in clus_indiv_unik[k]){
        occu[i,j] <- 1
      }
    }

  }

  # On regarde les OOB pour signifier les absentences.
  for(Z in rownames(oob_bootstrap)){
    absent[Z, ] <-  1                    # Toute les paires associé à l'individu qui est OOB sont donc absententes.
    absent[, Z] <-  1
  }

  diss <-  1 - absent - occu                                                    # Avec les absentences et les associations, facile de voir les paires dissociées.

  # Retourne la liste des 3 matrices
  #( On s'intéresse surtout à absent pour la heatmap de la matrice de similarité.)
  out <- list("occu" = occu, "diss" = diss, "absent" = absent)
  return(out)

}
