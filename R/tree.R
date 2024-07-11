

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

  cut_var <- sample(1:ncol(X), size = mtry)
  #variable sample
  Xtree <- X[, cut_var, drop=FALSE]

  rn <- rownames(X)
  #observation bootstrap
  index_boot <- sample(1:nrow(Xtree), size = nrow(Xtree), replace = TRUE)
  oob <- rn[-unique(index_boot)]
  X_ib <- Xtree[index_boot, , drop=FALSE]
  #rownames(X_ib) <- make.unique(rn[index_boot])

  # divclust tree on in-bag bootstrap sample
  div <- divclust(X_ib, K)


  occu <- matrix(0, nrow(X), nrow(X), dimnames = list(rn, rn))                                     #Initialisation des matrices de stockages
  absent <- matrix(0, nrow(X), nrow(X), dimnames = list(rn, rn))


  clus_indiv_unik <- sapply(div$clusters,
                            function(x){
                              unique(sapply(strsplit(x, ".", fixed = TRUE), "[", 1))
                            }
  )


  # Co-clustering occurences in each cluster
  for(k in 1:K){
    for(i in clus_indiv_unik[k]){
      for(j in clus_indiv_unik[k]){
        occu[i,j] <- 1
      }
    }

  }

  # On regarde les OOB pour signifier les absences.
  # Toute les paires associé à l'individu qui est OOB sont donc absentes.
  absent[oob, ] <-  1
  absent[, oob] <-  1

  diss <-  1 - absent - occu                                                    # Avec les absences et les associations, facile de voir les paires dissociées.

  # Retourne la liste des 3 matrices
  #( On s'intéresse surtout à absent pour la heatmap de la matrice de similarité.)
  out <- list("occu" = occu, "diss" = diss, "absent" = absent)
  return(out)

}
