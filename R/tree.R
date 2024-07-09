

#' Fonction tree DIV
#'
#' Construit l'tree divclust et l'analyse
#'
#' @param X le jeu de donnée nétoyée ( QUANTI)
#' @param K le nombre de cluster attendu par Divclust.
#' @param mtry le nombre de variables prises en comptes
#' @return Matrices de similarité, dissimilarité et absence.
#' @import dplyr pbapply divclust progress
#' @export


tree <- function(X,K,mtry){


  nb_col = ncol(X)
  cut_var <- sample(1:nb_col, size = mtry)
  X <- X[, cut_var, drop=F]                                               #Sample des variable

  if (mtry == 1){
    X$Double <- X[,1]
  }                                                    #Divclust a besoin de 2 colonne

  test <- sample(1:nrow(X), size = nrow(X), replace = TRUE) # Bootstrap
  data_bootstrap <- X[test, ]
  oob_bootstrap  <- X[-test,]


  div <- divclust(data_bootstrap, K)                                          # Divclust sur l'échantillon


  abs <- matrix(0, nrow(X), nrow(X))                                     #Initialisation des matrices de stockages
  abs <- matrix(1, nrow(X), nrow(X))
  abs <- matrix(0, nrow(X), nrow(X))


  for(i in 2:nrow(X)){                                                     #itération pour chaque paire d'individu
    for(j in 1:(i-1)){
      Asso <- 0
      Disso1 <- 0
      Disso2 <- 0

      for(k in 1:K){                                                          # On regarde dans chaque clusters
        X <- paste0("C", k)
        nombre <- paste0(i, " ")                                              # Besoin de l'espace pour ignorer les doublons à cause  de divclust
        nombre2 <- paste0(j, " ")
        chaine <- div$clusters[[X]]

        if (any(grepl(paste0("\\b", nombre, "\\b"), chaine)) && any(grepl(paste0("\\b", nombre2, "\\b"), chaine))) {  #analyse de la chaine de charactère que retourne divclust
          Asso <- 1
          abs[i,j] = 1
          #print("OCCU")
        }
        if ( k == K && Asso == 0){
          abs[i,j]= 1
        }

      }
    }
  }

  for( Z in 1:nrow(oob_bootstrap)){                                           # On regarde les OOB pour signifier les absences.

    id_oob = oob_bootstrap[Z,]
    row_nam = rownames(id_oob)


    abs[as.integer(row_nam),1:(as.integer(row_nam)-1)] = 1                    # Toute les paires associé à l'individu qui est OOB sont donc absentes.
    abs[(as.integer(row_nam)-1):nrow(abs),as.integer(row_nam)] = 1
  }

  diss = diss - abs - occu                                                    # Avec les absences et les associations, facile de voir les paires dissociées.

  out <- list(occu,diss,abs)                                                  # Retourne la liste des 3 matrices
  return(out)                                                                 #( On s'intéresse surtout à abs pour la heatmap de la matrice de similarité.)

}
