


RF <- function(Data,K,mtry){


  nb_col = ncol(Data)
  Cut_Var <- sample(1:nb_col,size = mtry)
  Data <- Data[,Cut_Var,drop=F]                                               #Sample des variable

  if (mtry == 1){
    Data$Double <- Data[,1]}                                                    #Divclust a besoin de 2 colonne

  test <- sample(1:nrow(Data), size = nrow(Data), replace = TRUE) # Bootstrap
  DATA_bootstrap <- Data[test, ]
  OOB_bootstrap  <- Data[-test,]


  Div <- divclust(DATA_bootstrap, K)                                          # Divclust sur l'échantillon


  Occu <- matrix(0,nrow(Data),nrow(Data))                                     #Initialisation des matrices de stockages
  Diss <- matrix(1,nrow(Data),nrow(Data))
  Abs <- matrix(0,nrow(Data),nrow(Data))


  for(i in 2:nrow(Data)){                                                     #itération pour chaque paire d'individu
    for(j in 1:(i-1)){
      Asso <- 0
      Disso1 <- 0
      Disso2 <- 0

      for(k in 1:K){                                                          # On regarde dans chaque clusters
        X <- paste0("C", k)
        nombre <- paste0(i, " ")                                              # Besoin de l'espace pour ignorer les doublons à cause  de divclust
        nombre2 <- paste0(j, " ")
        chaine <- Div$clusters[[X]]

        if (any(grepl(paste0("\\b", nombre, "\\b"), chaine)) && any(grepl(paste0("\\b", nombre2, "\\b"), chaine))) {  #analyse de la chaine de charactère que retourne divclust
          Asso <- 1
          Occu[i,j] = 1
          #print("OCCU")
        }
        if ( k == K && Asso == 0){
          Diss[i,j]= 1
        }

      }
    }
  }

  for( Z in 1:nrow(OOB_bootstrap)){                                           # On regarde les OOB pour signifier les absences.

    id_oob = OOB_bootstrap[Z,]
    Row_Nam = rownames(id_oob)


    Abs[as.integer(Row_Nam),1:(as.integer(Row_Nam)-1)] = 1                    # Toute les paires associé à l'individu qui est OOB sont donc absentes.
    Abs[(as.integer(Row_Nam)-1):nrow(Abs),as.integer(Row_Nam)] = 1
  }

  Diss = Diss - Abs - Occu                                                    # Avec les absences et les associations, facile de voir les paires dissociées.

  OUT <- list(Occu,Diss,Abs)                                                  # Retourne la liste des 3 matrices
  return(OUT)                                                                 #( On s'intéresse surtout à Occu pour la heatmap de la matrice de similarité.)

}
