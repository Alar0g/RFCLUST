



load_packages <- function(packages) {sapply(packages, require, character.only = TRUE)}
packages <- c("roxygen2","gplots","ggplot2", "dplyr", "tidyverse","palmerpenguins","progress","pbapply","divclust","GGally","FactoMineR","factoextra")
load_packages(packages)

#tes


clean <- function(Dat, centr = TRUE){

  Dat <- as.data.frame(Dat)                                                     # Forcer un dataframe.

  if (sum(is.na(Dat)) > 0) {                                                    # Supprimer les lignes avec des valeurs manquantes.
    Dat <- Dat[complete.cases(Dat), ]
  }



  if( centr == TRUE){
    #Dat <- scale(Dat, center = TRUE,)
    Dat <- as.data.frame(Dat)

    rownames(Dat) <- NULL
    rownames(Dat) <- seq_len(nrow(Dat))
    rownames(Dat) <- paste0(rownames(Dat), " ")                                 # Renommer les lignes pour les différencier des .1
  }
  else{
    rownames(Dat) <- NULL
    rownames(Dat) <- seq_len(nrow(Dat))
    rownames(Dat) <- paste0(rownames(Dat), " ")                                 # Renommer les lignes pour les différencier des .1 à cause du bootstrap et Divclust
  }

  Data_num <- Dat[, sapply(Dat, is.numeric)]                                    # Supprimer les colonnes non numériques

  non_numeric_columns <- !sapply(Dat, is.numeric)

    if (any(non_numeric_columns)) {
      # Sélectionner les colonnes non numériques
      Data_pas_num <- Dat[, non_numeric_columns]

      # Transformer toutes les colonnes non numériques en facteurs
      for (col in names(Data_pas_num)) {
        Data_pas_num[[col]] <- as.factor(Data_pas_num[[col]])
      }
    } else {
      Data_pas_num <- data.frame() # Retourner un dataframe vide si toutes les colonnes sont numériques
    }





    R = list(Data_num,Data_pas_num)
  return(R)
}


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



heat <- function(Forest ){

  Matrices_Asso <- lapply(Forest,'[[',1)
  Som_Asso <- Reduce('+',Matrices_Asso)
  class(Matrices_Asso)
  Som_Asso <- as.data.frame(Som_Asso)                                           # SOMME de toutes les matrices asso

  Matrices_Diss <- lapply(Forest,'[[',2)
  Som_Diss <- Reduce('+',Matrices_Diss)
  Som_Diss <- as.data.frame(Som_Diss)                                           # SOMME de toutes les matrices diss

  addi <- list(Som_Diss,Som_Asso)
  appartion_paire <- Reduce('+',addi)                                           #Somme des occurence des paires dans les forêts.

  #dim(appartion_paire)[1]

  Matrice_Finale <- Som_Asso
  sum(is.na(Matrice_Finale))

  if(dim(Matrice_Finale)[1] == dim(appartion_paire)[1] && dim(Matrice_Finale)[2] == dim(appartion_paire)[2]){ # Verifie que les matrices correspondes.
    for( i in 1:nrow(Matrice_Finale)){
      for(j in 1:i){
        if( j == i ){
          Matrice_Finale[i,j] = 1                                               #Diago = 1
        }
        else{
          if(appartion_paire[i,j]== 0){                                         # Reste à pas d'intéraction.
            Matrice_Finale[i,j]=0
          }
          else{
            A = Matrice_Finale[i,j]                                             # Divise le nombre de similaire par le nombre d'occurence
            Matrice_Finale[i,j] = A / appartion_paire[i,j]
          }
        }
      }
    }
  }

  Matrice_Finale <- as.matrix(Matrice_Finale)                                   # force la matrice au cas ou
  Matrice_Sym = Matrice_Finale + t(Matrice_Finale) - diag(diag(Matrice_Finale)) # passe en matrice carrée



  MAPTEST <- heatmap(Matrice_Sym,Colv = T,Rowv = T, symm = T)                       # génère la heatmap à partir de la matrice carrée

  if (!is.null(dev.list())) {dev.off()}
  graphics.off()                                                                # pour assurer qu'il n'y ait pas de conflit avec la palette.

  my_palette <- colorRampPalette(c("lightyellow1", "yellow",'orange2','red3', "red4"))(n = 299)
  # palette + la HT.

  MAP  <- heatmap.2(as.matrix(Matrice_Sym),
                    trace = "none",
                    symkey = F,
                    symm = T,
                    dendrogram = "column",
                    scale = "none",
                    col = my_palette)

  return(list(MAP,MAPTEST,Matrice_Sym))
}


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


D = penguins
C = clean(D,T)
str(C[[1]])

Data_Full <- C[[1]]

str(Data_Full)
nb_tree <- 5

Forest <- pblapply(1:nb_tree,function(i) RF(Data_Full,2,1 ))

Heattest <-heat(Forest)

M <- Heattest[[1]]

Final <- Analyse(M,4,Data_Full)
Final[[1]]
Final[[2]]
Final[[3]]
Final[[4]]
