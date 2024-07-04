library(roxygen2)

#' Fonction Clean
#'
#' Prépare le dataset à l'utilisation de divclust avec RF
#'
#' @param Dat le jeu de donnée brut initial.
#' @param centr si on veux scale le jeu oui ou non.
#' @return Le jeu de donné nétoyé : seulement les colonnes quanti et seulement les colonnes quali.
#' @import dplyr
#' @export


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


