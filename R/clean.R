

#' Fonction Clean
#'
#' Prépare le dataset à l'utilisation de divclust avec RF
#'
#' @inheritParams RFCLUST
#' @return Le jeu de donné nétoyé : seulement les colonnes quanti et seulement les colonnes quali.
#' @import dplyr
#' @export


clean <- function(X, center = TRUE){

  X <- as.data.frame(X)                                                     # Forcer un dataframe.

  if (sum(is.na(X)) > 0) {                                                    # Supprimer les lignes avec des valeurs manquantes.
    X <- X[complete.cases(X), ]
  }



  if( center == TRUE){
    #X <- scale(X, center = TRUE,)
    X <- as.data.frame(X)

    rownames(X) <- NULL
    rownames(X) <- seq_len(nrow(X))
    rownames(X) <- paste0(rownames(X), " ")                                 # Renommer les lignes pour les différencier des .1
  }
  else{
    rownames(X) <- NULL
    rownames(X) <- seq_len(nrow(X))
    rownames(X) <- paste0(rownames(X), " ")                                 # Renommer les lignes pour les différencier des .1 à cause du bootstrap et Divclust
  }

  Data_num <- X[, sapply(X, is.numeric)]                                    # Supprimer les colonnes non numériques

  non_numeric_columns <- !sapply(X, is.numeric)

  if (any(non_numeric_columns)) {
    # Sélectionner les colonnes non numériques
    Data_pas_num <- X[, non_numeric_columns]

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


