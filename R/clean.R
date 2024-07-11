

#' Fonction Clean
#'
#' Prépare le dataset à l'utilisation de divclust avec RF
#'
#' @inheritParams rfclust
#' @return Le jeu de donné nétoyé : seulement les colonnes quanti et seulement les colonnes quali.
#' @import dplyr
#' @export


clean <- function(X){

  X <- as.data.frame(X)                                                     # Forcer un dataframe.

  if (sum(is.na(X)) > 0) {                                                    # Supprimer les lignes avec des valeurs manquantes.
    X <- X[complete.cases(X), ]
  }

  if(is.null(rownames(X))){
    rn <- as.character(1:nrow(X))
  }

  data_num <- X[, sapply(X, is.numeric)]                                    # Supprimer les colonnes non numériques

  non_numeric_columns <- !sapply(X, is.numeric)

  if (any(non_numeric_columns)) {
    # Sélectionner les colonnes non numériques
    data_pas_num <- X[, non_numeric_columns]

    # Transformer toutes les colonnes non numériques en facteurs
    for (col in names(data_pas_num)) {
      data_pas_num[[col]] <- as.factor(data_pas_num[[col]])
    }
  } else {
    data_pas_num <- data.frame() # Retourner un dataframe vide si toutes les colonnes sont numériques
  }





  R = list("quanti" = data_num, "quali" = data_pas_num)
  return(R)
}


