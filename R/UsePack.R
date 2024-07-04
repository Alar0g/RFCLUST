

#' Fonction générale
#'
#' Lie les fonctions ensemble j'espère mdr
#'
#' @param Data_Raw Le jeu de donnée brut
#' @param Center Scale le jeu de donnée ?
#' @param Nb_T Nombre d'arbres pour la forêt.
#' @param K Nombre de cluster attendu par divclust.
#' @param mtry Nombre de variables prises en comptes.

#' @return la heat map et son dendrograme.
#'
#' @export
#'b


RFCLUST <- function(Data_Raw,Center = TRUE,Nb_T,K,mtry){

  Net <- clean(Data_Raw,Center)
  Quali <- Net[[1]]


  Forest <- pblapply(1:Nb_T,function(i) Arbre(Quali,K,mtry ))

  Map <- heat(Forest)

  return(Map)
}
