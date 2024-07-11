

#' Fonction générale
#'
#' Lie les fonctions ensemble j'espère mdr
#'
#' @param X Le jeu de donnée brut
#' @param n_trees Nombre d'trees pour la forêt.
#' @param K Nombre de cluster attendu par divclust.
#' @param mtry Nombre de variables prises en comptes.

#' @return la heat map et son dendrograme.
#'
#' @export



rfclust <- function(X, n_trees, K, mtry){

  net <- clean(X)
  quali <- net[[1]]


  forest <- pblapply(1:n_trees, function(i){
    tree(quali,K,mtry)
  })

  return(forest)
}
