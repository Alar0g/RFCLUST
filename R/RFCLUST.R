

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
#' @importFrom pbapply pblapply
#' @importFrom parallel detectCores
#'
#' @export



rfclust <- function(X, n_trees = 500, K = 2, mtry = 1, ncores = parallel::detectCores()-1){

  X_clean <- clean(X)

  forest <- pblapply(1:n_trees, function(i){
    tree(cbind(X_clean$quanti, X_clean$quali), K, mtry)
  }, cl = ncores)

  class(forest) <- "rfclust"

  return(forest)
}
