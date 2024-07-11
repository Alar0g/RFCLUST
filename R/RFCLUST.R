

#' Fonction générale
#'
#' Lie les fonctions ensemble j'espère mdr
#'
#' @param X a data.frame to be clustered
#' @param n_trees number of \code{divlust} tress. Default is \code{500}.
#' @param K Nombre de cluster attendu par divclust.
#' @param mtry Nombre de variables prises en comptes.

#' @return la heat map et son dendrograme.
#'
#' @importFrom pbapply pblapply
#' @importFrom parallel detectCores
#'
#' @export



rfclust <- function(X, n_trees = 500, K = 2, mtry = 1, ncores = parallel::detectCores()-1){


  stopifnot(is.data.frame(X))

  if(is.null(rownames(X))){
    X <- as.character(1:nrow(X))
    message("No rownames for `X`. using row number instead.")
  }

  forest <- pblapply(1:n_trees, function(i){
    tree(X, K, mtry)
  }, cl = ncores)

  class(forest) <- "rfclust"

  return(forest)
}
