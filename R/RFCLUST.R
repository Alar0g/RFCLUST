

#' Fonction générale
#'
#' Lie les fonctions ensemble j'espère mdr
#'
#' @param X a data.frame to be clustered
#' @param ntrees number of \code{divlust} tress. Default is \code{500}.
#' @param K Nombre de cluster attendu par divclust.
#' @param mtry Nombre de variables prises en comptes.
#' @param ncores number of cpus to parallelize over. Default is
#' \code{parallel::detectCores()-1}.
#'
#' @return a list of length \code{n_trees}, each elements containing 3 matrices.
#'
#' @importFrom pbapply pblapply
#' @importFrom parallel detectCores
#' @importFrom tibble is_tibble
#'
#' @export
#'
#' @examples
#' library(palmerpenguins)
#' mypeng <- as.data.frame(penguins)
#' mypeng$year <- factor(as.character(mypeng$year),
#'                          levels=c("2007", "2008", "2009"),
#'                          ordered=TRUE)
#' set.seed(123)
#' forest_clust <- rfclust(na.omit(mypeng[mypeng$sex=="male", -c(1, 7)]), ntrees = 50, ncores = 1)
#'
#' if(interactive()){
#'  resume <- summary(forest_clust)
#'
#'  plot(resume$Matrix)
#' }
#'
#' if(interactive()){
#'    ncol <- 15
#'    nobs_pg <- 43
#'    diff_mean <- 5
#'    X <- matrix(rnorm(n = ncol*nobs_pg, mean = 0, sd = 1), ncol=ncol)
#'    Y <- matrix(rnorm(n = ncol*nobs_pg, mean = diff_mean, sd = 1), ncol=ncol)
#'    mysim_data <- rbind.data.frame(X,Y)
#'    res <- rfclust(mysim_data)
#'    s <- summary(res)
#'    plot(s)
#' }

rfclust <- function(X, ntrees = 500, K = 2, mtry = 1, ncores = parallel::detectCores()-1){

  stopifnot(is.data.frame(X))

  if(tibble::is_tibble(X)){
    X <- as.data.frame(X)
    warning("X is a tibble. It is converted to a simple `data.frame` instead.")
  }

  if(is.null(rownames(X))){
    X <- as.character(1:nrow(X))
    message("No rownames for `X`. using row number instead.")
  }

  forest <- pblapply(1:ntrees, function(i){
    tree(X, K, mtry)
  }, cl = ncores)

  class(forest) <- "rfclust"
  message("We advise you do use the `summary()` on this object to agregate the result of this forest, before plotting the summary itself.")

  return(forest)
}
