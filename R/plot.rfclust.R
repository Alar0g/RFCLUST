#' Foction Heatmap
#'
#' Merge toutes les matrices et efectue la Heatmap sur la matrice de similarité.
#'
#' @param x le résultats de la fonction apply de sur la fonction RF ( tree )
#' @param ... other argumetns to be passed to and from other methods.

#' @return Fourni les 2 type de heatmap + la matrice de similarité cumulée de la RF
#' @import dplyr gplots ggplot2 GGally
#' @importFrom grDevices colorRampPalette
#' @importFrom stats as.hclust complete.cases cutree
#' @export



plot.rfclust <- function(x, ...){

  stopifnot(is.matrix(x))

  my_palette <- colorRampPalette(c("lightyellow1", "yellow",'orange2','red3', "red4"))(n = 299)


  map  <- heatmap.2(as.matrix(matrice_sym),
                    trace = "none",
                    symkey = F,
                    symm = T,
                    dendrogram = "column",
                    scale = "none",
                    col = my_palette,
                    ...)

  return(map = "Heatmap")
}

