#' Foction Heatmap
#'
#' Merge toutes les matrices et efectue la Heatmap sur la matrice de similarité.
#'
#' @param forest le résultats de la fonction apply de sur la fonction RF ( tree )
#' @return Fourni les 2 type de heatmap + la matrice de similarité cumulée de la RF
#' @import dplyr gplots ggplot2 GGally
#' @export



heat <- function(forest ){

  matrices_asso <- lapply(forest,'[[',1)
  som_asso <- Reduce('+',matrices_asso)
  class(Matrices_Asso)
  som_asso <- as.data.frame(som_asso)                                           # SOMME de toutes les matrices asso

  matrices_diss <- lapply(forest,'[[',2)
  som_diss <- Reduce('+',matrices_diss)
  som_diss <- as.data.frame(som_diss)                                           # SOMME de toutes les matrices diss

  addi <- list(som_diss,som_dsso)
  appartion_paire <- Reduce('+',addi)                                           #Somme des occurence des paires dans les forêts.

  #dim(appartion_paire)[1]

  matrice_finale <- som_asso
  sum(is.na(matrice_finale))

  if(dim(matrice_finale)[1] == dim(appartion_paire)[1] && dim(matrice_finale)[2] == dim(appartion_paire)[2]){ # Verifie que les matrices correspondes.
    for( i in 1:nrow(matrice_finale)){
      for(j in 1:i){
        if( j == i ){
          matrice_finale[i,j] = 1                                               #Diago = 1
        }
        else{
          if(appartion_paire[i,j]== 0){                                         # Reste à pas d'intéraction.
            matrice_finale[i,j]=0
          }
          else{
            A = matrice_finale[i,j]                                             # Divise le nombre de similaire par le nombre d'occurence
            matrice_finale[i,j] = A / appartion_paire[i,j]
          }
        }
      }
    }
  }

  matrice_finale <- as.matrix(matrice_finale)                                   # force la matrice au cas ou
  matrice_sym = matrice_finale + t(matrice_finale) - diag(diag(matrice_finale)) # passe en matrice carrée



  maptest1 <- heatmap(matrice_sym,Colv = T,Rowv = T, symm = T)                       # génère la heatmap à partir de la matrice carrée

  if (!is.null(dev.list())) {dev.off()}
  graphics.off()                                                                # pour assurer qu'il n'y ait pas de conflit avec la palette.

  my_palette <- colorRampPalette(c("lightyellow1", "yellow",'orange2','red3', "red4"))(n = 299)
  # palette + la HT.

  map  <- heatmap.2(as.matrix(matrice_sym),
                    trace = "none",
                    symkey = F,
                    symm = T,
                    dendrogram = "column",
                    scale = "none",
                    col = my_palette)

  return(list(map, maptest1, matrice_sym))
}

