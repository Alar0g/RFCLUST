#' Foction Heatmap
#'
#' Merge toutes les matrices et efectue la Heatmap sur la matrice de similarité.
#'
#' @param Forest le résultats de la fonction apply de sur la fonction RF ( tree )
#' @return Fourni les 2 type de heatmap + la matrice de similarité cumulée de la RF
#' @import dplyr gplots ggplot2 GGally
#' @export



heat <- function(Forest ){

  Matrices_Asso <- lapply(Forest,'[[',1)
  Som_Asso <- Reduce('+',Matrices_Asso)
  class(Matrices_Asso)
  Som_Asso <- as.data.frame(Som_Asso)                                           # SOMME de toutes les matrices asso

  Matrices_Diss <- lapply(Forest,'[[',2)
  Som_Diss <- Reduce('+',Matrices_Diss)
  Som_Diss <- as.data.frame(Som_Diss)                                           # SOMME de toutes les matrices diss

  addi <- list(Som_Diss,Som_Asso)
  appartion_paire <- Reduce('+',addi)                                           #Somme des occurence des paires dans les forêts.

  #dim(appartion_paire)[1]

  Matrice_Finale <- Som_Asso
  sum(is.na(Matrice_Finale))

  if(dim(Matrice_Finale)[1] == dim(appartion_paire)[1] && dim(Matrice_Finale)[2] == dim(appartion_paire)[2]){ # Verifie que les matrices correspondes.
    for( i in 1:nrow(Matrice_Finale)){
      for(j in 1:i){
        if( j == i ){
          Matrice_Finale[i,j] = 1                                               #Diago = 1
        }
        else{
          if(appartion_paire[i,j]== 0){                                         # Reste à pas d'intéraction.
            Matrice_Finale[i,j]=0
          }
          else{
            A = Matrice_Finale[i,j]                                             # Divise le nombre de similaire par le nombre d'occurence
            Matrice_Finale[i,j] = A / appartion_paire[i,j]
          }
        }
      }
    }
  }

  Matrice_Finale <- as.matrix(Matrice_Finale)                                   # force la matrice au cas ou
  Matrice_Sym = Matrice_Finale + t(Matrice_Finale) - diag(diag(Matrice_Finale)) # passe en matrice carrée



  MAPTEST <- heatmap(Matrice_Sym,Colv = T,Rowv = T, symm = T)                       # génère la heatmap à partir de la matrice carrée

  if (!is.null(dev.list())) {dev.off()}
  graphics.off()                                                                # pour assurer qu'il n'y ait pas de conflit avec la palette.

  my_palette <- colorRampPalette(c("lightyellow1", "yellow",'orange2','red3', "red4"))(n = 299)
  # palette + la HT.

  MAP  <- heatmap.2(as.matrix(Matrice_Sym),
                    trace = "none",
                    symkey = F,
                    symm = T,
                    dendrogram = "column",
                    scale = "none",
                    col = my_palette)

  return(list(MAP, MAPTEST, Matrice_Sym))
}

