#' Fit SBM Model
#'
#' @param list Network List
#' @param n_SBM Number of SBM models to fit. Default is 10. The top half (rounding up) are retained
#'  and averaged to produce a probability matrix.
#' @param G The number of groups to divide the top layer and the focal layer into.
#'
#' @return Network list with 'SBM_ProbsMat', a matrix of probabilities assigned to each possible interaction,
#'  'SBM1', the best model fit derived from Optimise_SBM(), and 'SBM_G', the number of fitted groups.
#' @export
#'
FitSBM <- function(list, n_SBM= 10, G=NULL){

  A = (list$obs>0)*1

  if(is.null(G)){
    G<- max(2,floor(min(sqrt(dim(A)))))
  }

  SBMs <-  map(1:n_SBM, Optimise_SBM, A= A, G=G)
  # Find Best
  SBM_fits <- unlist(transpose(SBMs)$LogLik)
  Best_SBMs<-which(rank(SBM_fits)> n_SBM/2)

  ProbMatrix<- matrix(0,dim(A)[1], dim(A)[2] )
  for( i in Best_SBMs){
    ProbMatrix <-ProbMatrix+(SBMs[[i]]$Omega_rs[SBMs[[i]]$SB_H, SBMs[[i]]$SB_W])
  }


  list$SBM_ProbsMat<- ProbMatrix
  list$SBM_G<- G
  list$SBM1<-SBMs[[which.max(SBM_fits)]]
  return(list)
}
