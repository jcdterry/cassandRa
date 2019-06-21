#' Fit Centrality Model
#'
#' Repeatedly fits a centrality model to a binary interaction network to return a probability matrix
#'
#' @param list Network List
#' @param N_runs Number of start points to try. The best (maximum likelihood) half will be used to construct the probability matrix
#' @param maxit Default = 10'000
#' @param method Passed to optim, default = 'Nelder-Mead'
#' @param ExtraSettings Other control settings to pass to optim()
#'
#' @return Network list with added 'C_par', best fitting parameters, C_ProbsMatrix, the probability matrix
#' @export
#'
FitCentrality<- function(list,N_runs=10, maxit = 10000, method='Nelder-Mead', ExtraSettings=NULL){

  A <- list$obs>0
  N1 <- dim(A)[1]
  N2 <- dim(A)[2]
  N= N1+N2

  ListOfFits<-map(1:N_runs, Optimiser, A=A, N_p=N, fixedSt_P=c(1), N_unif_P= 0, func= CentLogLikFunc, maxit=maxit,  method=method,  ExtraSettings=ExtraSettings)
  #  Best_C<- PerfectBest(Fits_C)

  Best_Fits<-which(rank(map_dbl(ListOfFits, 'value')) >= max(1,N_runs/2) )   # Find Best
  ProbMatrix<- matrix(0,dim(A)[1], dim(A)[2] )

  for( i in Best_Fits){
    p = ListOfFits[[i]]$par
    best_c1V = rep(p[1:N1],N2)
    best_c2V = rep(p[(N1+1):N],each=N1)
    Probs_C =  boot::inv.logit(best_c1V + best_c2V +p[N+1])
    ProbsMatrix_C = matrix(signif(Probs_C,6), ncol=N2)
    ProbMatrix <-ProbMatrix+ProbsMatrix_C
  }

  list$A<-A
  list$N1<- N1
  list$N2<- N2
  list$N  <- N

  Best_i<-which.min(map_dbl(ListOfFits, 'value'))
  Best_Pars <- ListOfFits[[Best_i]]$p
  names(Best_Pars) <- c(paste0('C1_', 1:N1),  paste0('C2_', 1:N2), 'C_k2' )
  list$C_par <- Best_Pars
  list$C_ProbsMatrix <- ProbMatrix
  return(list)
}


CentLogLikFunc <- function(p,A){

  N1 <- dim(A)[1]
  N2 <- dim(A)[2]
  N= N1+N2

  c1V = rep(p[1:N1],N2)
  c2V = rep(p[(N1+1):N],each=N1)

  ## Genetate vector of logscale probs
  Probs =  boot::inv.logit(c1V + c2V+ p[N+1])
  a_ij_V<-as.vector(A) # put results into a vector
  LogLiks =     a_ij_V*log(Probs) + (1- a_ij_V)*log(1- Probs)
  NegLogLik = -sum(LogLiks)
  return(NegLogLik)
}
