#' Fit Latent Trait (Matching Model)
#'
#' Repeatedly fits a latent trait model to a binary interaction network to return a probability matrix
#'
#' The optimiser is started at values derived from the row-sums and column-sums of a CCA analysis,
#' which correspond closely to latent traits by matching closely related species together.
#'
#' The k2 and lambda parameters are started from points drawn from a uniform distribution 0:1.
#'
#' @param list Network List
#' @param N_runs Number of start points for k2 and lambda to try. The best (maximum likelihood) half will
#'  be used to construct the probability matrix
#' @param maxit Default = 10'000
#' @param method Passed to optim, default = 'Nelder-Mead'
#' @param ExtraSettings Other control settings to pass to optim()
#'
#' @return Network list with added 'M_par',the best fitting parameters, 'M_ProbsMatrix', the probability matrix
#' @export
#'
#'
FitMatching<-  function(list,N_runs=10, maxit = 10000, method='Nelder-Mead', ExtraSettings=NULL){

  A <- list$obs>0
  N1 <- dim(A)[1]
  N2 <- dim(A)[2]
  N= N1+N2

  CCA<- vegan::cca(A)
  StartPointM<- c(scale(CCA$rowsum), scale(CCA$colsum))

  ListOfFits<-map(1:N_runs, Optimiser, A=A, N_p=0, maxit=maxit,  method=method,  ExtraSettings=ExtraSettings, fixedSt_P=StartPointM, N_unif_P=2, func= Match_LogLikFunc)
  #  Best_M<- PerfectBest(ListOfFits)

  Best_Fits<-which(rank(map_dbl(ListOfFits, 'value')) >= max(1,N_runs/2) )   # Find Best
  ProbMatrix<- matrix(0,dim(A)[1], dim(A)[2] )

  for( i in Best_Fits){
    p = ListOfFits[[i]]$par
    best_m1V = rep(p[1:N1],N2)
    best_m2V = rep(p[(N1+1):N],each=N1)
    best_k2 = p[N+1]
    best_lambda = abs(p[N+2])
    Probs_M =  boot::inv.logit(  -best_lambda*((best_m1V - best_m2V)^2) + best_k2)
    ProbsMatrix_M = matrix(signif(Probs_M,6), ncol=N2)
    ProbMatrix <-ProbMatrix+ProbsMatrix_M
  }

  list$A<-A
  list$N1<- N1
  list$N2<- N2
  list$N  <- N

  Best_i<-which.min(map_dbl(ListOfFits, 'value'))
  Best_Pars <- ListOfFits[[Best_i]]$p
  Best_Pars[N+2] =  abs(Best_Pars[N+2]) # Make lambda absolute
  names(Best_Pars) <- c(paste0('M1_', 1:N1),  paste0('M2_', 1:N2), 'M_k2', 'M_lam' )

  list$M_par <- Best_Pars
  list$M_ProbsMatrix <- ProbMatrix
  return(list)
}



Match_LogLikFunc <- function(p,A){

  N1 <- dim(A)[1]
  N2 <- dim(A)[2]
  N= N1+N2

  m1V = rep(p[1:N1],N2)
  m2V = rep(p[(N1+1):N],each=N1)

  k2 = p[N+1] # intercept
  lambda = abs(p[N+2])# lambda to scale diffs

  Probs =  boot::inv.logit(-lambda* ((m1V - m2V)^2) + k2)

  a_ij_V<-as.vector(A)

  ## Prior trait distribution pulls towards middle:
  ## Cauchy distribution with scale =2
  TraitPriors <-  dcauchy(c(m1V,m2V),scale = 2, log=TRUE)

  LogLiks =     a_ij_V*log(Probs)  + (1- a_ij_V)*(log(1-Probs)  )
  NegLogLik = -sum(LogLiks) - sum(TraitPriors)
  return(NegLogLik)
}

