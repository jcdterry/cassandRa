#' Fit Matching-Centrality Model
#'
#' Fit a model that contains both a trait-matching and a centrality term based on Rohr et al. (2016)
#'
#' @param list Network List
#' @param N_runs Number of different start points for k2 and lambda to try. The best (maximum likelihood)
#' half will be used to construct the probability matrix
#' @param maxit Default = 10'000
#' @param method Passed to optim, default = 'Nelder-Mead'
#' @param ExtraSettings Other control settings to pass to optim()
#'
#' @return Network list with added 'B_par',the best fitting parameters, 'M_ProbsMatrix', the probability matrix
#'
#'
#'@references Rohr, R.P., Naisbit, R.E., Mazza, C. & Bersier, L.-F. (2016). Matching-centrality
#' decomposition and the forecasting of new links in networks. Proc. R. Soc. B Biol. Sci., 283, 20152702
#' @export
#'
FitBothMandC <- function(list,N_runs=10, maxit = 10000, method='Nelder-Mead', ExtraSettings=NULL){

  A <- list$obs>0
  N1 <- dim(A)[1]
  N2 <- dim(A)[2]
  N= N1+N2

  CCA<- vegan::cca(A)
  StartPointM<- c(scale(CCA$rowsum), scale(CCA$colsum))

  ListOfFits<-map(1:N_runs, Optimiser, A=A, N_p=N, fixedSt_P=StartPointM, N_unif_P=2, func= Both_LogLikFunc, maxit=maxit,  method=method,  ExtraSettings=ExtraSettings)
  #  Best_B<- PerfectBest(Fits_B)

  Best_Fits<-which(rank(map_dbl(ListOfFits, 'value')) >=  max(1,N_runs/2) )   # Find Best
  ProbMatrix<- matrix(0,dim(A)[1], dim(A)[2] )

  for( i in Best_Fits){
    p = ListOfFits[[i]]$par
    best_m1V = rep(p[1:N1],N2)
    best_m2V = rep(p[(N1+1):N],each=N1)

    best_c1V = rep(p[(N+1):(N+N1)],N2)
    best_c2V = rep(p[(N+N1+1):(N+N)],each=N1)

    best_lambda = abs(p[N+N+2])
    best_kb = p[N+N+1]

    Both_Probs =  boot::inv.logit(-best_lambda*((best_m1V - best_m2V)^2) + best_c1V + best_c2V+ best_kb)
    Both_ProbsMatrix = matrix(signif(Both_Probs,6), ncol=N2)
    ProbMatrix <-ProbMatrix+Both_ProbsMatrix
  }

  list$A<-A
  list$N1<- N1
  list$N2<- N2
  list$N  <- N

  Best_i<-which.min(map_dbl(ListOfFits, 'value'))
  Best_Pars <- ListOfFits[[Best_i]]$p
  Best_Pars[N+N+2] =  abs(Best_Pars[N+N+2])# Make lambda absolute
  names(Best_Pars) <- c(paste0('M1_', 1:N1),
                        paste0('M2_', 1:N2),
                        paste0('C1_', 1:N1),
                        paste0('C2_', 1:N2),
                        'B_k2', 'B_lam' )

  list$B_par<- Best_Pars
  list$B_ProbsMat<- ProbMatrix

  return(list)
}


Both_LogLikFunc <- function(p,A){

  N1 <- dim(A)[1]
  N2 <- dim(A)[2]
  N= N1+N2

  m1V = rep(p[1:N1],N2)
  m2V = rep(p[(N1+1):N],each=N1)

  c1V = rep(p[(N+1):(N+N1)],N2)
  c2V = rep(p[(N+N1+1):(N+N)],each=N1)

  kb = p[N+N+1] # intercept
  lambda = abs(p[N+N+2])# lambda to scale diffs

  Probs =  boot::inv.logit(-lambda*((m1V - m2V)^2) + c1V + c2V+ kb)

  a_ij_V<-as.vector(A)

  ## Prior trait distribution pulls towards middle:
  ## Cauchy distribution with scale =2
  TraitPriors <-  dcauchy(c(m1V,m2V),scale = 2, log=TRUE)


  LogLiks =     a_ij_V*log(Probs)  + (1- a_ij_V)*(log(1-Probs)  )
  NegLogLik = -sum(LogLiks) - sum(TraitPriors)
  return(NegLogLik)
}

