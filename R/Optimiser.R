#' Optimiser wrapper for network models
#'
#' @param i RNG Seed to set
#' @param maxit Maximum number of iterations to be passed to optim (default is 10000)
#' @param method Optimiser method to pass to optim. Default is
#' @param A Interaction Presence-Absence matrix
#' @param N_p Number of parameters to draw from a normal distribution
#' @param fixedSt_P Vector of fixed parameters to pass
#' @param N_unif_P Number of parameters to take from a uniform distribution
#' @param func Function to optimiser
#' @param ExtraSettings Additional setting to pass to control
#'
#' @return A 'fit' object form optim, with a few of the input parameters attached.
#' @export
#'
Optimiser <- function(i=NULL, maxit = 10000, method='Nelder-Mead', A, N_p, fixedSt_P= c(), N_unif_P=0, func, ExtraSettings=NULL){
  if(!is.null(i)){set.seed(i)}

  if(is.null(ExtraSettings)){
    Settings = list('maxit' = maxit )
  }else{
    Settings = c(list('maxit' = maxit ), ExtraSettings)
  }


  fit<- optim(par = c(rnorm(N_p),
                      fixedSt_P,
                      runif(N_unif_P)),
              fn= func,
              A= A,
              method=method,
              control=Settings)

  fit$A = A
  fit$method = method
  fit$N_p = N_p
  fit$fixedSt_P = fixedSt_P
  fit$func = func

  return(fit)
}


