#' Fit all the models
#'
#'  Internal function called by \code{PredictLinks()}
#'  Fits the coverage deficit, Trait, Centrality, Matching-Centrality and SBM models by sequentially
#'  calling the individual functions.
#'
#' @param list A network list
#' @param RepeatModels How many times to fit each model from different starting points. Uses best half (rounding up)
#' @return A network list including the model fit
#' @export
#'
FitAllModels<- function(list, RepeatModels = 10){


  message('Starting:', list$WOL_Name,'_',list$i, ' at ', Sys.time(), '\n Fitting:')

  list<-CalcHostLevelCoverage(list)
  message('C')
  list<-FitCentrality(list, N_runs = RepeatModels)
  message('M')
  list<-FitMatching(list, N_runs = RepeatModels)
  message('B')
  list<-FitBothMandC(list, N_runs = RepeatModels)
  message('-SBM')
  list<- FitSBM(list, n_SBM = RepeatModels)
  
  return(list)
}
