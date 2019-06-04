#' Fit all the models
#'
#' Fits the coverage deficit, Trait, Centrality, Matching-Centrality and SBM models by sequentially
#'  calling the individual functions. W
#'
#' @param list A network list
#' @param RepeatModels How many times to fit each model from different starting points. Uses best half (rounding up)
#' @param SaveName Options string. If provided, will save the output to the name.
#'
#' @return A network list including the model fit
#' @export
#'
FitAllModels<- function(list, RepeatModels = 10, SaveName = NULL){


   cat(paste0('Starting', list$WOL_Name,'_',list$i, ' at ', Sys.time(), '\n Fitting:'))

  list<-CalcHostLevelCoverage(list)
  cat('C')
  list<-FitCentrality(list, N_runs = RepeatModels)
  cat('M')
  list<-FitMatching(list, N_runs = RepeatModels)
  cat('B')
  list<-FitBothMandC(list, N_runs = RepeatModels)
  cat('-SBM')
  list<- FitSBM(list, n_SBM = RepeatModels)

  if(!is.null(SaveName)){
    assign(SaveName, list)
    save(list = SaveName, file = paste0('Fits/',SaveName))
  }
  return(list)
}
