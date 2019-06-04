#' Test the models by AUC
#'
#' The function assumes FitAllModels() has already been run. It is a wrapper for 'SortResponseCategory()' and 'TestAUC()'
#'
#' @param list A network list
#'
#' @return the network list with added AUC data. Key values are 'AUC', a dataframe with the AUC of
#'  each model and many combinations.
#' @export
#'
TestAllModels<- function(list){
  list<-SortResponseCategory(list)
  list <- TestAUC(list)
  return(list)
}
