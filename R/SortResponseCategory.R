#' Adds a dataframe that defines each interaction as true positive, false negative or true negative
#'
#' @param list Network list
#'
#' @return A Network list object with ObsSuccess, a dataframe detailing all the interactions and whether they are True Positives, False Negative or True Negatives
#' @export
#'
SortResponseCategory <- function(list){

  ## Compares observations with known truth to assess fit quality

  Observed <- (list$obs>0)
  TrueMissed <- (list$TrueWeb>0) &(list$obs==0)

  dimnames(Observed)<-NULL
  dimnames(TrueMissed)<-NULL

  reshape2::melt(TrueMissed - Observed)%>%
    dplyr::rename('Host' = Var1, 'Wasp' = Var2,
                  'Category' = value)%>%
    mutate(Category = ifelse(Category == -1, 'True Positive', Category),
           Category = ifelse(Category == 1, 'False Negative', Category),
           Category = ifelse(Category == 0, 'True Negative', Category ))%>%
    mutate(int_code = paste(Host,Wasp, sep='_'))-> ObsSuccess


  if(!is.null(list$TrueWeb)){
      reshape2::melt(list$TrueWeb)%>%
      mutate(int_code = paste(Var1,Var2, sep='_'),
             LogStr = log10(value))%>%
      select(int_code, LogStr)%>%
      filter(!is.infinite(LogStr))-> Strengths
    ObsSuccess <-  left_join(ObsSuccess,Strengths, by =  'int_code')
  }

    reshape2::melt(list$obs)%>%
      mutate(int_code = paste(Var1,Var2, sep='_'),
             ObsStr = value)%>%
      select(int_code, ObsStr)-> ObsStrengths
    ObsSuccess <-  left_join(ObsSuccess,ObsStrengths, by =  'int_code')

  if(!is.null(list$EmpDataMat)){
      reshape2::melt( list$EmpDataMat)%>%
      mutate(int_code = paste(Var1,Var2, sep='_'),
             LogStr = log10(value))%>%
      select(int_code, LogStr)%>%
      filter(!is.infinite(LogStr))-> Strengths
    ObsSuccess <-  left_join(ObsSuccess,Strengths, by =  'int_code')
  }

    list$ObsSuccess <- ObsSuccess

  return(list)
}
