#' Generates a network list from a food web and fits all network models
#'
#' 
#' First calls \code{CreateListObject} to convert a matrix suitable for the bipartite package into a list structure.
#'
#' Then it calls \code{FitAllModels} to fit each of the missing link models in turn.
#'
#' @param web in format specified by the bipartite package. Rows = focal layer, columns = response layer
#' @param RepeatModels How many times to fit each model from different starting points. Uses best half (rounding up)
#' @return A network list including a large number of outputs.
#' @export
#'
#' @examples
#' \dontrun{
#' data(Safariland, 'bipartite')
#' tmp<- PredictLinks(Safariland)
#' PlotFit(tmp, Matrix_to_plot = 'SBM')
#' }

PredictLinks<- function(web, RepeatModels = 10){
  SF <- CreateListObject(web)
  list <- FitAllModels(SF, RepeatModels = RepeatModels)

  reshape2::melt(list$C_ProbsMatrix)%>%
    dplyr::rename('Centrality_Prob' = value)%>%
    select( Centrality_Prob) -> C_probs

  reshape2::melt(list$M_ProbsMatrix)%>%
    dplyr::rename('Matching_Prob' = value)%>%
    select(Matching_Prob) -> M_probs

  reshape2::melt(list$B_ProbsMat)%>%
    dplyr::rename( 'Both_Prob' = value)%>%
    select(Both_Prob) -> B_probs

  reshape2::melt( list$SBM_ProbsMat)%>%
    dplyr::rename( 'SBM_Prob' = value)%>%
    select( SBM_Prob) -> SBM_probs

  reshape2::melt(list$C_defmatrix)%>%
    dplyr::rename('C_def_Prob' = value)%>%
    select( C_def_Prob) -> C_def_probs

  expand.grid(list$HostNames, list$WaspNames, stringsAsFactors = FALSE)%>%
    as.data.frame()%>%
    transmute(Interaction = paste(Var1,Var2, sep=' - ')) -> Names

  reshape2::melt(list$obs>0)%>%
    dplyr::rename('Observed' = value)%>%
    mutate(int_code = paste(Var1,Var2, sep=' - '))%>%
    select(int_code, Observed) -> Observed

  DF <- bind_cols(Observed, C_probs, M_probs, B_probs, SBM_probs, C_def_probs, Names)
  to_Std_df <- filter(DF, Observed==FALSE)

  DF %>%
    mutate(std_Centrality_Prob  = ifelse(Observed,NA,  Centrality_Prob    / mean( to_Std_df$Centrality_Prob) ),
           std_Matching_Prob    = ifelse(Observed,NA,  Matching_Prob    / mean( to_Std_df$Matching_Prob) ),
           std_Both_Prob        = ifelse(Observed,NA,  Both_Prob    / mean( to_Std_df$Both_Prob) ),
           std_SBM_Prob         = ifelse(Observed,NA,  SBM_Prob   / mean( to_Std_df$SBM_Prob) ),
           std_C_def_Prob       = ifelse(Observed,NA,  C_def_Prob/ mean( to_Std_df$C_def_Prob) ))-> STDDF

  list$Predictions <- STDDF
  return(list)
}
