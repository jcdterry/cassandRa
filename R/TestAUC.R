#' Test via AUC the predictive capacity of each model or combination of models
#'
#' @param list Network List
#'
#' @return a list with 'DataforAUC', a data frame with each interaction as a row and the predictions
#'  of each model, and 'AUC', a data frame with the predictive capacity of all the models and many combinations
#' @export
#'
TestAUC  <- function(list){



  if(is.null(list$ObsSuccess)){
    stop('No success data. Run SortResponseCategory() First')
  }

  #### Because tidyverse functions don't give visible bindings, which CRAN complains about,
  #### need to to define names here to stop Note.
  Var1<-NULL;Var2<-NULL;value<-NULL;Host<-NULL;Wasp<-NULL;int_code<-NULL;Centrality_Prob<-NULL
  Matching_Prob<-NULL;Both_Prob<-NULL;SBM_Prob<-NULL;C_def_Prob<-NULL;Category<-NULL

  reshape2::melt(list$C_ProbsMatrix)%>%
    dplyr::rename('Host' = Var1, 'Wasp' = Var2,
           'Centrality_Prob' = value)%>%
    mutate(int_code = paste(Host,Wasp, sep='_'))%>%
    select(int_code, Centrality_Prob) %>%
    full_join(list$ObsSuccess, by = 'int_code')-> list$ObsSuccess

  reshape2::melt(list$M_ProbsMatrix)%>%
    dplyr::rename('Host' = Var1, 'Wasp' = Var2,
           'Matching_Prob' = value)%>%
    mutate(int_code = paste(Host,Wasp, sep='_'))%>%
    select(int_code, Matching_Prob) %>%
    full_join(list$ObsSuccess, by = 'int_code')-> list$ObsSuccess

  reshape2::melt(list$B_ProbsMat)%>%
    dplyr::rename('Host' = Var1, 'Wasp' = Var2,
           'Both_Prob' = value)%>%
    mutate(int_code = paste(Host,Wasp, sep='_'))%>%
    select(int_code, Both_Prob) %>%
    full_join(list$ObsSuccess, by = 'int_code')-> list$ObsSuccess


  reshape2::melt( list$SBM_ProbsMat)%>%
    dplyr::rename('Host' = Var1, 'Wasp' = Var2,
           'SBM_Prob' = value)%>%
    mutate(int_code = paste(Host,Wasp, sep='_'))%>%
    select(int_code, SBM_Prob) %>%
    full_join(list$ObsSuccess, by = 'int_code')-> list$ObsSuccess


  reshape2::melt(list$C_defmatrix)%>%
    dplyr::rename('Host' = Var1, 'Wasp' = Var2,
           'C_def_Prob' = value)%>%
    mutate(int_code = paste(Host,Wasp, sep='_'))%>%
    select(int_code, C_def_Prob) %>%
    full_join(list$ObsSuccess, by = 'int_code')-> list$ObsSuccess


  list$ObsSuccess%>%
    filter(Category != 'True Positive')-> DataforAUC


  data.frame('Host_obs' = rowSums(list$obs),
             'Host'= 1: list$n_hosts)-> HostObservations
  DataforAUC%>%
    left_join(HostObservations, by='Host') -> DataforAUC


  ## This scales the means of the probabilities, excluding the True-Positives.
  DataforAUC$Host_obs        <- DataforAUC$Host_obs       /  mean( DataforAUC$Host_obs)
  DataforAUC$SBM_Prob        <- DataforAUC$SBM_Prob       /  mean( DataforAUC$SBM_Prob)
  DataforAUC$C_def_Prob      <- DataforAUC$C_def_Prob     /  mean( DataforAUC$C_def_Prob)
  DataforAUC$Both_Prob       <- DataforAUC$Both_Prob      /  mean( DataforAUC$Both_Prob)
  DataforAUC$Matching_Prob   <- DataforAUC$Matching_Prob  /  mean( DataforAUC$Matching_Prob)
  DataforAUC$Centrality_Prob <- DataforAUC$Centrality_Prob/  mean( DataforAUC$Centrality_Prob)

   list$DataforAUC <- DataforAUC



  D<-list$DataforAUC

  AUC<-data.frame( 'Basis' = c( 'C_def',
                                'H_obs',

                                'C',
                                'M',
                                'B',
                                'CxM',
                                'C+M',
                                'SBM',
                                'SBM+B',
                                'SBM*B',

                                'C+C_def',
                                'M+C_def',
                                'B+C_def',
                                'CxM+C_def',
                                'C+M+C_def',
                                'SBM+C_def',
                                'SBM+B+C_def',
                                'SBM*B+C_def',

                                'C*C_def',
                                'M*C_def',
                                'B*C_def',
                                'CxM*C_def',
                                'C+M*C_def',
                                'SBM*C_def',
                                'SBM+B*C_def',
                                'SBM*B*C_def',

                                'C+H_obs',
                                'M+H_obs',
                                'B+H_obs',
                                'CxM+H_obs',
                                'C+M+H_obs',
                                'SBM+H_obs',
                                'SBM+B+H_obs',
                                'SBM*B+H_obs',

                                'C*H_obs',
                                'M*H_obs',
                                'B*H_obs',
                                'CxM*H_obs',
                                'C+M*H_obs',
                                'SBM*H_obs',
                                'SBM+B*H_obs',
                                'SBM*B*H_obs' ),
                   'AUC' = c(pROC::auc(D$Category, D$C_def_Prob),
                             pROC::auc(D$Category, D$Host_obs),

                             pROC::auc(D$Category, D$Centrality_Prob),
                             pROC::auc(D$Category, D$Matching_Prob),
                             pROC::auc(D$Category, D$Both_Prob),
                             pROC::auc(D$Category, D$Centrality_Prob * D$Matching_Prob),
                             pROC::auc(D$Category, D$Centrality_Prob + D$Matching_Prob),
                             pROC::auc(D$Category, D$SBM_Prob),
                             pROC::auc(D$Category, D$SBM_Prob + D$Matching_Prob),
                             pROC::auc(D$Category, D$SBM_Prob * D$Matching_Prob),

                             pROC::auc(D$Category,  D$C_def_Prob+D$Centrality_Prob),
                             pROC::auc(D$Category,  D$C_def_Prob+D$Matching_Prob),
                             pROC::auc(D$Category,  D$C_def_Prob+D$Both_Prob),
                             pROC::auc(D$Category,  D$C_def_Prob+D$Centrality_Prob * D$Matching_Prob),
                             pROC::auc(D$Category,  D$C_def_Prob+D$Centrality_Prob + D$Matching_Prob),
                             pROC::auc(D$Category,  D$C_def_Prob+D$SBM_Prob),
                             pROC::auc(D$Category,  D$C_def_Prob+D$SBM_Prob + D$Matching_Prob),
                             pROC::auc(D$Category,  D$C_def_Prob+D$SBM_Prob * D$Matching_Prob),

                             pROC::auc(D$Category, D$C_def_Prob * D$Centrality_Prob),
                             pROC::auc(D$Category, D$C_def_Prob * D$Matching_Prob),
                             pROC::auc(D$Category, D$C_def_Prob * D$Both_Prob),
                             pROC::auc(D$Category, D$C_def_Prob * D$Centrality_Prob * D$Matching_Prob),
                             pROC::auc(D$Category, D$C_def_Prob * D$Centrality_Prob + D$Matching_Prob),
                             pROC::auc(D$Category, D$C_def_Prob * D$SBM_Prob),
                             pROC::auc(D$Category, D$C_def_Prob * D$SBM_Prob + D$Matching_Prob),
                             pROC::auc(D$Category, D$C_def_Prob * D$SBM_Prob * D$Matching_Prob),

                             pROC::auc(D$Category,  D$Host_obs + D$Centrality_Prob),
                             pROC::auc(D$Category,  D$Host_obs + D$Matching_Prob),
                             pROC::auc(D$Category,  D$Host_obs + D$Both_Prob),
                             pROC::auc(D$Category,  D$Host_obs + D$Centrality_Prob * D$Matching_Prob),
                             pROC::auc(D$Category,  D$Host_obs + D$Centrality_Prob + D$Matching_Prob),
                             pROC::auc(D$Category,  D$Host_obs + D$SBM_Prob),
                             pROC::auc(D$Category,  D$Host_obs + D$SBM_Prob + D$Matching_Prob),
                             pROC::auc(D$Category,  D$Host_obs + D$SBM_Prob * D$Matching_Prob),

                             pROC::auc(D$Category,  D$Host_obs * D$Centrality_Prob),
                             pROC::auc(D$Category,  D$Host_obs * D$Matching_Prob),
                             pROC::auc(D$Category,  D$Host_obs * D$Both_Prob),
                             pROC::auc(D$Category,  D$Host_obs * D$Centrality_Prob * D$Matching_Prob),
                             pROC::auc(D$Category,  D$Host_obs * D$Centrality_Prob + D$Matching_Prob),
                             pROC::auc(D$Category,  D$Host_obs * D$SBM_Prob),
                             pROC::auc(D$Category,  D$Host_obs * D$SBM_Prob + D$Matching_Prob),
                             pROC::auc(D$Category,  D$Host_obs * D$SBM_Prob * D$Matching_Prob)))


  AUC$AUC = round(AUC$AUC, 2)
  list$AUC <- AUC
  return(list)
}
