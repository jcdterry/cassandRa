#' Compute Basic Confidence Intervals
#'
#' @param df A data frame produced by \code{RarefyNetwork()}
#'
#' @return a dataframe detailing confidence intervals at each tested sample size
#'
#' @examples
#'  \dontrun{
#'  X<-RarefyNetwork(Safariland, n_per_level = 100)
#'  PlotRarefaction(X)
#'  }
#' @export
ComputeCI <- function(df){
  map_df(unique(df$SampleSize),function(Size, df){

    dfs<- dplyr::filter(df, SampleSize ==  Size)
    CI_df <-  data.frame('Metric' = colnames(dfs)[-ncol(dfs)],
                         LowerCI = NA,
                         UpperCI = NA,
                         Mean = colMeans(dfs[,-ncol(dfs)], na.rm = TRUE),
                         SampleSize = Size)

    for(j in 1: (ncol(df)-1)){

      V <-dfs[,j]

      if(any(is.na(V))){

        warning(paste('One or more NA in metric:', colnames(df)[j],'at size', Size ))
      }
      V<- V[!is.na(V)]

    n005<-floor(length(V)*0.95)
    n095<-floor(length(V)*0.05)

      CI_df[j, c(2,3)]  <-sort(V, decreasing = TRUE)[c(n005,n095 )]
    }
    CI_df
  } ,   df=df )%>%
    dplyr::arrange(Metric)
}

