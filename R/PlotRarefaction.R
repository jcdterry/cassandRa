#' Plot Metric Response To Network Rarefaction
#'
#' Used to plot the output from \code{RarefyNetwork()}. See vignette!
#'
#' @param df A data frame produced by RarefyNetwork
#'
#' @return A ggplot
#' 
#' @examples
#'  \dontrun{
#'  X<-RarefyNetwork(Safariland, n_per_level = 100)
#'  ComputeCI(X)
#'  }
#' 
#' 
#' @export
PlotRarefaction <- function(df){
  df %>%
    tidyr::gather('Metric', 'Value', -SampleSize)%>%
    ggplot(aes(x= factor(SampleSize), y = Value) )+
    geom_boxplot()+
    facet_wrap(~Metric,scales = 'free_y')
}
