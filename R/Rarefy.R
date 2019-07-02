
#' Recalculate Network Metrics With Rarefied Webs
#'
#' Resamples empirical network observations at a range of sampling levels and calls networklevel()
#' function from bipartite package to calculate network metrics.
#'
#' Can return either a data frame of raw metrics, a ggplot or a data frame of 'confidence intervals'.
#'
#' These CI are calculated from the set of resamples by ordering the network values and taking the
#' value of the metric ranked at the 5th and 95th percentile. (this method is very similar to that
#' employed by Casas \emph{et al.} 2018 \emph{Assessing sampling sufficiency of network metrics using bootstrap}
#' Ecological Complexity 36:268-275.)
#'
#' Note that confidence intervals for many metrics, particularly qualitative ones, will be biased by
#'  the issue of false-negatives. Resampling of observations will not introduce missing links.
#'
#' By default the size of resamples are taken to be proportional to the original sample size. Original sample size is
#' defined as the sum of the supplied web. If a specific set of sample sizes is wanted, use \code{abs_sample_levels}
#'
#' It is possible to extrapolate how increases sample size may lead to increased confidence in a metric too.
#' Set the sequence to \code{frac_sample_levels} to go beyond 1.
#'
#' @param web A matrix format web, as for \code{bipartite}
#' @param n_per_level How many samples to take per sample level. Default is 1000.
#' @param frac_sample_levels Sequence of fractions of original sample size to resample at.
#' @param abs_sample_levels If supplied, vector of absolute sample sizes to use to override  \code{frac_sample_levels}. Default = NULL
#' @param metrics vector of metrics to calculate. Will be passed to  \code{index} of  \code{networklevel()}. Default = 'info'
#' @param PARALLEL Logical. If TRUE, will use parallel package to speed up metric calculation. Default = FALSE
#' @param cores If using parallel, how man cores to use. Default = 2
#' @param output String specifying output. If 'plot' will return a ggplot facetted by metric using  \code{PlotRarefaction()}. If
#' 'CI' will return a data frame (using  \code{ComputeCI()} containing 5 columns: Metric, LowerCI, UpperCI, Mean, SampleSize. Otherwise will return a
#' data frame of the raw recalculated metrics, with a separate column for each metric, and the last column specifying the resample size.
#' @param ... Additional arguments to pass to \code{networklevel}. e.g. empty.web=FALSE
#'
#' @seealso \code{\link[bipartite]{networklevel}}
#'
#' @return Either a dataframe or a ggplot object. See details.
#' @examples
#' data(Safariland, package = 'bipartite')
#' RarefyNetwork(Safariland, n_per_level = 100)
#' @export
RarefyNetwork<- function(web, n_per_level= 1000,
                         frac_sample_levels = seq(0.2, 1, l=5),
                         abs_sample_levels = NULL,
                         metrics = 'info', PARALLEL = FALSE, cores=2,
                         output = 'df', ...){

  sampledrawcounts<-sum(web)* frac_sample_levels
  if(!is.null(abs_sample_levels)){
    sampledrawcounts <- abs_sample_levels
  }

  df<- map_df(sampledrawcounts,.f = ReCalculate,
              web= web, n = n_per_level,
              PARALLEL=PARALLEL,  cores=cores, metrics = metrics , ...    )

  if(output== 'plot'){
    return(PlotRarefaction(df))
  }
  if(output == 'CI'){
    return(ComputeCI(df))
  }else{
    return(df)
  }
}


DoDraw <-function(i,web, samplesize){
  draws<- rmultinom(1, size = samplesize, prob = web)
  redrawnweb <- matrix(draws, nrow= nrow(web), ncol = ncol(web))
}

CalcMetric<- function(web, index,...){
  t_dd <- t(bipartite::networklevel(web,index,...))
  rownames(t_dd)<- NULL
  as.data.frame(t_dd)
}

ReCalculate<- function( samplesize,n, web, metrics, PARALLEL =FALSE, cores=2, ...){

  draws<-  map(1:n, DoDraw , web, samplesize)
  if(PARALLEL){
    cl<-parallel::makeCluster(cores)
    parallel::clusterExport(cl, ls())
    parallel::clusterEvalQ(cl,{library( bipartite)})
    Metrics<- parallel::parLapply(cl=cl, X=draws, fun=CalcMetric, index = metrics, ...)
    parallel::stopCluster(cl)
    Metrics<- dplyr::bind_rows(Metrics)
  }else{
    Metrics<- map_df(draws, CalcMetric, index = metrics, ...)
  }

  Metrics$SampleSize<- samplesize

  return(Metrics)
}
