#' Predicting Missing Links
#'
#' Package to make assessments of the most likely location of missing links and to test the effect of
#'  sample rarefaction or extrapolation on the confidence of bipartite network metrics.
#'
#'  The models to predict missing links are discussed in XXX.
#'
#'  See vignette for discussion and examples.
#'
#' @name EcoLinkPredict
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @import ggplot2
#' @importFrom magrittr %>%
#' @docType package
#' @importFrom stats dcauchy
#' @importFrom stats dnorm
#' @importFrom stats optim
#' @importFrom stats qlnorm
#' @importFrom stats rmultinom
#' @importFrom stats rnorm
#' @importFrom stats rpois
#' @importFrom stats runif
#' @importFrom stats setNames
NULL

# to Keep CRAN Happya about non-standard evalutation in tidyverse
Centrality_Prob=Matching_Prob=Both_Prob=SBM_Prob=C_def_Prob=Category = NULL
Var1=Var2=value=Host=Wasp=int_code=Prob=HostName=WaspName=ObsStr =NULL
Metric = SampleSize = Value = LogStr = NULL
