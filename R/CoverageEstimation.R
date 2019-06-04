#' Estimated probabilities of missing links based on the host Level Coverage Deficit
#'
#' Calls CoverageEstimator() to calculate host-level coverage deficit, then divides this by the number
#'  of unobserved interactions of that host.
#'
#' @param list Network List
#'
#' @return A network list, with 'C_defmatrix', a matrix of probabilities based on coverage deficit, and
#' 'OverallChaoEst' an estimate of the overall coverage deficit of the network.
#' @export
#'

 CalcHostLevelCoverage <- function(list){

C_def <- rep(NA,list$n_hosts )
for(i in 1: list$n_hosts){C_def[i] <- 1- CoverageEstimator(list$obs[i,])}
C_def<- C_def / rowSums(list$obs ==0) # standardise across avaiable interactions
C_defmatrix<- matrix(C_def, nrow=list$n_hosts, ncol= list$n_wasps)

OverallChaoEst <- CoverageEstimator(list$obs)
list$C_defmatrix <- C_defmatrix

list$OverallChaoEst <- OverallChaoEst
return(list)
}


