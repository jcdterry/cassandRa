#' Coverage Estimator, using Chao1 Index, Turing-Good or Binomial depending on what is possible
#'
#' An estimate of the sample coverage, which tries to use the most appropriate method
#'
#' Sample coverage is defined as the probability that the next interaction drawn is of a type not yet seen
#'
#' If the sample size is at or below a cutoff (5) or if all the samples are singletons,
#'  this is calculated as the posterior mean of a binomial model using a flat prior (this can be changed
#' to a Jeffereys).
#'
#' If there are singletons but no doubletons, the Turing-Good estimate is used:   c_hat = 1 - (f1/n)
#'
#' If there are both singletons and doubletons, the Chao1 index is used:
#'
#' c_hat = 1 -( (f1/n) * ( (f1*(n-1))/((n-1)*(f1+(2*f2)))   )  )
#'
#' @param x A vector of integers, the observed sample counts
#' @param cutoff When to switch from binomial model to Chao1 estimator
#' @param BayesPrior Prior to use. Either 'Flat' or 'Jeffereys'.
#' @return c_hat, the estimated coverage. (i.e. 1-  C_def)
#' @export
#'
CoverageEstimator <- function(x, cutoff=5, BayesPrior = 'Flat'){
  n= sum(x)
  f1 <- sum(x==1)
  f2 <- sum(x==2)

  # this is a first guess based on posterior mean of binomial distribution, Chao may overwirte it

  if(BayesPrior == 'Jeffereys'){ c_hat <-   1- ( 1/(2*(n+1)) )}# using Jeffereys prior ( beta(0.5, 0.5))
  if(BayesPrior == 'Flat'){c_hat <-  1-  (1 / (n + 2))}        #  using flat prior (a, b = 1)

  AllOnes <-  all(x%in% c(1,0))

  if(n > cutoff & !AllOnes){
    if(f1>0 & f2==0){ ## Turing-Good
      c_hat = 1 - (f1/n)
    }
    if(f1>0 & f2>0){ ## Chao1
      c_hat = 1 -( (f1/n) * ( (f1*(n-1))/((n-1)*(f1+(2*f2)))   )  )
    }
  }
  return(c_hat)
}
