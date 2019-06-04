#' Make an artificial bipartite networks with some properties of ecological networks, then sample from it
#'
#' Core model adapted from: "Sampling bias is a challenge [...]: lessons from a quantitative nichemodel" by
#' Jochen Frund, Kevin S. McCann and Neal M. Williams
#'
#' Abundances are assigned by generating abundances that match a log-normal distribution (but without
#'  introducing noise)
#'
#' @param seed Random number generator seed, if specified.
#' @param specpar Specialisation parameter, equal to 1/sd of the nornmal curve that defines the consumption range
#' @param n_hosts Number of focal level species (e.g. hosts, flowers)
#' @param n_wasps Number of non-focal level species (e.g. parasitic wasps, pollinators)
#' @param TargetTrueConn Proportion of possible interactions to keep
#' @param SampleObs Number of samples to draw
#' @param abun_mean Mean abundance level (log scale).
#' @param abun_sdlog Distributon of abundance level (SD vlog scale).
#' @param traitvsnested The relative balance between the nestedness generator and the trait-based generator
#' @param hosttrait_n Number of trait dimensions. Default 'two', uses two traits, with one dominant. 'single' and 'multi' retained from Frund et al.
#'
#' @return A network list containing 'obs' a matrix of observations, 'TrueWeb' a matrix of the 'true']
#'  drawn web, and number of other properties of these networks.
#' @export
#'
#' @examples make_true_and_sample_web()
make_true_and_sample_web <- function(seed=NULL,specpar =1,
                                     n_hosts=10, n_wasps=10,
                                     TargetTrueConn=0.5 ,
                                     SampleObs=1000, abun_mean=5,
                                     abun_sdlog=1, traitvsnested = 0.5, hosttrait_n = 'two'){

  if(!is.null(seed)){set.seed(seed)}

  wasp_abun <- get_skewabuns(n_wasps, abun_mean, abun_sdlog)
  host_abun <- get_skewabuns(n_hosts, abun_mean, abun_sdlog)

  web_trait<-   makeweb(specpar, n_wasps,n_hosts, hosttrait_n=hosttrait_n )
  web_trait<-matrix(scale(as.vector(web_trait), center = FALSE), ncol=ncol(web_trait), nrow= nrow(web_trait))

  web_gen <- matrix(rep(scale(rpois(n_wasps, n_hosts*TargetTrueConn), center = FALSE), n_hosts), ncol=n_wasps, nrow=n_hosts, byrow=TRUE)## This term increases the qualitiative nestedness, by assigning each species a vulnerability/generality, and changing the probability by the product
  web_vul <- matrix(rep(scale(rpois(n_hosts, n_wasps*TargetTrueConn), center = FALSE), n_wasps), ncol=n_wasps, nrow=n_hosts)
  web_Nest = web_gen*web_vul

  web_p <- (web_trait*traitvsnested) + (web_Nest*(1-traitvsnested))
  web_p <- web_p* runif(n_hosts*n_wasps, min = 0.9, max= 1.1) # add a bit of shake
  TrueIntCount = floor(n_hosts*n_wasps*TargetTrueConn)

  # this is the matrix needed for  scaling by abundance; it's not actually a network, but its dimensions fit
  web_relabun <- (host_abun %*% t(wasp_abun)) / mean(host_abun)
  ##### there is no relationship abundance and generality since selection of interactions to set to zero is done based on web_p

  web<-   web_p * web_relabun
  web[rank(-web_p)>TrueIntCount]<-0  ### Only keep the strongest interactions
  web_p[rank(-web_p)>TrueIntCount]<-0

  # make observations,

  obs <- matrix(rmultinom(1, SampleObs, prob = web), nrow = n_hosts, ncol=n_wasps)

  # remove from further consideration species that do not appear
  HostsToKeep<- rowSums(obs)>0
  WaspsToKeep <- colSums(obs)>0
  web<- web[HostsToKeep,WaspsToKeep]
  web_p<- web_p[HostsToKeep,WaspsToKeep]
  host_abun<- host_abun[HostsToKeep]
  obs<- obs[HostsToKeep,WaspsToKeep]
  n_wasps =sum(WaspsToKeep)
  n_hosts = sum(HostsToKeep)

  list('TrueWeb' = web,
       'Seed'= seed,
       'obs' = obs,
       'n_obs'= SampleObs,
       'TargetTrueConn'= TargetTrueConn,
       'n_hosts'= n_hosts,
       'n_wasps' = n_wasps,
       'TrueConn' = mean(web>0),
       'SampleConn' = mean(obs>0),
       'FracNetworkComplete' = sum(obs>0)/sum(web>0),
       'TrueNetCdef' = 1- (sum(web[obs>0])/sum(web))
  )
}



makeweb <- function(specpar = 1, n_wasps, n_hosts, hosttrait_n="two"){

  fun_pref <- function(traitdif){
    prefs <- dnorm(traitdif,mean=0,sd=1/specpar)
    prefs
  }
  if (hosttrait_n == "single"){
    hosttrait <- runif(n_hosts)
    # for use with empirical trait data, this could be readily replaced with a vector of host  traits (potentially a trait axis derived from multiple traits)
    web <- replicate(n_wasps, fun_pref(abs(runif(1) - hosttrait)))
  }

  if (hosttrait_n == "two"){
    hosttrait1 <- runif(n_hosts)
    hosttrait2 <- runif(n_hosts, max = 0.5) # trait 2 less important
    #Take Euclidean distance in trait space Ranges from 0:sqrt(1.25)
    web <- replicate(n_wasps, fun_pref( sqrt( ((runif(1) - hosttrait1)^2) +((runif(1, max=0.5) - hosttrait2))^2)    ))
  }
  if (hosttrait_n == "multi"){
    web <- replicate(n_wasps, fun_pref(abs(runif(1) - runif(n_hosts))))
  }
  web <- web / matrix(colSums(web),nrow=n_hosts,ncol=n_wasps,byrow=TRUE) # standardize link weights to probability;
  web
}

get_skewabuns <- function(myN, abun_mean, abun_sdlog){
  # generate abundances that match a log-normal distribution (but without introducing noise):
  # divide quantile distribution in N+1 regular intervals, and take the N non-0or1 intvl borders  as abundance values
  # it is rescaled in the second step to really have the intended mean abundance (not log-mean)
  abun <- qlnorm(seq(0, 1, length.out=myN+2), log(abun_mean), abun_sdlog)[-c(1,myN+2)] # takes  equidistant points of the quantile function, removing the extremes that would be 0 and Inf
  abun <- sort(abun, decreasing =TRUE) # just for convenience, sort to have abundant species first (as  often used for webs)
  abun * abun_mean / mean(abun)
}
