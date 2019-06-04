#' Custom optimiser function for SBM models
#'
#'Designed to be called by FitSBM()
#'
#'Based on optimising algorithm described in Larremore, D.B., Clauset, A. & Jacobs, A.Z. (2014).
#'Efficiently inferring community structure in bipartite networks. Phys. Rev. E - Stat. Nonlinear,
#'Soft Matter Phys., 90, 1-12
#'
#'Initially all species are randomly assigned to groups. Then, one at a time, each species is swapped
#'into a different group and the likelihood of the model assessed (with SBMLik()).
#'
#'The best model of all these swaps is then selected (even if it is worse) and used in the next round of swapping.
#'
#'This fits the 'degree-corrected' biSBM mdoel of Larremore et al., which is generally better when there are broad degree distributions
#'
#'This is repeated until either n_rounds_max is reached, or the (most commonly), if the best model
#'in the last 20 is within 0.1 log-likelihood of the best overall (implying it has stopped improving).
#'
#' @param i Seed
#' @param A Binary Interaction Matrix
#' @param G Number of Groups
#' @param N_Rounds_max Maximum number round to keep drawing
#' @param plot If set to TRUE, plots the progress of likelihood improvement, used to check if convergence is good.
#'
#' @return A list containing 'LogLik' (the maximum likelihood found) 'SB_H', the group assignments of the host,
#''SB_W', the group assignments of the other level, and 'Omega_rs', the interaction probabilities between groups.
#' @export

Optimise_SBM<- function(i=NULL,A, G, N_Rounds_max= 500, plot= FALSE){
  if(!is.null(i)){set.seed(i)}
  n_host=nrow(A)
  n_wasp=ncol(A)
  n = n_host+n_wasp
  BestLiks<- rep(NA,N_Rounds_max )
  SB_H_start <- sample(1:G, size=n_host, replace = TRUE)
  SB_W_start <- sample(1:G, size=n_wasp, replace = TRUE)

  for(repeatDraw in 1:N_Rounds_max ){

    LikH <- rep(NA, n_host)
    NewValueH <- rep(NA, n_host)
    LikW <- rep(NA, n_wasp)
    NewValueW <- rep(NA, n_wasp)

    # Test switching each host
    for( x in 1:n_host){
      SB_H <- SB_H_start
      SB_H[x]<- sample((1:G)[-SB_H[x]], 1) # Draw another group id, that isn't the one it already is..,
      LikH[x]<-SBMLik(A, SB_H,SB_W =  SB_W_start, G=G)
      NewValueH[x]<-SB_H[x]
    }
    # Test switching each wasp
    for( x in 1:n_wasp){
      SB_W <- SB_W_start
      SB_W[x]<- sample((1:G)[-SB_W[x]], 1) # Draw another group id, that isn't the one it already is..,
      LikW[x]<-SBMLik(A, SB_H_start,SB_W =  SB_W, G=G)
      NewValueW[x]<-SB_W[x]
    }
    # Find best
    ToChange<-which.max(c(LikH, LikW))
    BestLiks[repeatDraw]<-(max(c(LikH, LikW), na.rm=TRUE))

    if(ToChange <= n_host){
      SB_H_start[ToChange]<- NewValueH[ToChange]
    }else{
      SB_W_start[ToChange - n_host]<- NewValueW[ToChange - n_host]
    }

    if(repeatDraw>50){# After done 50 swaps, start lookin to see if settled
      Best_in_last_20<-max( BestLiks[(repeatDraw-20): repeatDraw])
      if(abs(max(BestLiks[1:(repeatDraw-20)], na.rm = TRUE)-Best_in_last_20)<0.1){ # If not much change from the overall max
        break # get out of the for-loop
      }
    }
  }
  if(plot){plot(BestLiks)}
  # SBMLik(A, SB_H = SB_H_start, SB_W = SB_W_start, G=G)



  CalcOmega_rs<- function(A, SB_H, SB_W, G){
    Ws <- matrix(NA, G, G)

    for(H_group in 1:G){
      for(W_Group in 1:G){
        Block<- A[SB_H==H_group, SB_W==W_Group,drop=FALSE]
        Ws[H_group,W_Group] <- mean(Block)
      }
    }
    return(Ws)
  }


  return(list('LogLik' = BestLiks[repeatDraw],
              'SB_H' = SB_H_start,
              'SB_W' = SB_W_start,
              'Omega_rs' = CalcOmega_rs(A, SB_H_start, SB_W_start, G)
  ))

}




SBM_LikGroupCombo<- function(A, SB_H, SB_W, SB_HID, SB_WID){

  if(any(SB_H==SB_HID) & any(SB_W==SB_WID)){
    m_rs <- sum(A[SB_H==SB_HID,SB_W==SB_WID] )
    k_r <-  sum(A[SB_H==SB_HID,])
    k_s <-  sum(A[,SB_W==SB_WID])
    G_L <- m_rs*log(m_rs/(k_r*k_s))
    if(is.na(G_L)){G_L<-0}
    return(G_L)
  }else{
    return(NA)
  }
}

SBMLik <- function(A, SB_H, SB_W, G){

  Gs <- matrix(NA, G, G)
  for(H_group in 1:G){
    for(W_Group in 1:G){
      Gs[H_group,W_Group] <- SBM_LikGroupCombo(A, SB_H, SB_W,SB_HID =  H_group,SB_WID =  W_Group)
    }
  }
  return(sum(Gs, na.rm = TRUE))
}

