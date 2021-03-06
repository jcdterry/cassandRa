% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Optimise_SBM.R
\name{Optimise_SBM}
\alias{Optimise_SBM}
\title{Custom optimiser function for SBM models}
\usage{
Optimise_SBM(i = NULL, A, G, N_Rounds_max = 500, plot = FALSE)
}
\arguments{
\item{i}{Seed}

\item{A}{Binary Interaction Matrix}

\item{G}{Number of Groups}

\item{N_Rounds_max}{Maximum number round to keep drawing}

\item{plot}{If set to TRUE, plots the progress of likelihood improvement, used to check if convergence is good.}
}
\value{
A list containing 'LogLik' (the maximum likelihood found) 'SB_H', the group assignments of the host,
'SB_W', the group assignments of the other level, and 'Omega_rs', the interaction probabilities between groups.
}
\description{
Designed to be called by FitSBM()
}
\details{
Based on optimising algorithm described in Larremore, D.B., Clauset, A. & Jacobs, A.Z. (2014).
Efficiently inferring community structure in bipartite networks. Phys. Rev. E - Stat. Nonlinear,
Soft Matter Phys., 90, 1-12

Initially all species are randomly assigned to groups. Then, one at a time, each species is swapped
into a different group and the likelihood of the model assessed (with SBMLik()).

The best model of all these swaps is then selected (even if it is worse) and used in the next round of swapping.

This fits the 'degree-corrected' biSBM mdoel of Larremore et al., which is generally better when there are broad degree distributions

This is repeated until either n_rounds_max is reached, or the (most commonly), if the best model
in the last 20 is within 0.1 log-likelihood of the best overall (implying it has stopped improving).
}
