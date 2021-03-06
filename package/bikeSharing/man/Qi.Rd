% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/negativeBinomialGLMM.R
\name{Qi}
\alias{Qi}
\title{Calculate the Q-function for the unique date i}
\usage{
Qi(yi, xi, thetat, betat, s2gammat, gammai, burn_in)
}
\arguments{
\item{yi}{a vector containing the bike counts observations for date i}

\item{xi}{a matrix containing the predictor variable observations for date i}

\item{thetat}{the estimate for theta at EM iteration t}

\item{betat}{the estimate for beta at EM iteration t}

\item{s2gammat}{the estimate for s2gammat at EM iteration t}

\item{gammai}{list of random chain results for date i}

\item{burn_in}{iterations to discard for burn_in}
}
\value{
q function values for i
}
\description{
This is a helper function that calculates the Q-function for
the unique date i
}
