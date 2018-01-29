\name{plotNIRS}
\alias{plotNIRS}
\title{
NIRS Time Series Visualization
}
\description{
This function visualizes the NIRS time series data and estimates the underlying smoothed trend of the NRIS based on a nonparametric regression approach. 
}
\usage{
plotNIRS(Yvec,timevec,transfusionvec)
}
\arguments{
    \item{Yvec}{The outcome of NIRS time series \eqn{Y(t_{i})} of length N ranging from 15 to 100.}
    \item{timevec}{The time index of NIRS time series \eqn{t_{i}} of length N.}
    \item{transfusionvec}{The 0/1 indicator of the transfusion status \eqn{X(t_{i})}. \eqn{X(t_{i})=0} means the current time point is before transfusion and \eqn{X(t_{i})=1} means the current time point is after transfusion.}
}
\details{
  This function visualizes the NIRS time series data before and after transfusion. In order to estimate the underlying smoothed curve, it first imputes the data with detection limit (DL) and utilizes a nonparametric regression approach for the imputed data. The time points with DL is in red and others are in black. 
}

\author{Yikai Wang and Ying Guo\cr
  Maintainer: Yikai Wang \email{yikai.wang@emory.edu} }

\examples{
# Data Simulation
dat = data.frame(Y= rep(0,200),t=1:200,trans = c(rep(0,100),rep(1,100)))
dat$Y = apply(dat,1,function(x){rnorm(1,5*rnorm(1),6*exp(rnorm(1)))})
dat$Y = dat$Y + 15 - quantile(dat$Y,0.3) 
dat$Y[dat$Y<=15] = 15


# Visualize the NIRS time series before and after transfusion. 
plotNIRS(dat$Y,dat$t,dat$trans)
}