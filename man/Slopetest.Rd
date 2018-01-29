\name{Slopetest}
\alias{Slopetest}
\title{
Slope statistics based Analysis for NIRS data.  
}
\description{
Estimate the slope statistics and conduct a nonparametric based test on the slope difference before transfuion and after trasfusion.  If detection limit occurs at 15%, this function will impute the missed data based on uniform distribution and conduct a within-band permutation approach within imputation for statistical testing.
}
\usage{
Slopetest(Yvec,timevec,transfusionvec,SD_est=F,num.permu=1000)
}
\arguments{
    \item{Yvec}{The outcome of NIRS time series \eqn{Y(t_{i})} of length N ranging from 15 to 100.}
    \item{timevec}{The time index of NIRS time series \eqn{t_{i}} of length N.}
    \item{transfusionvec}{The 0/1 indicator of the transfusion status \eqn{X(t_{i})}. \eqn{X(t_{i})=0} means the current time point is before transfusion and \eqn{X(t_{i})=1} means the current time point is after transfusion.}
    \item{SD_est}{Whether to estimate the SD of the SLOPE statistic for pre-transfusion and post-transfuion. Default value is FALSE.}
    \item{num.permu}{Number of permutation for permutation test. Default value is 1000.}
}
\details{
  This function estimates the slope statistics before transfusion and after transfusion based on penalized regression spline method and tests the difference based on a within-band permutation approach. If there is detection limit occurs (15), it will impute the missed data based on a uniform distribution and estimate the slope statistics through a standard imputation approach. The statistical testing is conducted through a nested within-band permutation approach across all imputated datasets. 
}

\value{
    An R vector from Slopetest containing Slope statistics and Pvalue in the following order:
   \item{Slope.before}{The estimated Slope statistic before transfusion.}
   \item{Slope.after}{The estimated Slope statistic after transfusion.}
   \item{Slope.diff}{The estimated Slope statistic difference between before transfusion and after transfusion.}
   \item{Pvalue}{The pvalue of testing the Slope difference to be zero or not.}
   \item{SD_pre}{SD of the Slope statistic for pre-transfusion. Optional, only when \code{SD_est} = TRUE.}
   \item{SD_post}{SD of the Slope statistic for post-transfusion. Optional, only when \code{SD_est} = TRUE.}
}

\references{
Guo, Y., Wang, Y., Marin, T., Kirk, E., Patel, R., Josephson, C.   \emph{
     Statistical methods for characterizing transfusion-related changes in regional oxygenation using Near-infrared spectroscopy in preterm infants. 
  } 
}


\author{Yikai Wang and Ying Guo\cr
  Maintainer: Yikai Wang \email{yikai.wang@emory.edu} }
  
\examples{
# Data Simulation
dat = data.frame(Y= rep(0,100),t=1:100,trans = c(rep(0,50),rep(1,50)))
dat$Y = apply(dat,1,function(x){rnorm(1,5*rnorm(1),6*exp(rnorm(1)))})
dat$Y = dat$Y + 15 - quantile(dat$Y,0.3) 
dat$Y[dat$Y<=15] = 15


# Estimate the Slope statistics of the NIRS data and test on the difference. 
Slopetest(dat$Y,dat$t,dat$trans,FALSE,100)
}