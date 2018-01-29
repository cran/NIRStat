\name{MAUCtest}
\alias{MAUCtest}
\title{
MAUC statistics based Analysis for NIRS time series.  
}
\description{
Estimate the Mean Area Under the Curve (MAUC) statistics and conduct a nonparametric test on the MAUC difference before transfuion and after trasfusion. If detection limit occurs at 15%, this function will impute the missed data based on uniform distribution and conduct a nested permutation approach within imputation for statistical testing. 
}
\usage{
MAUCtest(Yvec,timevec,transfusionvec,fig = T,SD_est=F,num.permu=1000)
}

\arguments{
  \item{Yvec}{The outcome of NIRS time series \eqn{Y(t_{i})} of length N ranging from 15 to 100.}
  \item{timevec}{The time index of NIRS time series \eqn{t_{i}} of length N.}
  \item{transfusionvec}{The 0/1 indicator of the transfusion status \eqn{X(t_{i})}. \eqn{X(t_{i})=0} means the current time point is before transfusion and \eqn{X(t_{i})=1} means the current time point is after transfusion.}
  \item{fig}{Whether to plot the NIRS time series. Default value is TRUE.}
  \item{SD_est}{Whether to estimate the SD of the MAUC statistic for pre-transfusion and post-transfuion. Default value is FALSE.}
  \item{num.permu}{Number of permutation for permutation test. Default value is 1000.}
}

\details{
  This functinon estimates the Mean Area Under the Curve (MAUC) statistics and conducts a permutation based test on the MAUC difference before transfuion and after trasfusion. If detection limit (DL) occurs (15), it will impute the missed data based on a uniform distribution and estimate the MAUC statistics through a standard imputation approach. The statistical testing is conducted through a nested permutation approach across all imputated datasets. 
}

\value{
  An R vector from MAUCtest containing MAUC statistics and Pvalue in the following order:
   \item{MAUC.before}{The estimated MAUC statistic before transfusion.}
   \item{MAUC.after}{The estimated MAUC statistic after transfusion.}
   \item{MAUC.diff}{The estimated MAUC statistic difference between before transfusion and after transfusion.}
   \item{Pvalue}{The pvalue of testing the MAUC difference to be zero or not.}
   \item{SD_pre}{SD of the MAUC statistic for pre-transfusion. Optional, only when \code{SD_est} = TRUE.}
   \item{SD_post}{SD of the MAUC statistic for post-transfusion. Optional, only when \code{SD_est} = TRUE.}
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


# Estimate the MAUC statistics of the NIRS data and test on the difference. 
MAUCtest(dat$Y,dat$t,dat$trans,TRUE,FALSE,100)
}