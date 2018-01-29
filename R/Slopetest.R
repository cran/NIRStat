
## Slopetest estimates the slope (difference) statisitc for NIRS time series before and after transfusion. ##

# Inputs: #
# Yvec :          a vector of the outcome Y(t_i);
# timevec :       a vector of the time t_i ;
# transfusionvec: a vector of indicating whether the current time point is before (0) transfusion or after tranfusion (1);

# Outputs: #
# a vector of (slope before transfusion, slope after transfusion, slope difference, p-value)


Slopetest <- function(Yvec,timevec,transfusionvec,SD_est=F,num.permu=1000)
{
  dat = data.frame(Gut = Yvec, tmsSec = timevec, Trans= transfusionvec)
  dat = dat[!is.na(dat$Gut),]
  
  if(sum(dat$Gut==15)>0 ) # If Detection Limit happens
  {
    Num = 10
    Slopebefore.imput = Slopeinter.imput = vector(); dat_permu = list()
    
    # First Imputed $Num$ Datasets for Detecetion Limits
    for(i in 1:Num)
    {
      dat$Gut_MI = dat$Gut
      dat$Gut_MI[(dat$Gut_MI==15)] = runif(sum((dat$Gut_MI==15)),0,15)
      dat$Gut_MI_smooth = 0
      dat[dat$Trans==0,]$Gut_MI_smooth = predict(loess(Gut_MI~tmsSec, data = dat[dat$Trans==0,],span=.3)) # pre-transfusion
      dat[dat$Trans==1,]$Gut_MI_smooth = predict(loess(Gut_MI~tmsSec, data = dat[dat$Trans==1,],span=.3)) # post-transfusion
      Slopeinter.imput[i] = coef(lm(Gut_MI_smooth ~ tmsSec+Trans+Trans*tmsSec,data=dat))[4]
      Slopebefore.imput[i] = coef(lm(Gut_MI_smooth ~ tmsSec+Trans+Trans*tmsSec,data=dat))[2]
      dat_permu[[i]] = dat
      dat_permu[[i]]$Gut = dat_permu[[i]]$Gut_MI
    }
    
    # Permute on the imputed datasets for pvalue
    Slopeinter.perm = vector()
    for(Permu in 1:num.permu)
    {
      Slopeinter.perm2 = vector()
      for(i in 1:Num)
      {
        datiii = dat_permu[[i]]
        datiii$Gut[datiii$Trans==0] = sample(datiii$Gut[datiii$Trans==0])
        datiii$Gut[datiii$Trans==1] = sample(datiii$Gut[datiii$Trans==1])
        
        datiii[datiii$Trans==0,]$Gut = predict(loess(Gut~tmsSec, data = datiii[datiii$Trans==0,],span=.3)) # pre-transfusion
        datiii[datiii$Trans==1,]$Gut = predict(loess(Gut~tmsSec, data = datiii[datiii$Trans==1,],span=.3)) # post-transfusion
        Slopeinter.perm2[i] = coef(lm(Gut ~ tmsSec+Trans+Trans*tmsSec,data=datiii))[4]
      }
      Slopeinter.perm[Permu] = mean(Slopeinter.perm2)
    }
    Slopeinter.pval = sum(abs(Slopeinter.perm)>abs(mean(Slopeinter.imput)))/1000
    resu.tab = c(mean(Slopebefore.imput), mean(Slopebefore.imput+Slopeinter.imput), mean(Slopeinter.imput), Slopeinter.pval)
    
  }else
  {
    dat$Gutsm = 0
    dat[dat$Trans==0,]$Gutsm = predict(loess(Gut~tmsSec, data = dat[dat$Trans==0,],span=0.3)) # Pre
    dat[dat$Trans==1,]$Gutsm = predict(loess(Gut~tmsSec, data = dat[dat$Trans==1,],span=0.3)) # Post
    Slopebefore.imput = coef(lm(Gutsm ~ tmsSec+Trans+Trans*tmsSec,data=dat))[2]
    Slopeinter.imput = coef(lm(Gutsm ~ tmsSec+Trans+Trans*tmsSec,data=dat))[4]
    Slopeinter.perm = vector()
    for(Permu in 1:1000)
    {
      datiii = dat
      # Permute with each band
      datiii$Gut[datiii$Trans==0] = sample(datiii$Gut[datiii$Trans==0]) 
      datiii$Gut[datiii$Trans==1] = sample(datiii$Gut[datiii$Trans==1])
      
      datiii$Gutsm = 0
      datiii[datiii$Trans==0,]$Gutsm = predict(loess(Gut~tmsSec, data = datiii[datiii$Trans==0,],span=0.3)) # Pre
      datiii[datiii$Trans==1,]$Gutsm = predict(loess(Gut~tmsSec, data = datiii[datiii$Trans==1,],span=0.3)) # Post
      Slopeinter.perm[Permu] =  coef(lm(Gutsm ~ tmsSec+Trans+Trans*tmsSec,data=datiii))[4]
    }
    Slopeinter.pval =(sum(abs(Slopeinter.perm)>abs((Slopeinter.imput)))/1000)
    resu.tab = c(Slopebefore.imput, Slopebefore.imput+Slopeinter.imput,Slopeinter.imput, Slopeinter.pval)
  }
  
  if(SD_est)
  {
	SD_pre  = sqrt(SLOPE_var( dat$Gut[dat$Trans==0], dat$tmsSec[dat$Trans==0] ))
	SD_post = sqrt(SLOPE_var( dat$Gut[dat$Trans==1], dat$tmsSec[dat$Trans==1] ))
	return(c(resu.tab,SD_pre,SD_post))
  }else
  {
    return(resu.tab)
  }
	
  
  
}

