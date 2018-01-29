
## MAUCtest estimates the MAUC (difference) statisitc for NIRS time series before and after transfusion. ##

# Inputs: #
# Yvec :          a vector of the outcome Y(t_i);
# timevec :       a vector of the time t_i ;
# transfusionvec: a vector of indicating whether the current time point is before (0) transfusion or after tranfusion (1);

# Outputs: #
# a vector of (MAUC before transfusion, MAUC after transfusion, MAUC difference, p-value);

MAUCtest <- function(Yvec,timevec,transfusionvec,fig = T,SD_est=F,num.permu=1000)
{
  if(fig){ plotNIRS(Yvec,timevec,transfusionvec) }
  
  dat = data.frame(Gut = Yvec, tmsSec = timevec, Trans= transfusionvec)
  dat = dat[!is.na(dat$Gut),]
  
  alpha = 0.3
  if(sum(dat$Gut==15,na.rm = T)>0)
  {
    Num = 10
    AUC.pre.imput = AUC.pos.imput = AUC.diff.imput = AUC.var.pre = AUC.var.pos = vector()
    dat111 = dat222 = list()
    
    # First Imputed $Num$ Datasets for Detecetion Limits
    for(i in 1:Num)
    {
      dat$Gut_MI = dat$Gut
      dat$Gut_MI[(dat$Gut_MI==15)] = runif(sum((dat$Gut_MI==15)),0,15)
      
      AUC.pre.imput[i] = mean(predict(loess(Gut_MI~tmsSec, data = dat[dat$Trans==0,],span=alpha ))) # pre-transfusion
      AUC.pos.imput[i] = mean(predict(loess(Gut_MI~tmsSec, data = dat[dat$Trans==1,],span=alpha ))) # post-transfusion
      AUC.diff.imput[i] = AUC.pre.imput[i] - AUC.pos.imput[i]
      
      dat111[[i]] = dat[dat$Trans==0,]
      dat111[[i]]$Gut = dat111[[i]]$Gut_MI
      dat222[[i]] = dat[dat$Trans==1,]
      dat222[[i]]$Gut = dat222[[i]]$Gut_MI      
    }
    
    
    # Permute on the imputed datasets for pvalue
    AUC.diff.perm = vector()
    for(Permu in 1:num.permu)
    {
      AUC.pre.perm = AUC.pos.perm = vector()
      for(i in 1:Num)
      {
        # Permute across all bands
        datiii = rbind(dat111[[i]],dat222[[i]])
        datiii$Gut = datiii$Gut[sample(1:dim(datiii)[1])]
        
        dat.perm1 = datiii[1:dim(dat111[[i]])[1],]
        dat.perm2 = datiii[-c(1:dim(dat111[[i]])[1]),]
        
        AUC.pre.perm[i] = mean(predict(loess(Gut~tmsSec, data = dat.perm1,span=alpha))) # pre-transfusion
        AUC.pos.perm[i] = mean(predict(loess(Gut~tmsSec, data = dat.perm2,span=alpha))) # post-transfusion
      }
      
      AUC.diff.perm[Permu] = mean(AUC.pre.perm - AUC.pos.perm)
    }
    
    AUC.pval = sum(abs(AUC.diff.perm)>abs(mean(AUC.diff.imput)))/num.permu
	
	if(SD_est)
	{
		SD_pre  = sqrt(MAUC_var( dat$Gut[dat$Trans==0], dat$tmsSec[dat$Trans==0] ))
		SD_post = sqrt(MAUC_var( dat$Gut[dat$Trans==1], dat$tmsSec[dat$Trans==1] ))
		return(c(mean(AUC.pre.imput),mean(AUC.pos.imput),-mean(AUC.diff.imput),AUC.pval,SD_pre,SD_post) )
	}else
	{
		return(c(mean(AUC.pre.imput),mean(AUC.pos.imput),-mean(AUC.diff.imput),AUC.pval))
	}
    
  }else{
    
    AUC.pre = mean(predict(loess(Gut~tmsSec, data = dat[dat$Trans==0,],span=alpha))) # pre-transfusion
    AUC.pos = mean(predict(loess(Gut~tmsSec, data = dat[dat$Trans==1,],span=alpha))) # post-transfusion
    AUC.diff = AUC.pos - AUC.pre
    # Permutation Test for AUC.diff
    AUC.diff.perm = vector()
    num.pre = sum(dat$Trans==0)
    for(Permu in 1:num.permu)
    {
      dat.perm = dat
      dat.perm$Gut = dat.perm$Gut[sample(1:dim(dat.perm)[1])]
      AUC.pre.perm = mean(predict(loess(Gut~tmsSec, data = dat.perm[1:num.pre,],    span=alpha))) # pre-transfusion
      AUC.pos.perm = mean(predict(loess(Gut~tmsSec, data = dat.perm[-c(1:num.pre),],span=alpha))) # post-transfusion
      AUC.diff.perm[Permu] = AUC.pre.perm - AUC.pos.perm
    }
	
	if(SD_est)
	{
		SD_pre  = sqrt(MAUC_var( dat$Gut[dat$Trans==0], dat$tmsSec[dat$Trans==0] ))
		SD_post = sqrt(MAUC_var( dat$Gut[dat$Trans==1], dat$tmsSec[dat$Trans==1] ))
		return(c(AUC.pre,AUC.pos,AUC.diff, sum(abs(AUC.diff.perm)>abs(AUC.diff))/num.permu,SD_pre,SD_post))
	}else
	{
		return(c(AUC.pre,AUC.pos,AUC.diff, sum(abs(AUC.diff.perm)>abs(AUC.diff))/num.permu))
	}
  }
}
