# MAUC variance estimation based on block-bootstrap approach.
MAUC_var<-function(Y,tmsSec,Nboot=50,Num=10,alpha=0.3)
{
  Gut = Y
  if(sum(Gut==15,na.rm = T)>0)
  {
	  MAUC_imput = MAUC_withinvar = vector()
	  for(i in 1:Num)
	  {
		Gut_MI = Gut
		Gut_MI[(Gut_MI==15)] = runif(sum((Gut_MI==15)),0,15)
		Gut_hat = predict(loess(Gut_MI~tmsSec, span=alpha ))
		MAUC_imput[i] = mean(Gut_hat) 
		Bts = blkboot(Gut_MI,R=Nboot)
		MAUC_withinvar[i] = var(apply(Bts,2,function(x){ y = x; mean(predict(loess(y~tmsSec, span=alpha )))    }))
	  }
	  return( var(MAUC_imput)*(1+1/Num) + mean(MAUC_withinvar) )
  }else
  {
	  Bts = blkboot(Gut,R=Nboot)
	  return(  var(apply(Bts,2,function(x){ y = x; mean(predict(loess(y~tmsSec, span=alpha )))    }))   )
  }
}