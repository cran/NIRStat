# Slope variance estimation based on block-bootstrap
SLOPE_var<-function(Y,tmsSec,Nboot=50,Num=10,alpha=0.3)
{
  Gut = Y
  if(sum(Gut==15,na.rm = T)>0)
  {
	  Slope_imput = Slope_withinvar = vector()
	  for(i in 1:Num)
	  {
		Gut_MI = Gut
		Gut_MI[(Gut_MI==15)] = runif(sum((Gut_MI==15)),0,15)
		Gut_hat = predict(lm(Gut_MI~tmsSec))
		Slope_imput[i] = coef(lm(Gut_hat~tmsSec))[2] 
		Gut_res = Gut_MI - Gut_hat
		B_tres = blkboot(Gut_res,R=50)
		Slope_withinvar[i] = var(apply(B_tres,2,function(x){ y = x+Gut_hat; yhat = predict(loess(y~tmsSec, span=alpha )); slope = coef(lm(yhat~tmsSec))[2]    }))
	  }
	  return( var(Slope_imput)*(1+1/Num) + mean(Slope_withinvar) )
  }else
  {
      Gut_MI = Gut
	  Gut_hat = predict(lm(Gut_MI~tmsSec))
	  Gut_res = Gut_MI - Gut_hat
	  B_tres = blkboot(Gut_res,R=50)
	  return( var(apply(B_tres,2,function(x){ y = x+Gut_hat; yhat = predict(loess(y~tmsSec, span=alpha )); slope = coef(lm(yhat~tmsSec))[2]    })) )
  }
}