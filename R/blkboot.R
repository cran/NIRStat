# block-bootstrap a time series for SD estimation
blkboot <- function(ts,l=NULL,R=20)
{
  N = length(ts) # length of ts
  if(is.null(l)) l = round(N^(1/3)) # length of block
  
  Bn = ceiling(N/l)
  boots = matrix(0,ncol=R,nrow=N)
  boots = apply(boots,2,function(x)
  {
    y = NULL
    btid = sample(1:(N-l+1),size=Bn,replace=T)
    i = 1
    while(i <=length(btid))
    {
      y = c(y,ts[btid[i]:(btid[i]+l-1)]); i = i+1
    }
    return(y[1:N])
  })
  return(boots)
}
