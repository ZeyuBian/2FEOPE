setwd("~/Desktop/JASA_Sims/Mixture")
source('dgp.R')

for (qq in 1:4) {
  
  dgp(paste('Scenario', qq))
n=40;t=40;t0=5
set.seed(24)
value=matrix(0,nrow = n,ncol = t)
for (e in 1:400) {
  v=target(n,t)
  v=do.call(cbind,v)
  value=value+v
}

vit=value[,(1:t0)]/400
vt=colMeans(value)[1:t0]/400
vi=rowMeans(value)/400
v=mean(value)/400



shi1=shi2=shi3=shi4=c()

for (z in 1:200) {
  set.seed(z)
  
  data=mix_data(n,t)
  X=do.call(c,data[[1]]);r=do.call(c,data[[2]]);r=r[(1:((t-1)*n))]
  x=X[(n+1):(t*n)];xprev=X[(1:((t-1)*n))]
  
  pi1=.8;pi0=1-pi1
  
  a0=do.call(c,data[[3]]);a=a0[(1:((t-1)*n))]
  
  x=cbind(x,x^2)
  xprev=cbind(xprev,xprev^2)
  
  
  ### base 1 method
  x2=x[,-1]
  xprev2=xprev[,-1]
  
  D1=cbind(1,-pi1*x+a*xprev,-pi0*x2+(1-a)*xprev2)
  D2=cbind(1,a*xprev,(1-a)*xprev2)
  
  PP=t(D2)%*%D1
  
  par=solve(PP)%*%t(D2)%*%r
  par=par[1]
  
  shi1[z]=(par-v)^2
  
  shi2[z]=crossprod(par-vi)/n
  
  shi3[z]=crossprod(par-vt)/t0
  
  shi4[z]=crossprod(par-c(vit))/(n*t0)
  
}


print(paste('Scenario', qq))
print(round(c(mean(shi1),mean(shi2),mean(shi3),mean(shi4)),2))
}
