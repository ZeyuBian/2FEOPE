setwd("~/Desktop/JASA_Sims/Mixture")
source('dgp.R')

for (qq in 1:4) {
  
  dgp(paste('Scenario', qq))

t0=5
mmw1=mmw2=mmw3=mmw4=c()
gam=.9

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


for (z in 1:200) {
  set.seed(z)
  
  data=mix_data(n,t)
  X=do.call(c,data[[1]]);R=do.call(c,data[[2]]);r=R[(1:((t-1)*n))]
  x=X[(n+1):(t*n)];xprev=X[(1:((t-1)*n))]
  
  pi1=.8;pi0=1-pi1
  
  a0=do.call(c,data[[3]]);aprev=a0[(1:((t-1)*n))];a=a0[(n+1):(t*n)]
  
  x=cbind(a,x,x^2)
  xprev=cbind(aprev,xprev,xprev^2)
  
  D=(pi1*a+pi0*(1-a))*x
  Dprev=(pi1*a+pi0*(1-a))*xprev
  
  PP=t(xprev)%*%(xprev)-gam*t(xprev)%*%D
  
  beta=(1-gam)*solve(PP)%*%t(Dprev)%*%r
  
  par=mean(cbind(a0,X,X^2)%*%beta*R)
  
  mmw1[z]=(par-v)^2
  
  mmw2[z]=crossprod(par-vi)/n
  
  mmw3[z]=crossprod(par-vt)/t0
  
  mmw4[z]=crossprod(par-c(vit))/(n*t0)
  
}


print(paste('Scenario', qq))
print(round(c(mean(mmw1),mean(mmw2),mean(mmw3),mean(mmw4)),2))
}
