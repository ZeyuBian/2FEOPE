setwd("~/Desktop/JASA_Sims/Mixture")

source('dgp.R')

for (qq in 1:4) {
  
  dgp(paste('Scenario', qq))

t0=5

drl1=drl2=drl3=drl4=c()
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
  
  ## weight learning
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
  
  omega=cbind(a0,X,X^2)%*%beta
  omega=omega[1:(n*(t-1))]
  
  ## Q-function
  X=do.call(c,data[[1]]);r=do.call(c,data[[2]]);r=r[(1:((t-1)*n))]
  x=X[(n+1):(t*n)];xprev=X[(1:((t-1)*n))]
  
  pi1=.8;pi0=1-pi1
  
  a0=do.call(c,data[[3]]);a=a0[(1:((t-1)*n))]
  
  a0=a0[(n+1):(t*n)]
  
  x=cbind(x,x^2)
  xprev=cbind(xprev,xprev^2)
  
  x2=x[,-1]
  xprev2=xprev[,-1]
  
  D1=cbind(1,-pi1*x+a*xprev,-pi0*x2+(1-a)*xprev2)
  D2=cbind(1,a*xprev,(1-a)*xprev2)
  PP=t(D2)%*%D1
  
  par=solve(PP)%*%t(D2)%*%r
  beta=par[-1]
  
  par=mean(omega*(r+D1[,-1]%*%beta))
  
  drl1[z]=(par-v)^2
  
  drl2[z]=crossprod(par-vi)/n
  
  drl3[z]=crossprod(par-vt)/t0
  
  drl4[z]=crossprod(par-c(vit))/(n*t0)
  
}

print(paste('Scenario', qq))
print(round(c(mean(drl1),mean(drl2),mean(drl3),mean(drl4)),2))}

