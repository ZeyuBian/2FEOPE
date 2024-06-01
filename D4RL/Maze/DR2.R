
library(truncnorm)
task=c('open','umaze','medium', 'large')

for (name in task) {
  TW1=TW2=TW3=TW4=c()
  for (z in 1:20) {
    load(paste("~/Desktop/JASA_Sims/D4RL/D4RL_Data/maze/",name,z,'.RData',sep = ''))
    
    n=20;t=20;t0=5
    MC <- read.csv(paste("~/Desktop/JASA_Sims/D4RL/D4RL_Data/D4RL_MC/maze/maze_",name,'_MC.csv',sep = ''))
    
    MC=MC$reward/500
    vit=matrix(MC,nrow = 40)
    vit=vit[1:n,]
  
  vi=rowMeans(vit)
  
  vt=colMeans(vit)[(1:t0)]
  
  v=mean(vit)
  
  vit=vit[,(1:t0)]
dima=dim(data[[2]][[1]])[1]
dimx=dim(data[[1]][[1]])[1]

a=do.call(rbind,data[[2]]);a=c(a)
a=matrix(a,  nrow = dima); a=t(a)

x=do.call(rbind,data[[1]]);x=c(x)
x=matrix(x,  nrow = dimx); x=t(x)

r=do.call(rbind,data[[3]]);r=c(r)


X=x;A=a;R=r

par=c()
ws=1
pi=(1/2)^dima
par=c()
weights=matrix(NA,nrow = n, ncol = t)
wsmatrix=matrix(NA,nrow = n, ncol = t)
w=1
for (h in 1:t) {
  ps=1
  x=X[((h-1)*n+1): (h*n),]
  a=A[((h-1)*n+1): (h*n),]
  
  for (l in 1:dima) {
    m=lm(a[,l]~x)
    sig=sd(m$residuals)
    ps=ps*dtruncnorm(a[,l],a=-1,b=1,mean=fitted(m),sd=sig)
  }
  w=pi/ps
  weights[,h]=w
  m=lm(w~x)
  wsmatrix[,h]=fitted(m)
}

## trimming
weights[weights>10]=10
wsmatrix[wsmatrix>10]=10
wsmatrix[wsmatrix<0]=0

omega=weights*wsmatrix
omega[omega>10]=10

## outcome learning
x=X;a=A
m=lm(c(r)~cbind(x,x^2,a,a^2))
par=coef(m)

q=cbind(a,a^2);q[,(1:dima)]=0;q[,-(1:dima)]=2/3

res=matrix(nrow=n,ncol=t0)
r0=cbind(1,x,x^2,q)%*%par* (1/2)^dima

res=matrix(nrow=n,ncol=t0)

r=c(r0)+c(omega)*(c(r)-fitted(m))
## clipping
r[r>10*max(R)]=max(R)
r[r<0.1*min(R)]=min(R)

r=matrix(r,nrow = n)
res[,1]=r[,1]
for (i in (2:t0)) {
  x=X[(1:((t-i+1)*n)),];a=A[(1:((t-i+1)*n)),];r=c(r[,-1])
  wsmatrix=wsmatrix[,-ncol(wsmatrix)]
  weights=weights[,-1]
  
  m=lm(c(wsmatrix*weights)~x)
  wsmatrix=matrix(fitted(m),nrow = n)
  wsmatrix[wsmatrix>10]=10
  m=lm(c(r)~cbind(x,x^2,a,a^2))
  par=coef(m)
  
  q=cbind(a,a^2);q[,(1:dima)]=0;q[,-(1:dima)]=2/3
  r0=cbind(1,x,x^2,q)%*%par*(1/2)^dima
  omega=wsmatrix*weights
  omega[omega>10]=10
  r=c(r0)+c(omega)*(r-fitted(m))
  ## clipping 
  r[r>10*max(R)]=max(R)
  r[r<0.1*min(R)]=min(R)
  r=matrix(r,nrow = n)
  res[,i]=r[,1]
}

TW1[z]=(mean(res)-v)^2

TW2[z]=crossprod(rowMeans(res)-vi)/n

TW3[z]=crossprod(colMeans(res)-vt)/t0

TW4[z]=sum((res-vit)^2)/(n*t0)
  }
  print(paste('maze-', name,sep=''))
  print(round(c(mean(TW1),mean(TW2),mean(TW3),mean(TW4)),2))
  
}
