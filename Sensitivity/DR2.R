setwd("~/Desktop/JASA_Sims/Sensitivity")
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

tdr1=tdr2=tdr3=tdr4=c()
pi1=.8;pi0=1-pi1
for (z in 1:200) {
  set.seed(z)
  
  ## learning the ratio
  data=mix_data(n,t)
  X=do.call(c,data[[1]]);R=do.call(c,data[[2]]); A=do.call(c,data[[3]])
  par=c()
  weights=matrix(NA,nrow = n, ncol = t)
  wsmatrix=matrix(NA,nrow = n, ncol = t)

  for (h in 1:t) {
    
    pi=rep(pi1,t)
    x=X[((h-1)*n+1): (h*n)]
    a=A[((h-1)*n+1): (h*n)]
    m=glm(a~x,binomial)
    ps=fitted(m)
    ps[a==0]=1-ps[a==0]
    pi[a==0]=1-pi[a==0]
    w=pi/ps
    weights[,h]=w
    m=lm(w~x)
    ws=fitted(m)
    wsmatrix[,h]=ws
  }
  
  ## outcome learning
  
  X=do.call(cbind,data[[1]]);R=do.call(cbind,data[[2]])
  
  A=do.call(cbind,data[[3]])
  x=c(X);r0=c(R);a=c(A)
  
  m=lm(r0~cbind(x,x^2,a))
  par=coef(m)
  
  Q1=cbind(1,x,x^2,1)%*%par
  Q0=cbind(1,x,x^2,0)%*%par
  
  res=matrix(nrow=n,ncol=t0)
  
  r=pi1*Q1+pi0*Q0+c(weights*wsmatrix)*(r0-fitted(m))
  
  r=matrix(r,nrow = n)
  res[,1]=r[,1]
  for (i in (2:t0)) {
    r=c(r[,-1]);x=c(X[,(i:t)])
    wsmatrix=wsmatrix[,-ncol(wsmatrix)]
    weights=weights[,-1]
    
    m=lm(c(wsmatrix*weights)~x)
    
    wsmatrix=matrix(fitted(m),nrow = n)
    
    a=c(A[,1:(t-i+1)])
    x=c(X[,1:(t-i+1)])
    m=lm(r~cbind(x,x^2,a))
    par=coef(m)
    Q1=cbind(1,x,x^2,1)%*%par
    Q0=cbind(1,x,x^2,0)%*%par
    r=pi1*Q1+pi0*Q0+c(wsmatrix*weights)*(r-fitted(m))
    r[r>=max(R)]=max(R)
    r[r<min(R)]=min(R)
    r=matrix(r,nrow = n)
    res[,i]=r[,1]
  }
  
  t0=5
  
  tdr1[z]=(mean(res)-v)^2
  
  tdr2[z]=crossprod(mean(res)-vi)/n
  
  tdr3[z]=crossprod(colMeans(res)-vt)/t0
  
  tdr4[z]=sum((colMeans(res)-vit)^2)/(n*t0)
  
}
print(paste('Scenario', qq))
print(round(c(mean(tdr1),mean(tdr2),mean(tdr3),mean(tdr4)),2))
}


