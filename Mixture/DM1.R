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

fq1=fq2=fq3=fq4=c()

for (z in 1:200) {
  set.seed(z)
  
  data=mix_data(n,t)
  X=do.call(cbind,data[[1]]);R=do.call(cbind,data[[2]])
  
  pi1=.8;pi0=1-pi1
  
  A=do.call(cbind,data[[3]])
  x=c(X);r0=c(R);a=c(A)
  
  m=lm(r0~cbind(x,x^2,a))
  par=coef(m)
  
  Q1=cbind(1,x,x^2,1)%*%par
  Q0=cbind(1,x,x^2,0)%*%par
  
  res=matrix(nrow=n,ncol=t0)
  r=pi1*Q1+pi0*Q0
  r=matrix(r,nrow = n)
  res[,1]=r[,1]
  
  for (i in (2:t0)) {
    x=c(X[,1:(t-i+1)]);a=c(A[,1:(t-i+1)]);r=c(r[,-1]);r0=c(R[,1:(t-i+1)])
    m=lm(r~cbind(x,x^2,a))
    par=coef(m)
    Q1=cbind(1,x,x^2,1)%*%par
    Q0=cbind(1,x,x^2,0)%*%par
    r=pi1*Q1+pi0*Q0
    r=matrix(r,nrow = n)
    res[,i]=r[,1]
  }
  
  t0=5
  
  fq1[z]=(mean(res)-v)^2
  
  fq2[z]=crossprod(mean(res)-vi)/n
  
  fq3[z]=crossprod(colMeans(res)-vt)/t0
  
  fq4[z]=sum((colMeans(res)-vit)^2)/(n*t0)
  
}


print(paste('Scenario', qq))
print(round(c(mean(fq1),mean(fq2),mean(fq3),mean(fq4)),2))
}


