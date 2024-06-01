library(truncnorm)
task=c('medium', 'noisy','mixed','medium_expert')
for (name in task) {
  TW1=TW2=TW3=TW4=c()
  for (z in 1:20) {
    load(paste("~/Desktop/JASA_Sims/D4RL/D4RL_Data/halfcheetah/",name,z,'.RData',sep = ''))
    
    n=20;t=20;t0=5
    MC <- read.csv(paste("~/Desktop/JASA_Sims/D4RL/D4RL_Data/D4RL_MC/halfcheetah/halfcheetah_",name,'_MC.csv',sep = ''))
    
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
r=matrix(r,nrow=n)

X=x;A=a;R=r

par=c()
w=1
for (h in 1:t0) {
  ps=1
  x=X[((h-1)*n+1): (h*n),]
  a=A[((h-1)*n+1): (h*n),]
  
  r=R[((h-1)*n+1): (h*n)]
  
  for (l in 1:dima) {
    m=lm(a[,l]~x)
    sig=sd(m$residuals)
    ps=ps*dtruncnorm(a[,l],a=-1,b=1,mean=fitted(m),sd=sig)
  }
  
  w=w*(1/2)^dima / ps
  ### trimming
  w[w>10]=10
  par[h]=mean(w*r)
}
TW1[z]=(mean(par)-v)^2

TW2[z]=crossprod(mean(par)-vi)/n

TW3[z]=crossprod(par-vt)/t0

TW4[z]=sum((par-vit)^2)/(n*t0)
  }
  print(paste('halfcheetah-', name,sep=''))
  print(round(c(mean(TW1),mean(TW2),mean(TW3),mean(TW4)),2))
  
}



