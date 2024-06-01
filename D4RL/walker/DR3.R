
library(truncnorm)
task=c('medium', 'noisy','mixed','medium_expert')

for (name in task) {
  TW1=TW2=TW3=TW4=c()
  for (z in 1:20) {
    load(paste("~/Desktop/JASA_Sims/D4RL/D4RL_Data/walker/",name,z,'.RData',sep = ''))
    
    n=20;t=20;t0=5
    MC <- read.csv(paste("~/Desktop/JASA_Sims/D4RL/D4RL_Data/D4RL_MC/walker/walker_",name,'_MC.csv',sep = ''))
    
    MC=MC$reward/500
    vit=matrix(MC,nrow = 40)
    vit=vit[1:n,]
  
  vi=rowMeans(vit)
  
  vt=colMeans(vit)[(1:t0)]
  
  v=mean(vit)
  
  vit=vit[,(1:t0)]
  
  gam=0.9
  dima=dim(data[[2]][[1]])[1]
  dimx=dim(data[[1]][[1]])[1]
  
  a=do.call(rbind,data[[2]]);a=c(a)
  a=matrix(a,  nrow = dima); a=t(a)
  
  x=do.call(rbind,data[[1]]);x=c(x)
  x=matrix(x,  nrow = dimx); x=t(x)
  X=x;A=a
  
  r=do.call(rbind,data[[3]]);r=c(r)
  r=r[(1:((t-1)*n))]
  
  x=X[(n+1):(t*n),];xprev=X[(1:((t-1)*n)),]
  
  a=A[(1:((t-1)*n)),]
  
  x=cbind(x,x^2)
  xprev=cbind(xprev,xprev^2)
  
  qq=matrix(0,nrow = (t-1)*n,ncol = 2*dima)
  qq[,-(1:dima)]=(1/2)^dima* 2/3
  
  
  Dpi=cbind(x*(1/2)^dima,qq); D=cbind(xprev,a,a^2)
  
  PP=t(D)%*%(D)-gam*t(D)%*%Dpi
  
  beta=(1-gam)*solve(PP)%*%t(Dpi)%*%r
  
  omega=D%*%beta
  omega=omega[1:(n*(t-1))]
  
  ## Q-function
  x=x[,-1]
  xprev=xprev[,-1]
  
  D=cbind(1,-x*(1/2)^dima+xprev,a,-2/3* (1/2)^dima+a^2)
  
  par=solve(t(D)%*%D)%*%t(D)%*%r
  
  beta=par[-1]
  
  par=mean(omega*(r+D[,-1]%*%beta))
  
  TW1[z]=(par-v)^2
  
  TW2[z]=crossprod(par-vi)/n
  
  TW3[z]=crossprod(par-vt)/t0
  
  TW4[z]=sum((par-vit)^2)/(n*t0)
  }
  print(paste('walker-', name,sep=''))
  print(round(c(mean(TW1),mean(TW2),mean(TW3),mean(TW4)),2))
  
} 