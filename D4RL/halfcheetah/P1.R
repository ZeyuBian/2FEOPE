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
  
  X=x;A=a
  
  H=I=diag(n)
  for (k in 1:(t-1)) {
    H=rbind(H,I)
  }
  
  G=L=matrix(0,nrow = n,ncol = t);G[,1]=1
  
  for (k in 2:(t)) {
    M=L
    M[,k]=1
    G=rbind(G,M)
  }
  H=H[,-1];G=G[,-1];D=cbind(H,G)
  
  m=lm(c(r)~cbind(x,x^2,a,a^2,D))
  par=coef(m)
  
  q=cbind(a,a^2);q[,(1:dima)]=0;q[,-(1:dima)]=2/3 * (1/2)^dima
  
  res=matrix(nrow=n,ncol=t0)
  r=cbind(1,x,x^2,q,D)%*%par
  r=matrix(r,nrow = n)
  res[,1]=r[,1]
  
  for (i in (2:t0)) {
    x=X[(1:((t-i+1)*n)),];a=A[(1:((t-i+1)*n)),];r=c(r[,-1])
    
    H=I=diag(n)
    for (k in 1:(t-i)) {
      if (i==t){
        break
      }
      H=rbind(H,I)
    }
    
    G=L=matrix(0,nrow = n,ncol = t-i+1);G[,1]=1
    
    for (k in 2:(t-i+1)) {
      if (i==t){
        break
      }
      M=L
      M[,k]=1
      G=rbind(G,M)
    }
    
    H=H[,-1];G=G[,-1]
    D=cbind(H,G)
    if (i==t0) {
      D=H
    }
    m=lm(c(r)~cbind(x,x^2,a,a^2,D))
    par=coef(m)
    
    q=cbind(a,a^2);q[,(1:dima)]=0;q[,-(1:dima)]=2/3 * (1/2)^dima
    
    
    r=cbind(1,x,x^2,q,D)%*%par
    r=matrix(r,nrow = n)
    res[,i]=r[,1]
  }
  TW1[z]=(mean(res)-v)^2
  
  TW2[z]=crossprod(rowMeans(res)-vi)/n
  
  TW3[z]=crossprod(colMeans(res)-vt)/t0
  
  TW4[z]=sum((res-vit)^2)/(n*t0)
}
print(paste('halfcheetah-', name,sep=''))
print(round(c(mean(TW1),mean(TW2),mean(TW3),mean(TW4)),2))

}


