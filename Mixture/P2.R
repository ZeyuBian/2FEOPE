setwd("~/Desktop/JASA_Sims/Mixture")
source('state.R');source('dgp.R')

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

em1=em2=em3=em4=c()

for (z in 1:200) {
  
  set.seed(z)
  data=mix_data(n,t)
  X=do.call(c,data[[1]]);X1=X[1:n];r=do.call(c,data[[2]])
  x=X[-(1:n)];xprev=X[(1:((t-1)*n))]
  a0=do.call(c,data[[3]]);a=a0[(1:((t-1)*n))]
  
  X=do.call(cbind,data[[1]])
  X=X[,-1]
  ## em!!! initial value
  
  pi0=0.5;pi1=pi2=(1-pi0)/2
  mui=rep(0,n);mut=rep(0,(t-1))
  sig1=sig2=sig0=0.2
  Psi=1
  
  B=-.5
  N=length(a)
  mu0=xprev*B+a*Psi
  
  p1=c()
  for (i in 1:n) {
    p1=c(p1,dnorm(X[i,],mui[i],sig1))
  }
  p1=pi1*p1
  
  p2=c()
  for (i in 1:(t-1)) {
    p2=c(p2,dnorm(X[,i],mut[i],sig2))
  }
  p2=pi2*p2
  
  p0=pi0*dnorm(x, mean=mu0,sd=sig0)
  p=p1+p0+p2
  gam0=p0/p; gam1=p1/p;gam2=p2/p
  n0=sum(gam0);n1=sum(gam1);n2=sum(gam2)
  
  muinew=rowSums(matrix(gam1*x,nrow=n))/rowSums(matrix(gam1,nrow=n))
  mutnew=colSums(matrix(gam2*x,nrow=n))/colSums(matrix(gam2,nrow=n))
  sig1new=sqrt(sum(gam1*(x-rep(muinew,(t-1)))^2)/sum(gam1))
  sig2new=sqrt(sum(gam2*(x-rep(mutnew,each=n))^2)/sum(gam2))
  
  Psinew=sum(gam0*(x-B*xprev)*a)/sum(gam0*a)
  Bnew=sum(gam0*(x-Psinew*a)*xprev)/sum(gam0*xprev^2)
  pi1new=n1/N;pi0new=n0/N;pi2new=n2/N
  mu0new=Psinew*a+Bnew*xprev
  sig0new=sqrt(sum(gam0*(x-mu0new)^2)/sum(gam0))
  
  k=1
  while (k<=20) {
    p1=c()
    for (i in 1:n) {
      p1=c(p1,dnorm(X[i,],muinew[i],sd=sig1new))
    }
    p1=pi1*p1
    
    p2=c()
    for (i in 1:(t-1)) {
      p2=c(p2,dnorm(X[,i],mutnew[i],sd=sig2new))
    }
    p2=pi2*p2
    
    p0=pi0*dnorm(x, mean=mu0new,sd=sig0new)
    p=p1+p0+p2
    gam0=p0/p; gam1=p1/p;gam2=p2/p
    n0=sum(gam0);n1=sum(gam1);n2=sum(gam2)
    
    muinew=rowSums(matrix(gam1*x,nrow=n))/rowSums(matrix(gam1,nrow=n))
    mutnew=colSums(matrix(gam2*x,nrow=n))/colSums(matrix(gam2,nrow=n))
    sig1new=sqrt(sum(gam1*(x-rep(muinew,(t-1)))^2)/sum(gam1))
    sig2new=sqrt(sum(gam2*(x-rep(mutnew,each=n))^2)/sum(gam2))
    
    Psinew=sum(gam0*(x-Bnew*xprev)*a)/sum(gam0*a)
    Bnew=sum(gam0*(x-Psinew*a)*xprev)/sum(gam0*xprev^2)
    pi1new=n1/N;pi0new=n0/N;pi2new=n2/N
    mu0new=Psinew*a+Bnew*xprev
    sig0new=sqrt(sum(gam0*(x-mu0new)^2)/sum(gam0))
    k=k+1
  }
  
  ### reward estimation
  
  X=do.call(cbind,data[[1]]);R=do.call(cbind,data[[2]])
  
  A=do.call(cbind,data[[3]])
  x=c(X);r=c(R);a=c(A)
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
  
  m=lm(r~cbind(x,x^2,a,D))
  par=coef(m)

  ## generate state
    
  res=matrix(0,nrow = n,ncol = t)
  for (ii in 1:200) {
    M=sim_state(n = n,t = t,X1 = X1,B = Bnew,
                P = Psinew,pv = pi1new,p0 = pi0new,mui = muinew,
                pt = pi2new,mut = mutnew,sig0=sig0new,sig1=sig1new,sig2=sig2new)
    
    X=do.call(cbind,M[[1]]);A=do.call(cbind,M[[2]])
    x=c(X);a=c(A)
    R=cbind(1,x,x^2,a,D)%*%par
    R=matrix(R,nrow = n)
    res=res+R
  }
  res=res/ii
  res=res[,1:t0]
  em1[z]=(mean(res)-v)^2
  
  em2[z]=crossprod(rowMeans(res)-vi)/n
  
  em3[z]=crossprod(colMeans(res)-vt)/t0
  
  em4[z]=sum((res-vit)^2)/(n*t0)
}
print(paste('Scenario', qq))
print(round(c(mean(em1),mean(em2),mean(em3),mean(em4)),2))

}





