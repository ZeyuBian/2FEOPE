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


t0=5
mis1=mis2=mis3=mis4=c()
pi1=0.8
for (z in 1:200) {
  set.seed(z)
  data=mix_data(n,t)
  X=do.call(c,data[[1]]);R=do.call(c,data[[2]]); A=do.call(c,data[[3]])
  par=c()
  ws=1
  for (h in 1:t0) {
    pi=rep(pi1,t)
    x=X[((h-1)*n+1): (h*n)]
    a=A[((h-1)*n+1): (h*n)]
    m=glm(a~x,binomial)
    r=R[((h-1)*n+1): (h*n)]
    ps=fitted(m)
    ps[a==0]=1-ps[a==0]
    pi[a==0]=1-pi[a==0]
    w=ws*pi/ps
    m=lm(w~x)
    ws=fitted(m)
    w=ws*pi/ps
    w[w>=10]=10
    par[h]=mean(w*r)
  }
  
  mis1[z]=(mean(par)-v)^2
  
  mis2[z]=crossprod(mean(par)-vi)/n
  
  mis3[z]=crossprod(par-vt)/t0
  
  mis4[z]=crossprod(par-c(vit))/(n*t0)
  
}
print(paste('Scenario', qq))
print(round(c(mean(mis1),mean(mis2),mean(mis3),mean(mis4)),2))
}

