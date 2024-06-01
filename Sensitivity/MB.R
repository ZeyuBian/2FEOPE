setwd("~/Desktop/JASA_Sims/Sensitivity")

source('state.R');source('dgp.R');source('sim_baseline.R')


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
mb1=mb2=mb3=mb4=c()

for (z in 1:200) {
  
  set.seed(z)
  data=mix_data(n,t)
  X=do.call(c,data[[1]]);X1=X[1:n];r=do.call(c,data[[2]])
  x=X[-(1:n)];xprev=X[(1:((t-1)*n))]
  a0=do.call(c,data[[3]]);a=a0[(1:((t-1)*n))]
  ## baseline
  base <- lm(r~ a0+X)
  psib=base$coefficients[2]
  betab=base$coefficients[3]
  
  tran=lm(x~xprev+a)
  Pb=tran$coefficients[3]
  Bb=tran$coefficients[2]
  
  
  
  
  ## generate state
  
  res=matrix(0,nrow = n,ncol = t)
  for (ii in 1:200) {
    M=sim_state_base(n = n,t = t,X1 = X1,B = Bb,
                     P = Pb,psi = psib,beta = betab)
    M=do.call(cbind,M)
    res=res+M
  }
  res=res/ii
  res=res[,1:t0]
  mb1[z]=(mean(res)-v)^2
  
  mb2[z]=crossprod(rowMeans(res)-vi)/n
  
  mb3[z]=crossprod(colMeans(res)-vt)/t0
  
  mb4[z]=sum((res-vit)^2)/(n*t0)
}
print(paste('Scenario', qq))
print(round(c(mean(mb1),mean(mb2),mean(mb3),mean(mb4)),2))

}



