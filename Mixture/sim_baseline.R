sim_state_base=function(n,t,X1,B,P,psi,beta){
  a1=rbinom(n,1,0.8)
  r1=rnorm(n,mean = X1*beta+a1*psi)
  X=r=a=list();X[[1]]=X1; r[[1]]=r1;a[[1]]=a1
  for (i in (2:t)){
    Xprev=X[[i-1]];aprev=a[[i-1]]
    Xnext=Xprev*B+P*aprev
    anext=rbinom(n,size = 1,prob = 0.8)
    rnext=rnorm(n,mean = (Xnext*beta+anext*psi))
    X[[i]]=Xnext;r[[i]]=rnext;a[[i]]=anext
  }
  r
}




