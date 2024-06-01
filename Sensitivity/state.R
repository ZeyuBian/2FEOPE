sim_state=function(n,t,X1,B,P,p0,pv,pt,
                   mui,mut,sig0,sig1,sig2){
  a1=rbinom(n,1,0.8)
  ind=sample(0:2,n,replace = T,prob = c(p0,pv,pt))
  mv=(ind==1);mt=(ind==2)
  
  X=a=list();X[[1]]=X1;a[[1]]=a1
  
  ## from time 2 to time t: loop
  
  for (j in 2:t) {
    Xprev=X[[j-1]];aprev=a[[j-1]]
    ind=sample(0:2,n,replace = T,prob = c(p0,pv,pt))
    
    ## transition probability
    tran=B*Xprev+aprev*P
    Xnext=rnorm(n,tran,sig0)
    
    mv=(ind==1);mt=(ind==2)
    
    Xnext[mv]=rnorm(sum(mv),mean=mui[mv],sd=sig1)
    Xnext[mt]=rnorm(sum(mt),mean=mut[j-1],sd=sig2)
    
    anext=rbinom(n,1,.8)
    X[[j]]=Xnext;a[[j]]=anext}
  list(X,a)
}
