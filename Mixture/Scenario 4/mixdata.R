
## Use this to generate the data!!!

mix_data=function(n,t){
  
  ### mixture proportion
  p0=.8;pv=pt=(1-p0)/2
  
  ## sampling 
  ind=sample(0:2,n,replace = T,prob = c(p0,pv,pt))
  
  X1=rnorm(n)
  mv=(ind==1);mt=(ind==2)
  
  theta=sin(1*(1:n))
  ## behavior policy
  pi1=.5
  a1=rbinom(n,1,pi1)
  
  ## reward function
  r1=rnorm(n,mean=2*X1+3*a1,sd=.5)+theta+cos(3)
  
  X=r=a=list();X[[1]]=X1; r[[1]]=r1;a[[1]]=a1
  
  ## from time 2 to time t: loop
  
  for (j in 2:t) {
    Xprev=X[[j-1]];aprev=a[[j-1]]
    ind=sample(0:2,n,replace = T,prob = c(p0,pv,pt))
    
    ## transition probability
    tran=-.25*Xprev+aprev
    Xnext=rnorm(n,tran)
    
    mv=(ind==1);mt=(ind==2)
    
    Xnext[mv]=rnorm(sum(mv),theta[mv])
    Xnext[mt]=rnorm(sum(mt),cos(j))
    
    anext=rbinom(n,1,pi1)
    rnext=rnorm(n,mean=2*Xnext+3*anext,sd=.5)+theta+cos(3*j)
    X[[j]]=Xnext;r[[j]]=rnext;a[[j]]=anext}
  
  list(X,r,a)
}




