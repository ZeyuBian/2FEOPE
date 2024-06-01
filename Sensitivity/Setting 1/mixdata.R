
## Use this to generate the data!!!

mix_data=function(n,t){
  
  iind=rep(c(T,F),n/2)
  X1=rnorm(n)
  theta=sin(1*(1:n))
  ## behavior policy
  pi1=.5
  a1=rbinom(n,1,pi1)
  
  ## reward function
  r1=2*X1+3*a1+theta+cos(1*3)+rnorm(n,sd=.5)
  
  X=r=a=list();X[[1]]=X1; r[[1]]=r1;a[[1]]=a1
  
  ## from time 2 to time t: loop
  
  for (j in 2:t) {
    Xprev=X[[j-1]];aprev=a[[j-1]]

    ## transition probability
    tran1=-.25*Xprev+aprev
    tran2=Xprev-aprev
    
    Xnext=rep(0,n)
    
    if (j%%2==1) {
      Xnext[iind]=rnorm(n/2,tran1)
      Xnext[!iind]=rnorm(n/2,-tran1)
      } 
    else{
      Xnext[iind]=rnorm(n/2,tran2,sd=.5)
      Xnext[!iind]=rnorm(n/2,-tran2,sd=.5)
      }

    anext=rbinom(n,1,pi1)
    rnext=2*Xnext+3*anext+theta+cos(3*j)+rnorm(n,sd=.5)
      
    X[[j]]=Xnext;r[[j]]=rnext;a[[j]]=anext}
  
  list(X,r,a)
}




