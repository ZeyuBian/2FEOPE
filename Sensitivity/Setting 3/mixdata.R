
## Use this to generate the data!!!

mix_data=function(n,t){
  
  X1=rnorm(n)
  
  theta=1*cbind(sin(1*(1:n)),sin(2*(1:n)),sin(3*(1:n)))
  lambda=1*cbind(cos(1*(1:t)),cos(2*(1:t)),cos(3*(1:t)))
  
  ## behavior policy
  pi1=.5
  a1=rbinom(n,1,pi1)
  
  ## reward function
  r1=2*X1+3*a1+apply(theta, 1, crossprod,y=lambda[1,])

  X=r=a=list();X[[1]]=X1; r[[1]]=r1;a[[1]]=a1
  
  ## from time 2 to time t: loop
  
  for (j in 2:t) {
    Xprev=X[[j-1]];aprev=a[[j-1]]

    ## transition probability
    tran=-.25*Xprev+aprev
    Xnext=rnorm(n,tran,sd=.5)+apply(theta, 1, crossprod,y=lambda[j,])
    
    anext=rbinom(n,1,pi1)
    rnext=2*Xnext+3*anext+c(theta[,1])+cos(j)+rnorm(n,sd=.5)
      
    X[[j]]=Xnext;r[[j]]=rnext;a[[j]]=anext}
  
  list(X,r,a)
}



