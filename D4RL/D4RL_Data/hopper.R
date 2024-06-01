library(rhdf5)
hopper = H5Fopen("/Users/zeyubian/Desktop/JASA_Sims/D4RL/D4RL_Data/hopper_medium.hdf5")
term=hopper$terminals
n=20;t=20;nn=20

ind=which(term==T)
ind=ind[1:(n*nn-1)]
ind=ind+1
ind=c(1,ind)

A=hopper$actions
X=hopper$observations
R=hopper$rewards

a=x=r=list()


for (j in 1:nn) {
  index=ind[((j-1)*n):((j)*n)]
  for (i in 1:n) {
    sub=index[i]
    a[[i]]=A[,sub:(sub+t-1)]
    x[[i]]=X[,sub:(sub+t-1)]
    r[[i]]=R[sub:(sub+t-1)]+sin(i)+cos(1:t)
  }
  
  data=list(x,a,r)
  
  save(data,file=paste(
         '/Users/zeyubian/Desktop/JASA_Sims/D4RL/D4RL_Data/hopper/medium',j,'.RData',sep = ''))
  
}


### expert
library(rhdf5)
hopper = H5Fopen("/Users/zeyubian/Desktop/JASA_Sims/D4RL/D4RL_Data/hopper_expert.hdf5")
term=hopper$terminals

gap=100;ind=which(term==T)
ind=seq(from=1,to=(n*nn-1)*gap+1,by=gap)

A=hopper$actions
X=hopper$observations
R=hopper$rewards

a=x=r=list()


for (j in 1:nn) {
  index=ind[((j-1)*n):((j)*n)]
  for (i in 1:n) {
    sub=index[i]
    a[[i]]=A[,sub:(sub+t-1)]
    x[[i]]=X[,sub:(sub+t-1)]
    r[[i]]=R[sub:(sub+t-1)]+sin(i)+cos(1:t)
  }
  
  data=list(x,a,r)
  
  save(data,file=paste(
    '/Users/zeyubian/Desktop/JASA_Sims/D4RL/D4RL_Data/hopper/expert',j,'.RData',sep = ''))
  
}




## medium-replay

library(rhdf5)
hopper = H5Fopen("/Users/zeyubian/Desktop/JASA_Sims/D4RL/D4RL_Data/hopper_mixed.hdf5")
term=hopper$terminals

ind=which(term==T)
ind=ind[1:(n*nn-1)]
ind=ind+1
ind=c(1,ind)

A=hopper$actions
X=hopper$observations
R=hopper$rewards

a=x=r=list()


for (j in 1:nn) {
  index=ind[((j-1)*n):((j)*n)]
  for (i in 1:n) {
    sub=index[i]
    a[[i]]=A[,sub:(sub+t-1)]
    x[[i]]=X[,sub:(sub+t-1)]
    r[[i]]=R[sub:(sub+t-1)]+sin(i)+cos(1:t)
  }
  
  data=list(x,a,r)
  
  save(data,file=paste(
    '/Users/zeyubian/Desktop/JASA_Sims/D4RL/D4RL_Data/hopper/mixed',j,'.RData',sep = ''))
  
}


## medium-expert

library(rhdf5)
hopper = H5Fopen("/Users/zeyubian/Desktop/JASA_Sims/D4RL/D4RL_Data/hopper_medium_expert.hdf5")
term=hopper$terminals

ind=which(term==T)
ind=ind[1:(n*nn-1)]
ind=ind+1
ind=c(1,ind)

A=hopper$actions
X=hopper$observations
R=hopper$rewards

a=x=r=list()


for (j in 1:nn) {
  index=ind[((j-1)*n):((j)*n)]
  for (i in 1:n) {
    sub=index[i]
    a[[i]]=A[,sub:(sub+t-1)]
    x[[i]]=X[,sub:(sub+t-1)]
    r[[i]]=R[sub:(sub+t-1)]+sin(i)+cos(1:t)
  }
  
  data=list(x,a,r)
  
  save(data,file=paste(
    '/Users/zeyubian/Desktop/JASA_Sims/D4RL/D4RL_Data/hopper/medium_expert',j,'.RData',sep = ''))
  
}


### noisy
n=10;t=20;nn=20
library(rhdf5)
hopper = H5Fopen("/Users/zeyubian/Desktop/JASA_Sims/D4RL/D4RL_Data/hopper_random.hdf5")
term=hopper$terminals

ind=which(term==T)
ind=ind[1:(n*nn-1)]
ind=ind+1
ind=c(1,ind)

A=hopper$actions
X=hopper$observations
R=hopper$rewards

a=x=r=list()


for (j in 1:nn) {
  index=ind[((j-1)*n):((j)*n)]
  for (i in 1:n) {
    sub=index[i]
    a[[i]]=A[,sub:(sub+t-1)]
    x[[i]]=X[,sub:(sub+t-1)]
    r[[i]]=R[sub:(sub+t-1)]+sin(i)+cos(1:t)
  }
  
}

hopper = H5Fopen("/Users/zeyubian/Desktop/JASA_Sims/D4RL/D4RL_Data/hopper_expert.hdf5")
term=hopper$terminals

gap=100;ind=which(term==T)
ind=seq(from=1,to=(n*nn-1)*gap+1,by=gap)

A=hopper$actions
X=hopper$observations
R=hopper$rewards


for (j in 1:nn) {
  index=ind[((j-1)*n):((j)*n)]
  for (i in 1:n) {
    sub=index[i]
    a[[i+n]]=A[,sub:(sub+t-1)]
    x[[i+n]]=X[,sub:(sub+t-1)]
    r[[i+n]]=R[sub:(sub+t-1)]+sin(i+n)+cos(1:t)
  }
  
  data=list(x,a,r)
  
  save(data,file=paste(
    '/Users/zeyubian/Desktop/JASA_Sims/D4RL/D4RL_Data/hopper/noisy',j,'.RData',sep = ''))
  
}





