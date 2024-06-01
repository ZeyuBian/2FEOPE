library(rhdf5)
halfcheetah = H5Fopen("/Users/zeyubian/Desktop/JASA_Sims/D4RL/D4RL_Data/halfcheetah_medium_expert.hdf5")
term=halfcheetah$terminals
n=20;t=20;nn=20
gap=100;ind=which(term==T)
ind=seq(from=1,to=(n*nn-1)*gap+1,by=gap)

A=halfcheetah$actions
X=halfcheetah$observations
R=halfcheetah$rewards

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
    '/Users/zeyubian/Desktop/JASA_Sims/D4RL/D4RL_Data/halfcheetah/medium_expert',j,'.RData',sep = ''))
  
}

### medium

library(rhdf5)
halfcheetah = H5Fopen("/Users/zeyubian/Desktop/JASA_Sims/D4RL/D4RL_Data/halfcheetah_medium.hdf5")
term=halfcheetah$terminals
n=20;t=20;nn=20
gap=100;ind=which(term==T)
ind=seq(from=1,to=(n*nn-1)*gap+1,by=gap)

A=halfcheetah$actions
X=halfcheetah$observations
R=halfcheetah$rewards

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
    '/Users/zeyubian/Desktop/JASA_Sims/D4RL/D4RL_Data/halfcheetah/medium',j,'.RData',sep = ''))
  
}

# expert

halfcheetah = H5Fopen("/Users/zeyubian/Desktop/JASA_Sims/D4RL/D4RL_Data/halfcheetah_expert.hdf5")
term=halfcheetah$terminals
n=20;t=20;nn=20
gap=100;ind=which(term==T)
ind=seq(from=1,to=(n*nn-1)*gap+1,by=gap)

A=halfcheetah$actions
X=halfcheetah$observations
R=halfcheetah$rewards

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
    '/Users/zeyubian/Desktop/JASA_Sims/D4RL/D4RL_Data/halfcheetah/expert',j,'.RData',sep = ''))
  
}


## mixed

library(rhdf5)
halfcheetah = H5Fopen("/Users/zeyubian/Desktop/JASA_Sims/D4RL/D4RL_Data/halfcheetah_mixed.hdf5")
term=halfcheetah$terminals
n=20;t=20;nn=20
gap=100;ind=which(term==T)
ind=seq(from=1,to=(n*nn-1)*gap+1,by=gap)

A=halfcheetah$actions
X=halfcheetah$observations
R=halfcheetah$rewards

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
    '/Users/zeyubian/Desktop/JASA_Sims/D4RL/D4RL_Data/halfcheetah/mixed',j,'.RData',sep = ''))
  
}


### random

library(rhdf5)
halfcheetah = H5Fopen("/Users/zeyubian/Desktop/JASA_Sims/D4RL/D4RL_Data/halfcheetah_random.hdf5")
term=halfcheetah$terminals
n=20;t=20;nn=20
gap=100;ind=which(term==T)
ind=seq(from=1,to=(n*nn-1)*gap+1,by=gap)

A=halfcheetah$actions
X=halfcheetah$observations
R=halfcheetah$rewards

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
    '/Users/zeyubian/Desktop/JASA_Sims/D4RL/D4RL_Data/halfcheetah/random',j,'.RData',sep = ''))
  
}


### noisy
library(rhdf5)
halfcheetah = H5Fopen("/Users/zeyubian/Desktop/JASA_Sims/D4RL/D4RL_Data/halfcheetah_random.hdf5")
term=halfcheetah$terminals
n=10;t=20;nn=20
gap=100;ind=which(term==T)
ind=seq(from=1,to=(n*nn-1)*gap+1,by=gap)

A=halfcheetah$actions
X=halfcheetah$observations
R=halfcheetah$rewards

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

halfcheetah = H5Fopen("/Users/zeyubian/Desktop/JASA_Sims/D4RL/D4RL_Data/halfcheetah_expert.hdf5")
term=halfcheetah$terminals
n=10;t=20;nn=20
gap=100;ind=which(term==T)
ind=seq(from=1,to=(n*nn-1)*gap+1,by=gap)

A=halfcheetah$actions
X=halfcheetah$observations
R=halfcheetah$rewards


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
    '/Users/zeyubian/Desktop/JASA_Sims/D4RL/D4RL_Data/halfcheetah/noisy',j,'.RData',sep = ''))
  
}






