library(rhdf5)
walker = H5Fopen("/Users/zeyubian/Desktop/JASA_Sims/D4RL/D4RL_Data/walker2d_expert.hdf5")
term=walker$terminals
n=20;t=20;nn=20

ind=which(term==T)
ind=ind[1:(n*nn-1)]
ind=ind+1
ind=c(1,ind)

A=walker$actions
X=walker$observations
R=walker$rewards

a=x=r=list()
index=ind

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
    '/Users/zeyubian/Desktop/JASA_Sims/D4RL/D4RL_Data/walker/expert',j,'.RData',sep = ''))
  
}

### medium
walker = H5Fopen("/Users/zeyubian/Desktop/JASA_Sims/D4RL/D4RL_Data/walker2d_medium.hdf5")
term=walker$terminals
n=20;t=20;nn=20

ind=which(term==T)
ind=ind[1:(n*nn-1)]
ind=ind+1
ind=c(1,ind)

A=walker$actions
X=walker$observations
R=walker$rewards

a=x=r=list()
index=ind

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
    '/Users/zeyubian/Desktop/JASA_Sims/D4RL/D4RL_Data/walker/medium',j,'.RData',sep = ''))
  
}

### medium expert

walker = H5Fopen("/Users/zeyubian/Desktop/JASA_Sims/D4RL/D4RL_Data/walker2d_medium_expert.hdf5")
term=walker$terminals
n=20;t=20;nn=20

ind=which(term==T)
ind=ind[1:(n*nn-1)]
ind=ind+1
ind=c(1,ind)

A=walker$actions
X=walker$observations
R=walker$rewards

a=x=r=list()
index=ind

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
    '/Users/zeyubian/Desktop/JASA_Sims/D4RL/D4RL_Data/walker/medium_expert',j,'.RData',sep = ''))
  
}


### mixed

walker = H5Fopen("/Users/zeyubian/Desktop/JASA_Sims/D4RL/D4RL_Data/walker2d_mixed.hdf5")
term=walker$terminals
n=20;t=20;nn=20

ind=which(term==T)
ind=ind[1:(n*nn-1)]
ind=ind+1
ind=c(1,ind)

A=walker$actions
X=walker$observations
R=walker$rewards

a=x=r=list()
index=ind

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
    '/Users/zeyubian/Desktop/JASA_Sims/D4RL/D4RL_Data/walker/mixed',j,'.RData',sep = ''))
  
}

### random

walker = H5Fopen("/Users/zeyubian/Desktop/JASA_Sims/D4RL/D4RL_Data/walker2d_random.hdf5")
term=walker$terminals
n=20;t=20;nn=20

ind=which(term==T)
ind=ind[1:(n*nn-1)]
ind=ind+1
ind=c(1,ind)

A=walker$actions
X=walker$observations
R=walker$rewards

a=x=r=list()
index=ind

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
    '/Users/zeyubian/Desktop/JASA_Sims/D4RL/D4RL_Data/walker/random',j,'.RData',sep = ''))
  
}

### noisy

walker = H5Fopen("/Users/zeyubian/Desktop/JASA_Sims/D4RL/D4RL_Data/walker2d_random.hdf5")
term=walker$terminals
n=10;t=20;nn=20

ind=which(term==T)
ind=ind[1:(n*nn-1)]
ind=ind+1
ind=c(1,ind)

A=walker$actions
X=walker$observations
R=walker$rewards

a=x=r=list()
index=ind

for (j in 1:nn) {
  index=ind[((j-1)*n):((j)*n)]
  for (i in 1:n) {
    sub=index[i]
    a[[i]]=A[,sub:(sub+t-1)]
    x[[i]]=X[,sub:(sub+t-1)]
    r[[i]]=R[sub:(sub+t-1)]+sin(i)+cos(1:t)
  }
  
}

walker = H5Fopen("/Users/zeyubian/Desktop/JASA_Sims/D4RL/D4RL_Data/walker2d_expert.hdf5")
term=walker$terminals
n=10;t=20;nn=20

ind=which(term==T)
ind=ind[1:(n*nn-1)]
ind=ind+1
ind=c(1,ind)

A=walker$actions
X=walker$observations
R=walker$rewards


index=ind

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
    '/Users/zeyubian/Desktop/JASA_Sims/D4RL/D4RL_Data/walker/noisy',j,'.RData',sep = ''))
  
}


