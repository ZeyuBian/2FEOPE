library(rhdf5)
maze = H5Fopen("/Users/zeyubian/Desktop/JASA_Sims/D4RL/D4RL_Data/maze2d-large-sparse-v1.hdf5")
term=maze$timeouts
n=20;t=20;nn=20
ind=which(term==T)

ind=ind[1:(n*nn-1)]
ind=ind+1
ind=c(1,ind)

A=maze$actions
X=maze$observations
R=maze$rewards

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
    '/Users/zeyubian/Desktop/JASA_Sims/D4RL/D4RL_Data/maze/large',j,'.RData',sep = ''))
  
}

## medium
maze = H5Fopen("/Users/zeyubian/Desktop/JASA_Sims/D4RL/D4RL_Data/maze2d-medium-sparse-v1.hdf5")
term=maze$timeouts
n=20;t=20;nn=20
ind=which(term==T)

ind=ind[1:(n*nn-1)]
ind=ind+1
ind=c(1,ind)

A=maze$actions
X=maze$observations
R=maze$rewards

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
    '/Users/zeyubian/Desktop/JASA_Sims/D4RL/D4RL_Data/maze/medium',j,'.RData',sep = ''))
  
}

## open
maze = H5Fopen("/Users/zeyubian/Desktop/JASA_Sims/D4RL/D4RL_Data/maze2d-open-sparse.hdf5")
term=maze$timeouts
n=20;t=20;nn=20
ind=which(term==T)

ind=ind[1:(n*nn-1)]
ind=ind+1
ind=c(1,ind)

A=maze$actions
X=maze$observations
R=maze$rewards

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
    '/Users/zeyubian/Desktop/JASA_Sims/D4RL/D4RL_Data/maze/open',j,'.RData',sep = ''))
  
}


## umaze
maze = H5Fopen("/Users/zeyubian/Desktop/JASA_Sims/D4RL/D4RL_Data/maze2d-umaze-sparse-v1.hdf5")
term=maze$timeouts
n=20;t=20;nn=20
ind=which(term==T)

ind=ind[1:(n*nn-1)]
ind=ind+1
ind=c(1,ind)

A=maze$actions
X=maze$observations
R=maze$rewards

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
    '/Users/zeyubian/Desktop/JASA_Sims/D4RL/D4RL_Data/maze/umaze',j,'.RData',sep = ''))
  
}