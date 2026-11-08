rm(list=ls())  # R code: parallel version 
library(snow) 
library(Rmpi)  
nclus=6  
cl <-snow::makeMPIcluster(nclus)  #alter either n or mc to affect run time 
n=30 
N_per_proc=100  
#x=matrix(runif(n),n,1) 
#x=cbind(1,x)  
min_eig_values=function(n,N){
    a=c()
    for (ind in 1:N){
    A=matrix(rnorm(n^2),nrow=n)
    A=A+t(A)
    a[ind] = max(eigen(A)$values)
    }
    return(a)
}  
ptim=proc.time()[3]  
b=clusterCall(cl,min_eig_values,n=n,N=N_per_proc)  
b=unlist(b)
hist(b)
tim=proc.time()[3]-ptim  
#Rmpi::mpi.quit()
snow::stopCluster(cl) 
