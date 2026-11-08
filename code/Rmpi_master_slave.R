#load sources

library(Rmpi)
n=30
#

size <- Rmpi::mpi.comm.size(0)
rank <- Rmpi::mpi.comm.rank(0)
host <- Rmpi::mpi.get.processor.name()
if (rank == 0){
  cat("size ","rank ","host ","max_eigen_value\n")
	cat(size,rank,host,"NaN\n")
} else {
  where=getwd()
  A=matrix(rnorm(n^2),nrow=n)
  A=A+t(A)
  a = max(eigen(A)$values)
  cat(size,rank,host,a,"\n")    
}
