# Load Rmpi
library(Rmpi)

n <- 30

# MPI information
size <- Rmpi::mpi.comm.size(0)
rank <- Rmpi::mpi.comm.rank(0)
host <- Rmpi::mpi.get.processor.name()

if (rank == 0) {
  
  cat("size", "rank", "host", "max_eigen_value", "\n")
  cat(size, rank, host, "NaN", "\n")
  
} else {
  
  A <- matrix(rnorm(n^2), nrow = n)
  A <- A + t(A)
  
  a <- max(eigen(A, symmetric = TRUE, only.values = TRUE)$values)
  
  cat(size, rank, host, a, "\n")
}

# Wait until all MPI processes have finished their work
Rmpi::mpi.barrier(0)

# Finish MPI cleanly
Rmpi::mpi.finalize()