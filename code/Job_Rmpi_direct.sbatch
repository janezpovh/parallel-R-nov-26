#!/bin/bash
#SBATCH --export=ALL,LD_PRELOAD=
#SBATCH --job-name Rmpi
#SBATCH --partition=qcpu
#SBATCH --time=02:00
#SBATCH --nodes=2
#SBATCH --ntasks-per-node 128 ## maximum is 128
#SBATCH --output=logs/%x_%j.out
#SBATCH --account atr-26-4

module --force purge
module load R/4.4.2-gfbf-2024a
module load OpenMPI/5.0.3-GCC-13.3.0

export R_LIBS_USER="$HOME/R/library/4.4"
mkdir -p "$R_LIBS_USER"

Rscript -e '
.libPaths(c(Sys.getenv("R_LIBS_USER"), .libPaths()))

if (!requireNamespace("Rmpi", quietly = TRUE)) {
    cat("Rmpi not installed - installing...\n")
    install.packages(
        "Rmpi",
        lib = Sys.getenv("R_LIBS_USER"),
        repos = "https://cloud.r-project.org"
    )
}

library(Rmpi)
cat("Rmpi OK\n")
'

mpirun Rscript Rmpi_master_slave.R
