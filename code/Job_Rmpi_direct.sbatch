#!/bin/bash
#SBATCH --export=ALL,LD_PRELOAD=
#SBATCH --job-name Rmpi
#SBATCH --partition=qcpu --time=02:00
#SBATCH --nodes=10
#SBATCH --ntasks-per-node 36 ## maximum is 36
#SBATCH --output=logs/%x_%j.out
#SBATCH --account DD-23-116

## not needed module load OpenMPI/4.1.4-GCC-11.3.0
module load R/4.1.0-foss-2021a
srun  Rscript Rmpi_master_slave.R
