#!/bin/bash
#SBATCH -A hpc2n2025-051
#SBATCH --output=window.output
#SBATCH --error=%J.error
#SBATCH --mail-type=ALL
#SBATCH -t 30:00:00
#SBATCH -N 1
#SBATCH -c 28

ml GCC/11.2.0
ml OpenMPI/4.1.1
ml R/4.1.2

R -q --slave -f filter_data_test.R