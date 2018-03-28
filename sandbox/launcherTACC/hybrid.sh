#!/bin/bash

#SBATCH -p skx-normal
#SBATCH -t 12:00:00
#SBATCH -N 1
#SBATCH -n 1
#SBATCH -A iPlant-Collabs
#SBATCH -J hybr-norm
#SBATCH --mail-user=dh4545@uncw.edu
#SBATCH --mail-type=all    # Send email at begin and end of job

R CMD BATCH hybrid_g2f_month/june.R $WORK/Premium/hybrid_g2f_month/june_output.txt
