#!/bin/bash
#
#
#SBATCH -J strata45
#SBATCH -N 1
#SBATCH -n 1
#SBATCH -p skx-normal
#SBATCH -t 24:00:00
#SBATCH -A Envriotype
#SBATCH -o slurm.%N.%j.out
#------------------------------------------------------
mkdir complete45_long
Rscript --vanilla --verbose ./statifiedCV_45.R > ./output_stratifedCV_45.Rout
