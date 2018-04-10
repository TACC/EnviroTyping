#!/bin/bash
#
#
#SBATCH -J wide45
#SBATCH -N 1
#SBATCH -n 1
#SBATCH -p skx-normal
#SBATCH -t 24:00:00
#SBATCH -A Envriotype
#SBATCH -o slurm.%N.%j.out
#------------------------------------------------------
mkdir complete45_wide
Rscript --vanilla --verbose ./complete45_long.R > ./output_long.Rout
