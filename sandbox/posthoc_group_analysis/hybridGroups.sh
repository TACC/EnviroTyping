#!/bin/bash
#
#
#SBATCH -J prune
#SBATCH -N 1
#SBATCH -n 1
#SBATCH -p skx-normal
#SBATCH -t 48:00:00
#SBATCH -A Envriotype
#SBATCH -o slurm.%N.%j.out
#------------------------------------------------------
mkdir output
remora Rscript --vanilla --verbose ./profileByGroup.R > ./output.Rout
