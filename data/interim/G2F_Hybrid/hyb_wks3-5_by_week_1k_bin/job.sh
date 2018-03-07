#!/bin/bash
#
#
#SBATCH -J bin
#SBATCH -N 1
#SBATCH -n 1
#SBATCH -p skx-normal
#SBATCH -t 24:00:00
#SBATCH -A Envriotype
#SBATCH -o slurm.%N.%j.out
#------------------------------------------------------
mkdir output
remora Rscript --vanilla --verbose ./job.R > ./output.Rout
