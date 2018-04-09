#!/bin/bash
#
#
#SBATCH -J complete45
#SBATCH -N 1
#SBATCH -n 1
#SBATCH -p skx-normal
#SBATCH -t 48:00:00
#SBATCH -A Envriotype
#SBATCH -o slurm.%N.%j.out
#------------------------------------------------------
remora Rscript --vanilla --verbose ./complete45.R > ./output.Rout
