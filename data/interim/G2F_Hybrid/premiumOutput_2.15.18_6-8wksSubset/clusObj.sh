#!/bin/bash
#
#
#SBATCH -J seed_6_8
#SBATCH -N 1
#SBATCH -n 1
#SBATCH -p skx-normal
#SBATCH -t 03:00:00
#SBATCH -A Envriotype
#SBATCH -o slurm.%N.%j.out
#------------------------------------------------------

Rscript --vanilla --verbose ./clusObj.R > clusObj.Rout
