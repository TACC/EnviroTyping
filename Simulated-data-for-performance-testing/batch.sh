#!/bin/bash
#
#
#SBATCH -J sim
#SBATCH -N 1
#SBATCH -n 1
#SBATCH -p skx-normal
#SBATCH -t 04:00:00
#SBATCH -A Envriotype
#SBATCH -o job_%j_%N.out
#------------------------------------------------------
mkdir -p output
remora Rscript --vanilla --verbose ./full.R > ./output.Rout
