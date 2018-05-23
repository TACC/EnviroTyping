#!/bin/bash
#
#
#SBATCH -J iter_1000
#SBATCH -N 1
#SBATCH -n 1
#SBATCH -p skx-normal
#SBATCH -t 48:00:00
#SBATCH -A Envriotype
#SBATCH -o job_%j_%N.out
#------------------------------------------------------
mkdir output
remora Rscript --vanilla --verbose ./iter_1000.R > ./iter_1000.Rout
