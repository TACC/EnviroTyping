#!/bin/bash
#
#
#SBATCH -J 2016
#SBATCH -N 1
#SBATCH -n 1
#SBATCH -p skx-normal
#SBATCH -t 48:00:00
#SBATCH -A Envriotype
#SBATCH -o job_%j_%N.out
#------------------------------------------------------
mkdir output
remora Rscript --vanilla --verbose ./2016_wide.R > ./2016_wide_output.Rout
