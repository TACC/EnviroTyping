#!/bin/bash
#
#
#SBATCH -J 16_shift
#SBATCH -N 1
#SBATCH -n 1
#SBATCH -p skx-normal
#SBATCH -t 48:00:00
#SBATCH -A Envriotype
#SBATCH -o job_%j_%N.out
#------------------------------------------------------
mkdir output_shift
remora Rscript --vanilla --verbose ./2016_wide_shifted.R > ./2016_wide_output_shifted.Rout
