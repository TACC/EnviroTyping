#!/bin/bash
#
#
#SBATCH -J cov162
#SBATCH -N 1
#SBATCH -n 1
#SBATCH -p skx-normal
#SBATCH -t 48:00:00
#SBATCH -A Envriotype
#SBATCH -o job_%j_%N.out
#------------------------------------------------------
mkdir output
Rscript --vanilla --verbose ./month_45_162covs.R > ./output.Rout
