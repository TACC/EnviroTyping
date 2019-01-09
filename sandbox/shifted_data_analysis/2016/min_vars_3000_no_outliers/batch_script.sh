#!/bin/bash
#
#
#SBATCH -J min3_1
#SBATCH -N 1
#SBATCH -n 1
#SBATCH -p skx-normal
#SBATCH -t 15:00:00
#SBATCH -A Envriotype
#SBATCH -o job_%j_%N.out
#------------------------------------------------------
mkdir -p output
Rscript --verbose ./riskOutput.R > ./output.Rout