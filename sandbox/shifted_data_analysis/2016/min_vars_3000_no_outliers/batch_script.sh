#!/bin/bash
#
#
#SBATCH -J 2016min3
#SBATCH -N 2
#SBATCH -n 4
#SBATCH -p skx-normal
#SBATCH -t 15:00:00
#SBATCH -A Envriotype
#SBATCH -o job_%j_%N.out
#------------------------------------------------------
mkdir -p output
Rscript --verbose ./riskOutput.R > ./output.Rout