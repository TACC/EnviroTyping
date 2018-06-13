#!/bin/bash
#
#
#SBATCH -J seeds
#SBATCH -N 1
#SBATCH -n 1
#SBATCH -p skx-normal
#SBATCH -t 24:00:00
#SBATCH -A Envriotype
#SBATCH -o job_%j_%N.out
#------------------------------------------------------
mkdir -p output
Rscript --vanilla --verbose ./testing_seeds.R > ./output_seeds.Rout
