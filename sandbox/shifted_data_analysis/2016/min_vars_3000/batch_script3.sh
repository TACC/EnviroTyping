#!/bin/bash
#
#
#SBATCH -J min163k
#SBATCH -N 1
#SBATCH -n 1
#SBATCH -p skx-normal
#SBATCH -t 15:00:00
#SBATCH -A Envriotype
#SBATCH -o job_%j_%N.out
#------------------------------------------------------
mkdir -p output3
Rscript --verbose ./prem_workflow3.R > ./output.Rout