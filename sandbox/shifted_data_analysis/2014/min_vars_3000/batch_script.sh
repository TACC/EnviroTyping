#!/bin/bash
#
#
#SBATCH -J min_3K_14
#SBATCH -N 1
#SBATCH -n 1
#SBATCH -p skx-normal
#SBATCH -t 24:00:00
#SBATCH -A Envriotype
#SBATCH -o job_%j_%N.out
#------------------------------------------------------
mkdir output
Rscript --verbose ./prem_workflow.R > ./output.Rout