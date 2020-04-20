#!/bin/bash
#
#
#SBATCH -J minday
#SBATCH -N 1
#SBATCH -n 1
#SBATCH --mail-user=azg5169@uncw.edu
#SBATCH --mail-type=all 
#SBATCH -p skx-normal
#SBATCH -t 48:00:00
#SBATCH -A Envriotype
#SBATCH -o job_%j_%N.out
#------------------------------------------------------
mkdir -p output
Rscript --verbose ./prem_workflow.R > ./output.Rout
