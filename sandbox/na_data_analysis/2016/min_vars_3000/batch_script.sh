#!/bin/bash
#
#
#SBATCH -J 16na
#SBATCH -N 1
#SBATCH -n 1
#SBATCH --mail-user=azg5169@uncw.edu
#SBATCH --mail-type=all 
#SBATCH -p skx-normal
#SBATCH -t 24:00:00
#SBATCH -A Envriotype
#SBATCH -o job_%j_%N.out
#------------------------------------------------------
mkdir -p output
Rscript --verbose ./prem_workflow.R > ./output.Rout
