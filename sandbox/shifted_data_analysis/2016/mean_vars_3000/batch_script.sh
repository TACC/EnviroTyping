#!/bin/bash
#
#
#SBATCH -J mean_3K_16
#SBATCH -N 1
#SBATCH -n 1
#SBATCH --mail-user=azg5169@uncw.edu
#SBATCH --mail-type=all    # Send email at begin and end of job
#SBATCH -p skx-normal
#SBATCH -t 48:00:00
#SBATCH -A Envriotype
#SBATCH -o job_%j_%N.out
#------------------------------------------------------
mkdir output
Rscript --verbose ./prem_workflow.R > ./output.Rout
