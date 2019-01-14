#!/bin/bash
#
#
#SBATCH -J mean_3K_16
#SBATCH -N 2
#SBATCH -n 4
#SBATCH --mail-user=bno5761@uncw.edu
#SBATCH --mail-type=all    # Send email at begin and end of job
#SBATCH -p long
#SBATCH -t 72:00:00
#SBATCH -A Envriotype
#SBATCH -o job_%j_%N.out
#------------------------------------------------------
mkdir output
Rscript --verbose ./prem_workflow.R > ./output.Rout
