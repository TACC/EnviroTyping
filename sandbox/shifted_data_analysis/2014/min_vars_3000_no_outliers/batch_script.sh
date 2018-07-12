#!/bin/bash
#
#
#SBATCH -J min3K14no
#SBATCH -N 1
#SBATCH -n 1
#SBATCH -p skx-normal
#SBATCH -t 48:00:00
#SBATCH -A Envriotype
#SBATCH -o job_%j_%N.out
#------------------------------------------------------
mkdir -p output
Rscript --verbose ./finding_number_of_clusters.R > ./output.Rout
