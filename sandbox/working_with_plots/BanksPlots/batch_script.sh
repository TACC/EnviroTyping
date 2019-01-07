#!/bin/bash
#
#
#SBATCH -J risk_prof
#SBATCH -N 4
#SBATCH -n 8
#SBATCH -p skx-normal
#SBATCH -t 48:00:00
#SBATCH -A Envriotype
#SBATCH -o job_%j_%N.out
#------------------------------------------------------
mkdir -p output
Rscript --verbose ./2016YieldCalibWide.R > ./output.Rout
