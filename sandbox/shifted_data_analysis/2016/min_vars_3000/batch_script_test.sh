#!/bin/bash
#
#
#SBATCH -J test
#SBATCH -N 1
#SBATCH -n 1
#SBATCH -p skx-normal
#SBATCH -t 10:00:00
#SBATCH -A Envriotype
#SBATCH -o job_%j_%N.out
#------------------------------------------------------
Rscript --verbose ./riskproftest.R > ./output.Rout
