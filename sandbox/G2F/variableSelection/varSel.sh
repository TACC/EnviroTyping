#!/bin/bash
#
#
#SBATCH -J varSel
#SBATCH -N 10
#SBATCH -n 10
#SBATCH -p skx-normal
#SBATCH -t 48:00:00
#SBATCH -A Envriotype
#SBATCH -o slurm.%N.%j.out
#------------------------------------------------------
mkdir output
remora Rscript --vanilla --verbose ./variableSelection.R > ./output.Rout
