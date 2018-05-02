#!/bin/bash
#
#
#SBATCH -J var_sel
#SBATCH -N 1
#SBATCH -n 1
#SBATCH -p skx-normal
#SBATCH -t 24:00:00
#SBATCH -A Envriotype
#SBATCH -o slurm.%N.%j.out
#------------------------------------------------------
mkdir output
Rscript --vanilla --verbose ./var_sel.R > ./output_var_sel.Rout