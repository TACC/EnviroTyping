#!/bin/bash
#
#
#SBATCH -J seed_3_5
#SBATCH -N 1
#SBATCH -n 1
#SBATCH -p normal
#SBATCH -t 48:00:00
#SBATCH -A Envriotype
#SBATCH -o slurm.%N.%j.out
#------------------------------------------------------

Rscript --vanilla --verbose ./seed_3_5_TRUE.R > output_seed_3_5/output.Rout 2> output_seed_3_5/error.Rerr
