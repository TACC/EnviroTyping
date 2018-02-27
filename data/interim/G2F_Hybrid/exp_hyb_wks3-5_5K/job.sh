#!/bin/bash
#
#
#SBATCH -J exp_hyb_wks3-5_5K
#SBATCH -N 1
#SBATCH -n 1
#SBATCH -p skx-normal
#SBATCH -t 48:00:00
#SBATCH -A Envriotype
#SBATCH -o slurm.%N.%j.out
#------------------------------------------------------
mkdir output
auth-tokens-refresh
files-get -N data.csv dhbrand/Premium/hybrid_by_weeksSincePlanted_cleaned_weather.csv
remora Rscript --vanilla --verbose ./job.R > ./output.Rout
