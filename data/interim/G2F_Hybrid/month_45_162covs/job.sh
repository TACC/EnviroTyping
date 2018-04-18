#!/bin/bash
#
#
#SBATCH -J cov162
#SBATCH -N 1
#SBATCH -n 2
#SBATCH -p skx-normal
#SBATCH -t 48:00:00
#SBATCH -A Envriotype
#SBATCH -o job_%j_%N.out
#------------------------------------------------------

Rscript --vanilla --verbose ./iters200/month_45_162covs.R > ./iters200/output.Rout &
Rscript --vanilla --verbose ./iters400/month_45_162covs.R > ./iters400/output.Rout 

