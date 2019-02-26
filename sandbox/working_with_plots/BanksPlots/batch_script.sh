#!/bin/bash
#
#
<<<<<<< HEAD:sandbox/working_with_plots/BanksPlots/batch_script.sh
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
=======
#SBATCH -J min163k
#SBATCH -N 1
#SBATCH -n 1
#SBATCH --mail-user=azg5169@uncw.edu
#SBATCH --mail-type=all 
#SBATCH -p skx-normal
#SBATCH -t 15:00:00
#SBATCH -A Envriotype
#SBATCH -o job_%j_%N.out
#------------------------------------------------------
mkdir -p output1
Rscript --verbose ./prem_workflow.R > ./output1.Rout
>>>>>>> edd5433f108d82bd649c4fc25d03ac6f85c31afe:sandbox/shifted_data_analysis/2016/min_vars_3000/batch_script.sh
