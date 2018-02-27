#!/bin/bash
#
# Simple SLURM script for submitting multiple serial
# jobs (e.g. parametric studies) using a script wrapper
# to launch the jobs.
#
# To use, build the launcher executable and your
# serial application(s) and place them in your WORKDIR
# directory.  Then, edit the CONTROL_FILE to specify
# each executable per process.
#-------------------------------------------------------
#-------------------------------------------------------
#
#         <------ Setup Parameters ------>
#
#SBATCH -J hyb_mon_1k
#SBATCH -N 9
#SBATCH -n 9
#SBATCH -p normal
#SBATCH -t 1:00:00
#SBATCH -A <allocation>
#------------------------------------------------------

export LAUNCHER_PLUGIN_DIR=$LAUNCHER_DIR/plugins
export LAUNCHER_RMI=SLURM
export LAUNCHER_JOB_FILE="$WORK/GitHub/EniroTyping/data/intermin/G2F_Hybrid/hyb_mon_1k/jobsFile"

$LAUNCHER_DIR/paramrun
