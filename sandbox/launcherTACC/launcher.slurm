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
#SBATCH -J hyb_month
#SBATCH -N 7
#SBATCH -n 7
#SBATCH -p normal
#SBATCH -t 48:00:00
#SBATCH -A Envriotype
#------------------------------------------------------

export LAUNCHER_PLUGIN_DIR=$LAUNCHER_DIR/plugins
export LAUNCHER_RMI=SLURM
export LAUNCHER_JOB_FILE="$WORK/EnviroTyping/scripts/G2F/hybridYieldHPC/launcherTest2"

$LAUNCHER_DIR/paramrun
