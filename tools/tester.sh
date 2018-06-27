#!/bin/bash
#
#
#SBATCH -A Envriotype
#SBATCH -N 1
#SBATCH -n 1
#SBATCH -p skx-normal
#SBATCH -o job_%j_%N.out
#SBATCH -t 48:00:00
#SBATCH -J tester

Rscript --verbose ./Benchmark_Function.R > ./output_tester_%j.Rout
