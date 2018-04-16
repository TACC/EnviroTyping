#!/bin/bash
#
#
#SBATCH -A Envriotype
#SBATCH -N 1
#SBATCH -n 1
#SBATCH -P skx-normal
#SBATCH -o job_%N_%j.out
#SBATCH -t 48:00:00
#SBATCH -J tester

Rscript --vanilla --verbose ./Benchmark_Function.R > ./output_tester_%j.Rout