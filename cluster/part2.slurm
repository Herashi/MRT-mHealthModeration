#!/bin/bash
#SBATCH --job-name=combine
#SBATCH --time=1:00:00
#SBATCH --mail-type=NONE
#SBATCH --mem=10g
#SBATCH --cpus-per-task=1
#SBATCH --output=/home/herashi/MRT/slurm_output/slurm-%j.out

R CMD BATCH --no-save --no-restore part2.R
