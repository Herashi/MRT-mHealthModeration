#!/bin/bash
#SBATCH --job-name=hello_world
#SBATCH --time=2:00:00
#SBATCH --mail-type=NONE
#SBATCH --mem=25g
#SBATCH --cpus-per-task=1
#SBATCH --array=1-1200
#SBATCH --output=/home/herashi/MRT/slurm_output/slurm-%j.out

R CMD BATCH --no-save --no-restore part1.R
