#!/bin/bash
#SBATCH --job-name=combine
#SBATCH --time=1:00:00
#SBATCH --mail-type=begin,end,fail
#SBATCH --account=wdem2
#SBATCH --partition=standard
#SBATCH --mem=5g
#SBATCH --cpus-per-task=1
#SBATCH --output=/home/herashi/MRT/slurm_output/slurm-%j.out

module load R
R CMD BATCH --no-save --no-restore /home/herashi/MRT/indirect_effect/100_25/part2.R
