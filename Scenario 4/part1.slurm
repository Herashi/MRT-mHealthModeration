#!/bin/bash
#SBATCH --job-name=hello_world
#SBATCH --time=1-0
#SBATCH --mail-type=begin,end,fail
#SBATCH --account=wdem2
#SBATCH --partition=standard
#SBATCH --mem=80g
#SBATCH --cpus-per-task=1
#SBATCH --array=1-1000
#SBATCH --output=/home/herashi/MRT/slurm_output/slurm-%j.out

module load R
R CMD BATCH --no-save --no-restore /home/herashi/MRT/indirect_effect/100_25/part1.R