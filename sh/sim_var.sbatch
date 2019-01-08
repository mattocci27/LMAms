#!/bin/bash
#SBATCH --job-name=sim_var
#SBATCH --mail-user=mattocci27@gmail.com
#SBATCH --mail-type=ALL
#SBATCH --output=sim_var.out
#SBATCH --error=sim_var.err
#SBATCH --cpus-per-task=5   # Number of cores: Can also use -c=4
#SBATCH --mem-per-cpu=2000mb
#SBATCH --time=03:00:00
date;hostname;pwd

cd LES_code

module load gcc/5.2.0 R/3.2.2

Rscript cov_new.r

date
