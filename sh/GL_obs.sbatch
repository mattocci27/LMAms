#!/bin/bash
#SBATCH --job-name=GL_LMA
#SBATCH --mail-user=mattocci27@gmail.com
#SBATCH --mail-type=ALL
#SBATCH --output=GL_obs.out
#SBATCH --error=GL_obs.err
#SBATCH --cpus-per-task=3   # Number of cores: Can also use -c=4
#SBATCH --mem-per-cpu=2000mb
#SBATCH --time=03:00:00
date;hostname;pwd

cd LES_code

module load gcc/5.2.0 R/3.2.2

Rscript GL_LMA.r

date
