#!/bin/bash
#SBATCH --job-name=GL_ra
#SBATCH --mail-user=mattocci27@gmail.com
#SBATCH --mail-type=ALL
#SBATCH --output=GL_ra.out
#SBATCH --error=GL_ra.err
#SBATCH --cpus-per-task=3   # Number of cores: Can also use -c=4
#SBATCH --mem-per-cpu=1000mb
#SBATCH --time=03:00:00
date;hostname;pwd

cd LES_code

module load gcc/5.2.0 R/3.2.2

Rscript GL_rand_across.r

date
