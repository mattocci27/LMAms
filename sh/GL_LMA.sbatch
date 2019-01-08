#!/bin/bash
#SBATCH --job-name=GL_LMA
#SBATCH --mail-user=mattocci27@gmail.com
#SBATCH --mail-type=ALL
#SBATCH --output=./log/GL_LMA.out
#SBATCH --error=./log/GL_LMA.err
#SBATCH --cpus-per-task=3   # Number of cores: Can also use -c=4
#SBATCH --mem-per-cpu=2000mb
#SBATCH --time=03:00:00
date;hostname;pwd

module load gcc/5.2.0 R/3.2.2

nohup Rscript ./model/GL_obs_LMA.r > ./log/GL_LMA.log &

date
