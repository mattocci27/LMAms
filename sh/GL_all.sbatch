#!/bin/bash
#SBATCH --mail-user=mattocci27@gmail.com
#SBATCH --mail-type=ALL
#SBATCH --cpus-per-task=3   # Number of cores: Can also use -c=4
#SBATCH --mem-per-cpu=4000mb
#SBATCH --time=40:00:00
date;hostname;pwd

module load gcc/5.2.0 R/3.2.2

R --vanilla --slave --args ${MODEL} 20000 10000 20 10 ${NULL_DATA} < ./model/GL_ra_all.r > ./log/GL_${MODEL}_${NULL_DATA}.log

date
