#!/bin/bash
#SBATCH --job-name=PA_model3_new
#SBATCH --mail-user=mattocci27@gmail.com
#SBATCH --mail-type=ALL
#SBATCH --output=PA_model3_new.out
#SBATCH --error=PA_model3_new.err
#SBATCH --cpus-per-task=3   # Number of cores: Can also use -c=4
#SBATCH --mem-per-cpu=1000mb
#SBATCH --time=03:00:00

date;hostname;pwd

cd LES_code

module load gcc/5.2.0 R/3.2.2

R --vanilla --slave --args model3 20000 10000 20 < PA_rand_all.R

date
