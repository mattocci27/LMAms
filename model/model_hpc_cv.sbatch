#!/bin/bash
#SBATCH --mail-user=mattocci27@gmail.com
#SBATCH --mail-type=ALL
#SBATCH --cpus-per-task=4   # Number of cores: Can also use -c=4
#SBATCH --mem-per-cpu=4000mb
#SBATCH --time=24:00:00
date;hostname;pwd

module load R/3.6

# sbatch
R --vanilla --slave --args ${MODEL} ${DATA} 4000 3000 1 ${OBS} < ./model/k_fold.r > ./log/${MODEL}_${OBS}.log
 
date
