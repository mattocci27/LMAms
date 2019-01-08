#!/bin/bash
#SBATCH --mail-user=mattocci27@gmail.com
#SBATCH --mail-type=ALL
#SBATCH --cpus-per-task=4   # Number of cores: Can also use -c=4
#SBATCH --mem-per-cpu=4000mb
#SBATCH --time=04:00:00
date;hostname;pwd

module load R/3.4.3

# sbatch
R --vanilla --slave --args ${MODEL} ${DATA} 10000 9000 1 ${OBS} < ./model/model_more.r > ./log/${DATA}_${MODEL}_${OBS}.log
 
date
