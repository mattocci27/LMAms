#!/bin/bash
#SBATCH --mail-user=mattocci27@gmail.com
#SBATCH --mail-type=ALL
#SBATCH --cpus-per-task=3   # Number of cores: Can also use -c=4
#SBATCH --mem-per-cpu=4000mb
#SBATCH --time=04:00:00
date;hostname;pwd

module load gcc/5.2.0 R/3.2.2

# sbatch
R --vanilla --slave --args ${MODEL} ${DATA} 3000 2000 1 ${OBS} < ./sim/sim_model_s.r > ./log/${DATA}_${MODEL}_${OBS}_sim.log
 
date
