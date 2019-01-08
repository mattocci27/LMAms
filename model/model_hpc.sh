#!/bin/bash
DATA=GL
OBS=obs

for MODEL in GL_LMAms
do
  #
  echo "${MODEL}, ${DATA}, ${OBS}"
  export MODEL DATA OBS
  #
  sbatch --job-name=${DATA}_${MODEL}_${OBS}\
  --output=./log/${DATA}_${MODEL}_${OBS}.out\
  --error=./log/${DATA}_${MODEL}_${OBS}.err\
  --job-name=${DATA}_${MODEL}_${OBS}\
  ./model/model_hpc.sbatch
  sleep 1 # pause to be kind to the scheduler
done

for MODEL in GL_LMAms_CV GL_LMA_CV
do
  #
  echo "${MODEL}, ${DATA}, ${OBS}"
  export MODEL DATA OBS
  #
  sbatch --job-name=${DATA}_${MODEL}_${OBS}\
  --output=./log/${DATA}_${MODEL}_${OBS}.out\
  --error=./log/${DATA}_${MODEL}_${OBS}.err\
  --job-name=${DATA}_${MODEL}_${OBS}\
  ./model/model_hpc_cv.sbatch
  sleep 1 # pause to be kind to the scheduler
done

DATA=PA
OBS=obs
for MODEL in PA_LMAms_L PA_LMAms PA_LMAms_L0
do
  #
  echo "${MODEL}, ${DATA}, ${OBS}"
  export MODEL DATA OBS
  
  sbatch --job-name=${DATA}_${MODEL}_${OBS}\
  --output=./log/${DATA}_${MODEL}_${OBS}.out\
  --error=./log/${DATA}_${MODEL}_${OBS}.err\
  --job-name=${DATA}_${MODEL}_${OBS}\
  ./model/model_hpc_more.sbatch
  
  sleep 1 # pause to be kind to the scheduler
done
  
for MODEL in PA_LMA_CV PA_LMA_L_CV PA_LMAms_CV PA_LMAms_L_CV PA_LD_L_CV
do
  #
  echo "${MODEL}, ${DATA}, ${OBS}"
  export MODEL DATA OBS
  
  sbatch --job-name=${DATA}_${MODEL}_${OBS}\
  --output=./log/${DATA}_${MODEL}_${OBS}.out\
  --error=./log/${DATA}_${MODEL}_${OBS}.err\
  --job-name=${DATA}_${MODEL}_${OBS}\
  ./model/model_hpc_cv.sbatch
  
  sleep 1 # pause to be kind to the scheduler
done
