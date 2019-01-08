#!/bin/bash

DATA=GL
OBS=obs
for MODEL in GL_LMAms GL_LMA
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


