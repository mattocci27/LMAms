#!/bin/bash
DATA=GL
OBS=obs
for MODEL in GL_LMAms_CV GL_LMA_CV
do
  #
  echo "${MODEL}, ${DATA}, ${OBS}"
  export MODEL DATA OBS
  nohup R --vanilla --slave --args ${MODEL} ${DATA} 2 1 1 ${OBS} < ./model/k_fold.r > ./log/${MODEL}_${OBS}.log &

  sleep 1 # pause to be kind to the scheduler
done

DATA=PA
for MODEL in PA_LMA_CV PA_LMA_L_CV PA_LMAms_CV PA_LMAms_L_CV PA_LD_L_CV
do
  #
  echo "${MODEL}, ${DATA}, ${OBS}"
  export MODEL DATA OBS
  nohup R --vanilla --slave --args ${MODEL} ${DATA} 2 1 1 ${OBS} < ./model/k_fold.r > ./log/${MODEL}_${OBS}.log &

  sleep 1 # pause to be kind to the scheduler
done
