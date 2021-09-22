#!/bin/bash

# for best models ---------------------------------------------------------------
OBS=obs
#DATA=GL
#MODEL=GL_LMAms
#echo "${MODEL}, ${DATA}, ${OBS}"
#export MODEL DATA OBS
#nohup R --vanilla --slave --args ${MODEL} ${DATA} 4000 3000 1 ${OBS} < ./model/model.r > ./log/${MODEL}_${OBS}.log &
#sleep 1 # pause to be kind to the scheduler

DATA=PA
for MODEL in PA_LMAms_L1 PA_LMAms_L2 PA_LMAms_L3 PA_LMAms_L4
do
  #
  echo "${MODEL}, ${DATA}, ${OBS}"
  export MODEL DATA OBS
  nohup R --vanilla --slave --args ${MODEL} ${DATA} 4000 3000 1 ${OBS} < ./model/model.r > ./log/${MODEL}_${OBS}.log &
  sleep 1 # pause to be kind to the scheduler
done
