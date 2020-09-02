#!/bin/bash

# run up to 3 models for each time
# model_more.r or model.r don't matter for GL data
# LMA -----------------------------------------------------------------------
DATA=GL
OBS=obs
MODEL=GL_LMA
echo "${MODEL}, ${DATA}, ${OBS}"
export MODEL DATA OBS
nohup R --vanilla --slave --args ${MODEL} ${DATA} 4000 3000 1 ${OBS} < ./model/model.r > ./log/${MODEL}_${OBS}.log &
sleep 1 # pause to be kind to the scheduler

# small sample for model selection 
DATA=PA
for MODEL in PA_LMA PA_LMA_L
do
  #
  echo "${MODEL}, ${DATA}, ${OBS}"
  export MODEL DATA OBS
  nohup R --vanilla --slave --args ${MODEL} ${DATA} 4000 3000 1 ${OBS} < ./model/model.r > ./log/${MODEL}_${OBS}.log &
  sleep 1 # pause to be kind to the scheduler
done
wait

# LMAms and LD --------------------------------------------------------------
# small sample for model selection 
DATA=PA
for MODEL in PA_LMAms0 PA_LMAms_L0 PA_LD_L
do
  #
  echo "${MODEL}, ${DATA}, ${OBS}"
  export MODEL DATA OBS
  nohup R --vanilla --slave --args ${MODEL} ${DATA} 4000 3000 1 ${OBS} < ./model/model.r > ./log/${MODEL}_${OBS}.log &
  sleep 1 # pause to be kind to the scheduler
done
wait

# for best models ---------------------------------------------------------------
DATA=GL
MODEL=GL_LMAms
echo "${MODEL}, ${DATA}, ${OBS}"
export MODEL DATA OBS
nohup R --vanilla --slave --args ${MODEL} ${DATA} 4000 3000 1 ${OBS} < ./model/model_more.r > ./log/${MODEL}_${OBS}.log &
sleep 1 # pause to be kind to the scheduler

DATA=PA
MODEL=PA_LMAms_L
echo "${MODEL}, ${DATA}, ${OBS}"
export MODEL DATA OBS
nohup R --vanilla --slave --args ${MODEL} ${DATA} 4000 3000 1 ${OBS} < ./model/model_more.r > ./log/${MODEL}_${OBS}.log &
sleep 1 # pause to be kind to the scheduler
