#!/bin/bash

# for looic for best models models ---------------------------------------------------------------
OBS=obs
#DATA=GL
#for MODEL in GL_LMAms1 GL_LMAms2 GL_LMAms3 GL_LMAms4
#do
#  echo "${MODEL}, ${DATA}, ${OBS}"
#  export MODEL DATA OBS
#  nohup R --vanilla --slave --args ${MODEL} ${DATA} 4000 3000 1 ${OBS} < ./model/model.r > ./log/${MODEL}_${OBS}.log &
#  sleep 1 # pause to be kind to the scheduler
#done
#
#wait
#
#DATA=PA
#for MODEL in PA_LMAms_L1 PA_LMAms_L2 PA_LMAms_L3 PA_LMAms_L4
#do
#  #
#  echo "${MODEL}, ${DATA}, ${OBS}"
#  export MODEL DATA OBS
#  nohup R --vanilla --slave --args ${MODEL} ${DATA} 4000 3000 1 ${OBS} < ./model/model.r > ./log/${MODEL}_${OBS}.log &
#  sleep 1 # pause to be kind to the scheduler
#done

DATA=PA
MODEL=PA_LMAms_L3
echo "${MODEL}, ${DATA}, ${OBS}"
export MODEL DATA OBS
nohup R --vanilla --slave --args ${MODEL} ${DATA} 4000 3000 1 ${OBS} < ./model/model_more.r > ./log/${MODEL}_${OBS}.log &
