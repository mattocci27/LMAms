#!/bin/bash
DATA=GL
OBS=obs
for MODEL in GL_LMAms_CV
do
  #
  echo "${MODEL}, ${DATA}, ${OBS}"
  export MODEL DATA OBS
  nohup R --vanilla --slave --args ${MODEL} ${DATA} 4000 3000 1 ${OBS} < ./model/k_fold.r > ./log/${MODEL}_${OBS}.log &

  sleep 1 # pause to be kind to the scheduler
done

DATA=PA
for MODEL in PA_LMAms_L_CV
do
  #
  echo "${MODEL}, ${DATA}, ${OBS}"
  export MODEL DATA OBS
  nohup R --vanilla --slave --args ${MODEL} ${DATA} 4000 3000 1 ${OBS} < ./model/k_fold.r > ./log/${MODEL}_${OBS}.log &

  sleep 1 # pause to be kind to the scheduler
done
wait
