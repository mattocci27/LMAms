#!/bin/bash
OBS=obs
DATA=PA
for MODEL in PA_LMAms_CV PA_LMAms_L_CV PA_LD_L_CV
do
  #
  echo "${MODEL}, ${DATA}, ${OBS}"
  export MODEL DATA OBS
  nohup R --vanilla --slave --args ${MODEL} ${DATA} 4000 3000 1 ${OBS} < ./model/k_fold.r > ./log/${MODEL}_${OBS}.log && python line.py "$DATA $OBS $MODEL done!" &

  sleep 1 # pause to be kind to the scheduler
done
