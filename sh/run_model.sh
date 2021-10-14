#!/bin/bash

# for all models ---------------------------------------------------------------
OBS=obs
DATA=PA
for MODEL in PA_Ap_LLs_opt PA_Ap_LLps_opt

do
  #
  echo "${MODEL}, ${DATA}, ${OBS}"
  export MODEL DATA OBS
  nohup R --vanilla --slave --args ${MODEL} ${DATA} 4000 3000 1 ${OBS} < ./model/model_more.r > ./log/${MODEL}_${OBS}_more.log &
  if [ $((i % 3)) = 0 ]; then
    wait
  fi
  ((i+=1))
  sleep 1 # pause to be kind to the scheduler
done
echo "run model - done"
