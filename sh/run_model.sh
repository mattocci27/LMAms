#!/bin/bash

set -e

## Obs
# GL-obs
DATA=GL
OBS=obs
N_ITER=2
N_WARM=1

for MODEL in GL_LMAms
do
   nohup R --vanilla --slave --args ${MODEL} ${DATA} ${N_ITER} ${N_WARM} 1 ${OBS} < ./model/model_more.r > ./log/${DATA}_${MODEL}_${OBS}.log &
done

# PA-obs 2 models
DATA=PA
for MODEL in PA_LMAms_L0 PA_LMAms0
do
    nohup R --vanilla --slave --args ${MODEL} ${DATA} ${N_ITER} ${N_WARM} 1 ${OBS} < ./model/model_more.r > ./log/${DATA}_${MODEL}_${OBS}.log &
done
wait #---------------------------------------------------------

## rand
OBS=rand
DATA=GL
MODEL=GL_LMAms

nohup R --vanilla --slave --args ${MODEL} ${DATA} ${N_ITER} ${N_WARM} 1 ${OBS} < ./model/model_more.r > ./log/${MODEL}_${OBS}.log &

DATA=PA
MODEL=PA_LMAms_L0

nohup R --vanilla --slave --args ${MODEL} ${DATA} ${N_ITER} ${N_WARM} 1 ${OBS} < ./model/model_more.r > ./log/${MODEL}_${OBS}.log &
wait #----------------------------------------------------------

## CV
DATA=GL
OBS=obs
for MODEL in GL_LMAms_CV GL_LMA_CV
do
  #
  echo "${MODEL}, ${DATA}, ${OBS}"
  export MODEL DATA OBS
  nohup R --vanilla --slave --args ${MODEL} ${DATA} ${N_ITER} ${N_WARM} 1 ${OBS} < ./model/k_fold.r > ./log/${MODEL}_${OBS}.log &

  sleep 1 # pause to be kind to the scheduler
done

DATA=PA
for MODEL in PA_LMA_CV PA_LMA_L_CV 
do
  #
  echo "${MODEL}, ${DATA}, ${OBS}"
  export MODEL DATA OBS
  nohup R --vanilla --slave --args ${MODEL} ${DATA} ${N_ITER} ${N_WARM} 1 ${OBS} < ./model/k_fold.r > ./log/${MODEL}_${OBS}.log &

  sleep 1 # pause to be kind to the scheduler
done
wait #----------------------------------------------------------

DATA=PA
for MODEL in PA_LMAms_CV PA_LMAms_L_CV PA_LD_L_CV
do
  #
  echo "${MODEL}, ${DATA}, ${OBS}"
  export MODEL DATA OBS
  nohup R --vanilla --slave --args ${MODEL} ${DATA} ${N_ITER} ${N_WARM} 1 ${OBS} < ./model/k_fold.r > ./log/${MODEL}_${OBS}.log &

  sleep 1 # pause to be kind to the scheduler
done
wait #----------------------------------------------------------
