#!/bin/bash
OBS=obs
DATA=PA

for MODEL in  PA_LMAms_CV PA_LMAms_L_CV PA_LD_L_CV

do
  for OBS in obs
  do
R --vanilla --slave --args ${MODEL} ${DATA} 2 1 1 ${OBS} < ./model/k_fold.r > ./log/${MODEL}_${OBS}.log

  done
done


for MODEL in PA_LMAms_L PA_LMAms PA_LMAms_L0
do
  for OBS in obs
  do
R --vanilla --slave --args ${MODEL} ${DATA} 2 1 1 ${OBS} < ./model/model_more.r > ./log/${MODEL}_${OBS}.log

  done
done
