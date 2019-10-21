#!/bin/bash
DATA=GL
OBS=obs

for MODEL in GL_LMA GL_LMAms GL_LMA_CV GL_LMAms_CV
do
  for OBS in obs
  do
R --vanilla --slave --args ${MODEL} ${DATA} 200 100 1 ${OBS} < ./model/model_more.r > ./log/${DATA}_${MODEL}_${OBS}.log

  done
done


DATA=PA

#  LMA: PA_LMA_CV
#  LMA + light: PA_LMA_L_CV
#  LMAm + LMAs: PA_LMAms_CV
#  LMAm + LMAs + light:  PA_LMAms_L_CV
#  LMAm + LMAs/LT + light: PA_LD_L_CV


#  LMAm + LMAs + light (non-repulsive): PA_LMAms_L0

for MODEL in PA_LMAms_L PA_LMAms PA_LMAms_L0
do
  for OBS in obs
  do
R --vanilla --slave --args ${MODEL} ${DATA} 200 100 1 ${OBS} < ./model/model_more.r > ./log/${DATA}_${MODEL}_${OBS}.log

  done
done
