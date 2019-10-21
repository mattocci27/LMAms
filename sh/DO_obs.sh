#!/bin/bash
DATA=GL
OBS=obs

for MODEL in GL_LMA GL_LMAms
do
  for OBS in obs
  do
R --vanilla --slave --args ${MODEL} ${DATA} 4000 3000 1 ${OBS} < ./model/model.r > ./log/${MODEL}_${OBS}.log

  done
done

# k-fold
for MODEL in GL_LMA_CV GL_LMAms_CV
do
  for OBS in obs
  do
R --vanilla --slave --args ${MODEL} ${DATA} 4000 3000 1 ${OBS} < ./model/k_fold.r > ./log/${MODEL}_${OBS}.log

  done
done


DATA=PA
for MODEL in PA_LMA_CV PA_LMA_L_CV 
do
  for OBS in obs
  do
R --vanilla --slave --args ${MODEL} ${DATA} 4000 3000 1 ${OBS} < ./model/k_fold.r > ./log/${MODEL}_${OBS}.log

  done
done
