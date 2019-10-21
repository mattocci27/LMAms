#!/bin/bash
DATA=GL
OBS=obs

#for MODEL in latent_model_ordered05 latent_model_ordered11
#for MODEL in latent_replusive_model_light10 latent_ordered_model_light10
#for model in latent_replusive_model_light11
for MODEL in latent_model_ordered10

do
  for OBS in obs
  do

R --vanilla --slave --args ${MODEL} ${DATA} 4000 3000 1 ${OBS} < ./model/model_more.r > ./log/${DATA}_${MODEL}_${OBS}.log

  done
done
