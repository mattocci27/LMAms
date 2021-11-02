#!/bin/bash
OBS=rand
DATA=GL
MODEL=GL_Aps_LLs

nohup R --vanilla --slave --args ${MODEL} ${DATA} 4000 3000 1 ${OBS} < ./model/model.r > ./log/${MODEL}_${OBS}.log &

DATA=PA
MODEL=PA_Ap_LLs_opt

nohup R --vanilla --slave --args ${MODEL} ${DATA} 4000 3000 1 ${OBS} < ./model/model_more.r > ./log/${MODEL}_${OBS}.log &
wait
