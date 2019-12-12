#!/bin/bash
DATA=GL
OBS=obs
for MODEL in GL_LMAms
do
    nohup R --vanilla --slave --args ${MODEL} ${DATA} 4 3 1 ${OBS} < ./model/model_more.r > ./log/${DATA}_${MODEL}_${OBS}.log &
done

DATA=PA
for MODEL in PA_LMAms_L0 PA_LMAms0
do
    nohup R --vanilla --slave --args ${MODEL} ${DATA} 4 3 1 ${OBS} < ./model/model_more.r > ./log/${DATA}_${MODEL}_${OBS}.log &
done
