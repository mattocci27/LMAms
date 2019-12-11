#!/bin/bash
OBS=rand
DATA=GL
MODEL=GL_LMAms

R --vanilla --slave --args ${MODEL} ${DATA} 4 3 1 ${OBS} < ./model/model.r > ./log/${MODEL}_${OBS}.log

DATA=PA
MODEL=PA_LMAms_L0

R --vanilla --slave --args ${MODEL} ${DATA} 4 3 1 ${OBS} < ./model/model_more.r > ./log/${MODEL}_${OBS}.log
