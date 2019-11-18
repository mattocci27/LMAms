#!/bin/bash
OBS=rand
DATA=PA
MODEL=PA_LMAms_L0

R --vanilla --slave --args ${MODEL} ${DATA} 4000 3000 1 ${OBS} < ./model/model_more.r > ./log/${MODEL}_${OBS}.log

