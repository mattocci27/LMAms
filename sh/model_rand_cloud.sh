#!/bin/bash
OBS=rand
DATA=GL
MODEL=GL_LMAms

nohup R --vanilla --slave --args ${MODEL} ${DATA} 4000 3000 1 ${OBS} < ./model/model.r > ./log/${MODEL}_${OBS}.log &&  python line.py "$DATA $OBS $MODEL done!" &

DATA=PA
MODEL=PA_LMAms_L0

nohup R --vanilla --slave --args ${MODEL} ${DATA} 4000 3000 1 ${OBS} < ./model/model_more.r > ./log/${MODEL}_${OBS}.log &&  python line.py "$DATA $OBS $MODEL done!" &
wait
