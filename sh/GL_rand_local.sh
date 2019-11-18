#!/bin/bash
OBS=rand
DATA=GL
MODEL=GL_LMAms

R --vanilla --slave --args ${MODEL} ${DATA} 4000 3000 1 ${OBS} < ./model/model.r > ./log/${MODEL}_${OBS}.log

