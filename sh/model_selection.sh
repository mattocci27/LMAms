#!/bin/bash

# for all models ---------------------------------------------------------------
#OBS=obs
#DATA=GL
#i=1
#for MODEL in GL_Ap_LLps GL_Ap_LLs GL_Aps_LLps GL_Aps_LLs GL_LMA
#do
#  echo "${MODEL}, ${DATA}, ${OBS}"
#  export MODEL DATA OBS
#  nohup R --vanilla --slave --args ${MODEL} ${DATA} 4000 3000 1 ${OBS} < ./model/model.r > ./log/${MODEL}_${OBS}.log &
#  if [ $((i % 3)) = 0 ]; then
#    wait
#  fi
#  ((i+=1))
#  sleep 1 # pause to be kind to the scheduler
#done
#
#DATA=PA
#for MODEL in PA_Ap_LDps_opt \
# PA_Ap_LDs_opt \
# PA_Ap_LLps \
# PA_Ap_LLps_opt \
# PA_Ap_LLs \
# PA_Ap_LLs_opt \
# PA_Aps_LDps_opt \
# PA_Aps_LDs_opt \
# PA_Aps_LLps \
# PA_Aps_LLps_opt \
# PA_Aps_LLs \
# PA_Aps_LLs_opt \
# PA_LMA \
# PA_LMA_opt
#
#do
#  #
#  echo "${MODEL}, ${DATA}, ${OBS}"
#  export MODEL DATA OBS
#  nohup R --vanilla --slave --args ${MODEL} ${DATA} 4000 3000 1 ${OBS} < ./model/model.r > ./log/${MODEL}_${OBS}.log &
#  if [ $((i % 3)) = 0 ]; then
#    wait
#  fi
#  ((i+=1))
#  sleep 1 # pause to be kind to the scheduler
#done
#echo "model selection - done"


OBS=obs
DATA=GL
i=1
for MODEL in GL_Ap_LLps
do
  echo "${MODEL}, ${DATA}, ${OBS}"
  export MODEL DATA OBS
  nohup R --vanilla --slave --args ${MODEL} ${DATA} 4000 3000 1 ${OBS} < ./model/model.r > ./log/${MODEL}_${OBS}.log &
  if [ $((i % 3)) = 0 ]; then
    wait
  fi
  ((i+=1))
  sleep 1 # pause to be kind to the scheduler
done
