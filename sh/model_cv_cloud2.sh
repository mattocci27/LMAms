#!/bin/bash
OBS=obs
DATA=PA
for MODEL in PA_LMAms_CV PA_LMAms_L_CV PA_LD_L_CV
do
  #
  echo "${MODEL}, ${DATA}, ${OBS}"
  export MODEL DATA OBS
  nohup R --vanilla --slave --args ${MODEL} ${DATA} 4000 3000 1 ${OBS} < ./model/k_fold.r > ./log/${MODEL}_${OBS}.log &

  sleep 1 # pause to be kind to the scheduler
done
wait

python line.py "host - ${HOSTNAME}: MCMC for CV done!"

sh ./sh/mv_dat.sh local_to_dropbox

python line.py "${HOSTNAME}: moved local files to Dropbox!"
