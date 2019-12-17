#!/bin/bash
nohup sh ./sh/model_csv_cloud.sh &
wait

python line.py "host - ${HOSTNAME}: MCMC for CV done!"

sh ./sh/mv_dat.sh local_to_dropbox

python line.py "${HOSTNAME}: moved local files to Dropbox!"
