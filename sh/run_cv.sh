#!/bin/bash
nohup sh ./sh/model_csv_cloud.sh &
wait
python line.py "host - ${HOSTNAME}: MCMC for CV done!"
