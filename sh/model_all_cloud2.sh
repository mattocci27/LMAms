#!/bin/bash
nohup sh ./sh/model_obs_cloud.sh &
wait
python line.py "host - ${HOSTNAME}: MCMC for obs data done!"

nohup sh ./sh/model_rand_cloud.sh &
wait
python line.py "host - ${HOSTNAME}: MCMC for rand data done!"
