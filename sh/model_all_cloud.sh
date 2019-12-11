#!/bin/bash
nohup sh ./sh/model_cv_cloud.sh &
nohup sh ./sh/model_obs_cloud.sh &
nohup sh ./sh/model_rand_cloud.sh &
wait
