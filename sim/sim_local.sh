MODEL=site_diff
DATA=PA
OBS=obs

R --vanilla --slave --args ${MODEL} ${DATA} 200 100 1 ${OBS} < ./sim/sim_model_s.r > ./log/${DATA}_${MODEL}_${OBS}.log

