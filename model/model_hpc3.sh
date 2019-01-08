#!/bin/bash
DATA=PA
#MODEL=LMAm_LMAsLT32_std
#MODEL=LMAm_LMAsLT33_std
DATA=GL
for OBS in obs
do
  for MODEL in latent_repulsive_model3
  #for MODEL in latent_repulsive_model latent_model latent_repulsive_model_gr latent_model_gr
  do
  #
  echo "${MODEL}, ${DATA}, ${OBS}"
  export MODEL DATA OBS
  #
  sbatch --job-name=${DATA}_${MODEL}_${OBS}\
  --output=./log/${DATA}_${MODEL}_${OBS}.out\
  --error=./log/${DATA}_${MODEL}_${OBS}.err\
  --job-name=${DATA}_${MODEL}_${OBS}\
  ./model/model_hpc.sbatch
  sleep 1 # pause to be kind to the scheduler
  done
done

DATA=PA
for MODEL in latent_replusive_model_light2_CV latent_repulsive_model_cv
do
for OBS in obs
do
  #
  echo "${MODEL}, ${DATA}, ${OBS}"
  export MODEL DATA OBS
  #
  sbatch --job-name=${DATA}_${MODEL}_${OBS}\
  --output=./log/${DATA}_${MODEL}_${OBS}.out\
  --error=./log/${DATA}_${MODEL}_${OBS}.err\
  --job-name=${DATA}_${MODEL}_${OBS}\
  ./model/model_hpc_more.sbatch
  sleep 1 # pause to be kind to the scheduler
done
done

DATA=GL
#for MODEL in latent_replusive_model_light2_CV latent_repulsive_model_cv
for MODEL in GL_LMA_CV GL_LMAms_CV
do
for OBS in obs
do
  #
  echo "${MODEL}, ${DATA}, ${OBS}"
  export MODEL DATA OBS
  #
  sbatch --job-name=${DATA}_${MODEL}_${OBS}\
  --output=./log/${DATA}_${MODEL}_${OBS}.out\
  --error=./log/${DATA}_${MODEL}_${OBS}.err\
  --job-name=${DATA}_${MODEL}_${OBS}\
  ./model/model_hpc_cv.sbatch
  sleep 1 # pause to be kind to the scheduler
done
done


DATA=PA
for OBS in obs
do
  for MODEL in latent_replusive_model_light2_LT
  #for MODEL in latent_repulsive_model latent_model latent_repulsive_model_gr latent_model_gr
  do
  #
  echo "${MODEL}, ${DATA}, ${OBS}"
  export MODEL DATA OBS
  #
  sbatch --job-name=${DATA}_${MODEL}_${OBS}\
  --output=./log/${DATA}_${MODEL}_${OBS}.out\
  --error=./log/${DATA}_${MODEL}_${OBS}.err\
  --job-name=${DATA}_${MODEL}_${OBS}\
  ./model/model_hpc.sbatch
  sleep 1 # pause to be kind to the scheduler
  done
done


DATA=PA
for MODEL in latent_replusive_model_light2
do
for OBS in rand
do
  #
  echo "${MODEL}, ${DATA}, ${OBS}"
  export MODEL DATA OBS
  #
  sbatch --job-name=${DATA}_${MODEL}_${OBS}\
  --output=./log/${DATA}_${MODEL}_${OBS}.out\
  --error=./log/${DATA}_${MODEL}_${OBS}.err\
  --job-name=${DATA}_${MODEL}_${OBS}\
  ./model/model_hpc_more.sbatch
  sleep 1 # pause to be kind to the scheduler
done
done

render("md/note_LT32.rmd")
render("md/note_LT33.rmd")
render("md/PA_note_LT32_r.rmd")
render("md/PA_note_LT33_r.rmd")

render("md/note_m1q_more_std.rmd")
render("md/note_m1q_more_std2.rmd")
render("md/note_m1q_more4.rmd")
render("md/note_m1q_more5.rmd")

render("md/note_m1q_std_r.rmd")
render("md/note_m1q_std_r2.rmd")

MODEL=PA_LMAms_L0
OBS=obs
DATA=PA
R --vanilla --slave --args ${MODEL} ${DATA} 4000 3000 1 ${OBS} < ./model/model_more.r > ./log/${DATA}_${MODEL}_${OBS}2.log

MODEL=GL_LMA
OBS=obs
DATA=GL
R --vanilla --slave --args ${MODEL} ${DATA} 4000 3000 1 ${OBS} < ./model/model.r > ./log/${DATA}_${MODEL}_${OBS}2.log



DATA=PA
OBS=obs
for MODEL in PA_LMAms_CV
do
  #
  echo "${MODEL}, ${DATA}, ${OBS}"
  export MODEL DATA OBS
  
  sbatch --job-name=${DATA}_${MODEL}_${OBS}\
  --output=./log/${DATA}_${MODEL}_${OBS}.out\
  --error=./log/${DATA}_${MODEL}_${OBS}.err\
  --job-name=${DATA}_${MODEL}_${OBS}\
  ./model/model_hpc_cv.sbatch
  
  sleep 1 # pause to be kind to the scheduler
done
