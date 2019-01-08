#!/bin/bash
for OBS in obs rand
#for OBS in rand
do
  for MODEL in potPL pot_diff
  do
    for DATA in GL1 GL2
    #for DATA in GL1 
    do
      #
      echo "${MODEL}, ${DATA}, ${OBS}"
      export MODEL DATA OBS
      #
      sbatch --job-name=${DATA}_${MODEL}_${OBS}_sim\
      --output=./log/${DATA}_${MODEL}_${OBS}_sim.out\
      --error=./log/${DATA}_${MODEL}_${OBS}_sim.err\
      --job-name=${DATA}_${MODEL}_${OBS}_sim\
      ./sim/sim_hpc.sbatch
      #sh ./sim/sim_local.sh
    sleep 1 # pause to be kind to the scheduler
    done
  done
done

for OBS in obs rand
do
  for MODEL in sitePL site_diff
  do
    for DATA in PA
    do
      #
      echo "${MODEL}, ${DATA}, ${OBS}"
      export MODEL DATA OBS
      
      sbatch --job-name=${DATA}_${MODEL}_${OBS}_sim\
      --output=./log/${DATA}_${MODEL}_${OBS}_sim.out\
      --error=./log/${DATA}_${MODEL}_${OBS}_sim.err\
      --job-name=${DATA}_${MODEL}_${OBS}_sim\
      ./sim/sim_hpc.sbatch
      #sh ./sim/sim_local.sh
      
    sleep 1 # pause to be kind to the scheduler
    done
  done
done


MODEL=potPL
DATA=WC
for OBS in obs rand
do
  echo "${MODEL}, ${DATA}, ${OBS}"
  export MODEL DATA OBS
  
  sbatch --job-name=${DATA}_${MODEL}_${OBS}_sim\
  --output=./log/${DATA}_${MODEL}_${OBS}_sim.out\
  --error=./log/${DATA}_${MODEL}_${OBS}_sim.err\
  --job-name=${DATA}_${MODEL}_${OBS}_sim\
  ./sim/sim_hpc.sbatch
  #sh ./sim/sim_local.sh
  
sleep 1 # pause to be kind to the scheduler
done
