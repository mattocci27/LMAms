for MODEL in model1 model2 model3
do
  #
  echo "${MODEL}"
  export MODEL
  #
  sbatch --job-name=PA${MODEL}\
  --output=PA_${MODEL}.out\
  --error=PA_${MODEL}.err\
  --job-name=PA_${MODEL}\
  PA_obs.sbatch
  #
  sleep 30 # pause to be kind to the scheduler
done
