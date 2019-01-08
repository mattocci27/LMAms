for MODEL in model1 model2 model3 model4
do
  #
  echo "${MODEL}"
  export MODEL
  #
  sbatch --job-name=./log/PA${MODEL}\
  --output=./log/PA_${MODEL}.out\
  --error=./log/PA_${MODEL}.err\
  --job-name=PA_${MODEL}\
  ./sh/PA_pow.sbatch
  #
  sleep 1 # pause to be kind to the scheduler
done


for MODEL in model_LMA1 model_LMA2 model_LMA3 model_LMA1p model_LMA2p model_LMA3p
do
  echo "${MODEL}"
  export MODEL
  #
  sbatch --job-name=./log/PA_${MODEL}\
  --output=./log/PA_${MODEL}.out\
  --error=./log/PA_${MODEL}.err\
  --job-name=PA_${MODEL}\
  ./sh/PA_LMA.sbatch
  #
  sleep 1 # pause to be kind to the scheduler
done


for MODEL in model4 model5
do
  #
  echo "${MODEL}"
  export MODEL
  #
  sbatch --job-name=./log/PA${MODEL}\
  --output=./log/PA_${MODEL}.out\
  --error=./log/PA_${MODEL}.err\
  --job-name=PA_${MODEL}\
  ./sh/PA_pow.sbatch
  #
  sleep 1 # pause to be kind to the scheduler
done

