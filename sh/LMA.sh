for MODEL in model1 model2 model3
do
  for NULL_DATA in LMA_ra_data LMA_rc_data
  do
    #
    echo "${MODEL}, ${NULL_DATA}"
    export MODEL NULL_DATA
    #
    sbatch --job-name=PA${MODEL}\
    --output=PA_${MODEL}_${NULL_DATA}.out\
    --error=PA_${MODEL}_${NULL_DATA}.err\
    --job-name=PA_${MODEL}\
    LMA.sbatch
    #
  sleep 1 # pause to be kind to the scheduler
  done
done


for MODEL in model1 model2 model3
do
  for NULL_DATA in LMA_rw_data
  do
    #
    echo "${MODEL}, ${NULL_DATA}"
    export MODEL NULL_DATA
    #
    sbatch --job-name=PA${MODEL}\
    --output=PA_${MODEL}_${NULL_DATA}.out\
    --error=PA_${MODEL}_${NULL_DATA}.err\
    --job-name=PA_${MODEL}\
    LMA.sbatch
    #
  sleep 30 # pause to be kind to the scheduler
  done
done

for MODEL in model1 model2 model3
do
  for NULL_DATA in LMA_rs_data
  do
    #
    echo "${MODEL}, ${NULL_DATA}"
    export MODEL NULL_DATA
    #
    sbatch --job-name=PA${MODEL}\
    --output=PA_${MODEL}_${NULL_DATA}.out\
    --error=PA_${MODEL}_${NULL_DATA}.err\
    LMA.sbatch
    #
  sleep 30 # pause to be kind to the scheduler
  done
done





for MODEL in model1 model2 model3
do
  for NULL_DATA in LMA_rc_data
  do
    #
    echo "${MODEL}, ${NULL_DATA}"
    export MODEL NULL_DATA
    #
    sbatch --job-name=PA${MODEL}\
    --output=PA_${MODEL}_${NULL_DATA}_99.out\
    --error=PA_${MODEL}_${NULL_DATA}_99.err\
    LMA.sbatch
    #
  sleep 30 # pause to be kind to the scheduler
  done
done



for MODEL in model1 model2 model3
do
  for NULL_DATA in LMA_rc_data
  do
    #
    echo "${MODEL}, ${NULL_DATA}, ${RUNS}"
    export MODEL NULL_DATA RUNS=49
    #
    sbatch --job-name=PA_${MODEL}_${RUNS}\
    --output=PA_${MODEL}_${NULL_DATA}_${RUNS}.out\
    --error=PA_${MODEL}_${NULL_DATA}_${RUNS}.err\
    LMA.sbatch
    #
  sleep 30 # pause to be kind to the scheduler
  done
done


for MODEL in model1 model2 model3
do
  for NULL_DATA in LMA_rc_data
  do
    #
    echo "${MODEL}, ${NULL_DATA}, ${RUNS}"
    export MODEL NULL_DATA RUNS=50
    #
    sbatch --job-name=PA_${MODEL}_${RUNS}\
    --output=PA_${MODEL}_${NULL_DATA}_${RUNS}.out\
    --error=PA_${MODEL}_${NULL_DATA}_${RUNS}.err\
    LMA2.sbatch
    #
  sleep 30 # pause to be kind to the scheduler
  done
done
