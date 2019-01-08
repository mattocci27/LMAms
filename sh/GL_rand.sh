
for MODEL in model1 model1r4
do
  for NULL_DATA in obs LMA All
  do
    #
    echo "${MODEL}, ${NULL_DATA}"
    export MODEL NULL_DATA
    #
    sbatch --job-name=GL${MODEL}_${NULL_DATA}\
    --output=./log/GL_${MODEL}_${NULL_DATA}.out\
    --error=./log/GL_${MODEL}_${NULL_DATA}.err\
    ./sh/GL_all.sbatch
    #
  sleep 10 # pause to be kind to the scheduler
  done
done


for MODEL in model3r model3r2 model3r5 model1r
do
  for NULL_DATA in obs LMA.all.data2
  do
    #
    echo "${MODEL}, ${NULL_DATA}"
    export MODEL NULL_DATA
    #
    sbatch --job-name=${MODEL}_${NULL_DATA}_PA\
    --output=./log/${MODEL}_${NULL_DATA}_PA.out\
    --error=./log/${MODEL}_${NULL_DATA}_PA.err\
    ./sh/PA_rand.sbatch
    #
  sleep 3 # pause to be kind to the scheduler
  done
done


for MODEL in model3r6 
do
  for NULL_DATA in obs LMA.all.data2
  do
    #
    echo "${MODEL}, ${NULL_DATA}"
    export MODEL NULL_DATA
    #
    sbatch --job-name=${MODEL}_${NULL_DATA}_PA\
    --output=./log/${MODEL}_${NULL_DATA}_PA.out\
    --error=./log/${MODEL}_${NULL_DATA}_PA.err\
    ./sh/PA_rand.sbatch
    #
  sleep 3 # pause to be kind to the scheduler
  done
done


for MODEL in model1 model1r model2 model2r model2r2 model2r3 model3 model3r model3r2 model3r3
do
  for NULL_DATA in obs LMA.all.data
  do
    #
    echo "${MODEL}, ${NULL_DATA}"
    export MODEL NULL_DATA
    #
    sbatch --job-name=${MODEL}_${NULL_DATA}_PA\
    --output=./log/${MODEL}_${NULL_DATA}_PA.out\
    --error=./log/${MODEL}_${NULL_DATA}_PA.err\
    ./sh/PA_rand.sbatch
    #
  sleep 3 # pause to be kind to the scheduler
  done
done


for MODEL in model1 model1r4
do
  for NULL_DATA in obs LMA All
  do
    #
    echo "${MODEL}, ${NULL_DATA}"
    export MODEL NULL_DATA
    #
    sbatch --job-name=GL${MODEL}_${NULL_DATA}\
    --output=./log/GL_${MODEL}_${NULL_DATA}.out\
    --error=./log/GL_${MODEL}_${NULL_DATA}.err\
    ./sh/GL_all.sbatch
    #
  sleep 10 # pause to be kind to the scheduler
  done
done


for MODEL in model1r
do
  for NULL_DATA in obs LMA All
  do
    #
    echo "${MODEL}, ${NULL_DATA}"
    export MODEL NULL_DATA
    #
    sbatch --job-name=GL${MODEL}_${NULL_DATA}\
    --output=./log/GL_${MODEL}_${NULL_DATA}.out\
    --error=./log/GL_${MODEL}_${NULL_DATA}.err\
    ./sh/GL_all.sbatch
    #
  sleep 10 # pause to be kind to the scheduler
  done
done

for MODEL in model1r model1r2 
do
  for NULL_DATA in obs 
  do
    #
    echo "${MODEL}, ${NULL_DATA}"
    export MODEL NULL_DATA
    #
    sbatch --job-name=GL${MODEL}_${NULL_DATA}\
    --output=./log/GL_${MODEL}_${NULL_DATA}.out\
    --error=./log/GL_${MODEL}_${NULL_DATA}.err\
    ./sh/GL_all.sbatch
    #
  sleep 10 # pause to be kind to the scheduler
  done
done
