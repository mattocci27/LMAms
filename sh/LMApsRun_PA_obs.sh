#!/bin/bash
model=(model1 model2 model3)

for i in {0..2}
do
  R --vanilla --slave --args ${model[i]} 20000 10000 20 < ~/Dropbox/MS/LES_MS/LMApsModel/model/PA_all.r > ~/Dropbox/LES/PA_${model[i]}.log
done

# echo $model
# echo ${model[3]}
