# LMA

## Codes

To clone this repo,

```

mail -s "test mail 2" mattocci27@gmail.com <<< "hello 2"

./mhsendmail --from="admin@mailhog.local" mattocci27@gmail.com "test"

docker run -it --link test_mailhog_1 test_lma_1

sudo docker container stats
cat /var/log/daemon.log

curl https://raw.githubusercontent.com/mattocci27/gce-startup-scripts/git/create32.sh | sh -s stan-32

curl https://raw.githubusercontent.com/mattocci27/gce-startup-scripts/git/create16.sh | sh -s stan-16-1

curl https://raw.githubusercontent.com/mattocci27/gce-startup-scripts/git/create16-2.sh | sh -s stan-16-2

curl https://raw.githubusercontent.com/mattocci27/gce-startup-scripts/git/create8.sh | sh -s stan-8

ssh-keygen -t rsa -b 4096 -C "mattocci"

cat ~/.ssh/id_rsa.pub

git clone --recurse-submodules git@github.com:mattocci27/LMAms.git ~/LMAms
cd ~/LMAms
git checkout -b CV origin/CV
git submodule update --init --recursive
mkdir log


cd ~/dotfiles
sh dropbox.sh link
sh dropbox.sh setup

ls ~/Dropbox
# 1 min

sh dropbox.sh exclude

~/bin/dropbox.py status

dropbox exclude remove ~/Dropbox/MS/LMAms/log
cp -rf ~/Dropbox/gce ~/LMAms/gce
#cp -rf ~/Dropbox/MS/LMAms/log ~/LMAms/log

sudo fallocate -l 32G /swapfile
sudo chmod 600 /swapfile
sudo mkswap /swapfile
sudo swapon /swapfile

sudo echo "/swapfile swap swap defaults 0 0" >> /etc/fstab


```

### docker

To build docker image

```

docker run --rm -it mattocci/lma:local /bin/bash
time docker build -t mattocci/lma:local $(pwd)/test
time docker build -t mattocci/lma:local $(pwd)/docker-local
docker run --rm -it -v $(pwd):/home/rstudio/LMAms -u rstudio mattocci/lma:local /bin/bash

```

or pull docker image

```
sudo docker pull mattocci/lma:local
```

```

time docker build -f docker-local/Dockerfile -t mattocci/lma:3.6.3 .
docker tag mattocci/lma:3.6.3 192.168.1.123:5000/mattocci/lma:3.6.3
docker push 192.168.1.123:5000/mattocci/lma:3.6.3
time sudo singularity build lma.sif docker-daemon://192.168.1.123:5000/mattocci/lma:3.6.3

singularity shell ../dockerfiles/singularity/rstan-3.6.3.sif

singularity shell ../dockerfiles/singularity/myenv_3.6.3.sif

singularity shell ../dockerfiles/singularity/rmd-crossref_4.0.2.sif
time sh render.sh

singularity shell lma.sif
time sh ./sh/run_model.sh

docker run --rm -it -v $(pwd):/home/rstudio/LMAms -u rstudio mattocci/rmd-crossref:4.0.2 /bin/bash

# shell
docker run --rm -it --user rstudio -e PASSWORD=test mattocci/lma:local /bin/bash

docker run --rm -it -v $(pwd):/home/rstudio --user rstudio -e PASSWORD=test mattocci/lma:local /bin/bash

docker run --rm -it -v $(pwd):/home/rstudio/LMAms --user rstudio -e PASSWORD=test mattocci/rstan /bin/bash

docker run --rm -it -v $(pwd):/home/rstudio --user rstudio -e PASSWORD=te mattocci/test /bin/bash

docker run -it -v $(pwd):/home/rstudio --user rstudio -e PASSWORD=te mattocci/lma:local /bin/bash

docker run --rm -it -v $(pwd):/home/rstudio -e PASSWORD=KS07L1yiNbXNf9APHxPx mattocci/lma:local


docker run -it -v $(pwd):/home/rstudio --user rstudio -e PASSWORD=te mattocci/rstan /bin/bash

# rstudio


docker run \
  --rm \
  -v $(pwd):/home/rstudio/LMAms \
  -e PASSWORD=F85hPRItkcsaQ7lR6AHK \
  -p 8787:8787 mattocci/rmd-crossref:4.0.2

```

### Data cleaning

To produce GL_data.csv and PA_data.csv

```
Rscript data_cleaning.r
```

### Analysis

- `sh ./model/model_hpc.sh` to run the main analysis on HPC
- `sh ./sim/sim_hpc.sh` to run the simulation analysis on HPC

#### Docker

```

nohup sh ./sh/run_obs_rand.sh &

nohup sh ./sh/model_cv_cloud1.sh &

nohup sh ./sh/model_cv_cloud2.sh &





nohup sh ./line_test.sh &

# 32 cpu
nohup sh ./sh/run_cv.sh &

# 8 cpu
nohup sh ./sh/run_obs_rand.sh &

# 16 cpu
nohup sh ./sh/model_cv_cloud1.sh &

nohup sh ./sh/model_cv_cloud2.sh &

vi ./sh/run_obs_rand.sh
GCE="${HOME}/Dropbox/gce"
OVERWRITE=true
  array=`ls log`
  for f in $array
  do
    # Force remove a dotfile if it's already there
    if [ -f ${f} ] &&
      [ -n "${OVERWRITE}" -a -e ${GCE}/${f} ]; then
      rm -f ${GCE}/${f}
    fi
    if [ ! -e ${GCE}/${f} ]; then
      cp -rf log/${f} ${GCE}/${f}
    fi
  done

```



#### Outside of docker

- move rda files to dropbox

```
sh ./sh/mv_dat.sh local_to_dropbox
```

#### List of rda files

note: L0 indicates model without repulsive priors

- obs
  - `GL_LMAms_more_obs.rda`
  - `PA_LMAms_L0_more_obs.rda`
  - `PA_LMAms_more_obs.rda`

- rand
  - `GL_LMAms_rand.rda`
  - `PA_LMAms_L0_more_rand.rda`

- CV
  - `GL_LMA_CV`
  - `GL_LMAms_CV_obs_cv.rda`
  - `PA_LMA_CV_obs_cv.rda`
  - `PA_LMA_L_CV_obs_cv.rda`
  - `PA_LMAms_CV_obs_cv.rda` need to test this again
  - `PA_LMAms_L_CV_obs_cv.rda`
  - `PA_LD_L_CV_obs_cv.rda`


### Results

- move rda files from dropbox to LMAms dir

```
sh ./sh/mv_dat.sh dropbox_to_local
```

- r2_yml.r
- res_para.r
- fig.r_
- render.sh

```{r}

Rscript k_fold_cv_tab.r

Rscript res_rand.r
Rscript util/res_para.r
Rscript util/r2_yml.r
Rscript util/res_para.r
Rscript fig_code/fig.r
Rscript fig_code/fig_SI.r
sh render.sh

```

```{r}

  load("./data/PA_LMAms_L0_more_obs.rda")
  load("./data/PA_LMAms_more_obs.rda")




DATA=GL
OBS=obs
for MODEL in GL_LMAms_CV
do
  #
  echo "${MODEL}, ${DATA}, ${OBS}"
  export MODEL DATA OBS
  nohup R --vanilla --slave --args ${MODEL} ${DATA} 4000 3000 1 ${OBS} < ./model/k_fold.r > ./log/${MODEL}_${OBS}.log &

  sleep 1 # pause to be kind to the scheduler
done

#!/bin/bash

DATA=GL
OBS=obs
for MODEL in GL_LMA
do
   nohup R --vanilla --slave --args ${MODEL} ${DATA} 4000 3000 1 ${OBS} < ./model/model_more.r > ./log/${DATA}_${MODEL}_${OBS}.log &&  python line.py "$DATA $OBS $MODEL done!"  &
done


```
