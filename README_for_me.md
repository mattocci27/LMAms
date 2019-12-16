# LMA

## Codes

To clone this repo,

```

cat /var/log/daemon.log

ssh-keygen -t rsa -b 4096 -C "mattocci"

git clone --recurse-submodules git@github.com:mattocci27/LMAms.git ~/LMAms
git checkout -b CV origin/CV
git submodule update --init --recursive


cd ~/dotfiles
sh dropbox.sh link
sh dropbox.sh setup

~/bin/dropbox.py status

```

### docker

To build docker image

```
sudo docker build -t mattocci/docker-lma $(pwd)/docker
sudo docker run --rm -it --user rstudio -e PASSWORD=test mattocci/rstan /bin/bash

```

or pull docker image

```
sudo docker pull mattocci/docker-lma
```

```
# shell
sudo docker run --rm -it --user rstudio -e PASSWORD=test mattocci/lma /bin/bash

sudo docker run --rm -it -v $(pwd):/home/rstudio --user rstudio -e PASSWORD=test mattocci/docker-lma /bin/bash

# rstudio
sudo docker run -v $(pwd):/home/rstudio -e PASSWORD=test -p 8787:8787 mattocci/docker-lma
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

sh ./sh/model_cv_cloud.sh
sh ./sh/model_obs_cloud.sh
sh ./sh/model_rand_cloud.sh

# or 
sh ./sh/model_all_cloud.sh
```

#### Outside of docker

- move rda files to dropbox

```
sh ./sh/mv_dat.sh local_to_dropbox
```


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

Rscript res_rand.r
Rscript util/res_para.r
Rscript util/r2_yml.r
Rscript util/res_para.r
Rscript fig_code/fig.r
Rscript fig_code/fig_SI.r
sh render.sh

```

