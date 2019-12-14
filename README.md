# LMA

## Codes

To clone this repo,

```

git clone git@github.com:mattocci27/LMAms.git ~/LMAms
git checkout -b CV origin/CV

```

### docker

To build docker image

```
sudo docker build -t mattocci/lma $(pwd)/docker
sudo docker run --rm -it --user rstudio -e PASSWORD=test mattocci/rstan /bin/bash

```

or pull docker image

```
sudo docker pull mattocci/docker-lma
```

```
# shell
sudo docker run --rm -it --user rstudio -e PASSWORD=test mattocci/rstan /bin/zsh

# rstudio
sudo docker run -v $(pwd):/home/rstudio -e PASSWORD=test -p 8787:8787 mattocci/lma
```

### Data cleaning  

To produce GL_data.csv and PA_data.csv

```
Rscript data_cleaning.r
```

### Analysis

- `sh ./model/model_hpc.sh` to run the main analysis on HPC
- `sh ./sim/sim_hpc.sh` to run the simulation analysis on HPC


```
sh ./sh/model_cv_cloud.sh
sh ./sh/model_obs_cloud.sh
sh ./sh/model_rand_cloud.sh

# or 
sh ./sh/model_all_cloud.sh
```

### Results

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

