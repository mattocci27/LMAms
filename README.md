# LMA

## Codes

To clone this repo,

```
git clone --recurse-submodules git@github.com:mattocci27/LMAms.git ~/LMAms
cd ~/LMAms
git checkout -b CV origin/CV
git submodule update --init --recursive

```

### docker

To build docker image

```
time docker build -f docker-local/Dockerfile -t mattocci/lma:3.6.3 .
docker tag mattocci/lma:3.6.3 192.168.1.123:5000/mattocci/lma:3.6.3 
docker push 192.168.1.123:5000/mattocci/lma:3.6.3 
```

### Singularity

To build singularity image

```
time sudo singularity build ../dockerfiles/singularity/rstan_3.6.3.sif docker-daemon://192.168.1.123:5000/mattocci/rstan:3.6.3
```

### Data cleaning  

To produce GL_data.csv and PA_data.csv

```
Rscript data_cleaning.r
```

### Analysis (MCMC)

```
singularity exec ../dockerfiles/singularity/rstan_3.6.3.sif ./sh/run_model.sh
```

or 

```
singularity ../dockerfiles/singularity/rstan_3.6.3.sif
time sh ./sh/run_model.sh
```


### Results

- r2_yml.r
- res_para.r
- fig.r_
- render.sh

#### List of rda files

note: L0 indicates model without repulsive priors 

- obs
  - `GL_LMAms_more_obs.rda`
  - `PA_LMAms_L0_more_obs.rda` 
  - `PA_LMAms_more_obs.rda`

- rand
  - `GL_LMAms_more_rand.rda`
  - `PA_LMAms_L0_more_rand.rda`


```{r}

Rscript k_fold_cv_tab.r
Rscript res_rand.r
Rscript util/r2_yml.r
Rscript util/res_para.r
Rscript fig_code/fig.r
Rscript fig_code/fig_SI.r
sh render.sh

```

