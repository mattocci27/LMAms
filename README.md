# LMA

## Codes

To clone this repo,

- `git clone https://github.com/mattocci27/LMAms.git`

or 

- `git clone git@github.com:mattocci27/LMAms.git`


### Singularity

To build singularity image

```
time sudo singularity build ../dockerfiles/singularity/rstan_3.6.3.sif docker-daemon://219.72.93.96:5000/mattocci/rstan:3.6.3
```

### Data cleaning  

To produce GL_data.csv and PA_data.csv

```
singularity exec ../dockerfiles/singularity/rstan_3.6.3.sif Rscript data_cleaning.r
```

### Analysis (MCMC)

```
singularity exec ../dockerfiles/singularity/rstan_3.6.3.sif ./sh/run_model.sh
```

or 

```
singularity shell ../dockerfiles/singularity/rstan_3.6.3.sif
time sh ./sh/run_model.sh
```


### Check MCMC

```
singularity exec ../dockerfiles/singularity/myenv_4.0.2.sif \
  Rscript -e "library(rmarkdown); render('check_obs.rmd')"
```

```
singularity exec ../dockerfiles/singularity/myenv_4.0.2.sif \
  Rscript -e "library(rmarkdown); render('check_GL_obs.rmd')"
```

```
singularity exec ../dockerfiles/singularity/myenv_4.0.2.sif \
  Rscript -e "library(rmarkdown); render('check_rand.rmd')"
```

```
singularity exec ../dockerfiles/singularity/myenv_4.0.2.sif \
  Rscript -e "library(rmarkdown); render('check_rand_PA.rmd')"
```

### Results

```
singularity shell ../dockerfiles/singularity/myenv_4.0.2.sif
# inside singularity shell
Rscript util/res_para.r
Rscript util/r2_yml.r
Rscript -e "library(rmarkdown); render('fig_code/fig.rmd')"
```

### Main text

```
# inside singularity shell
sh render.sh
```

