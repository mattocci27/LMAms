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
singularity shell ../dockerfiles/singularity/rstan_3.6.3.sif
time sh ./sh/run_model.sh
```


### Check MCMC

```
singularity exec ../dockerfiles/singularity/myenv_3.6.3.sif \
  Rscript -e "library(rmarkdown); render('check_obs.rmd')"
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
  - `PA_LMAms0_more_obs.rda`

- rand
  - `GL_LMAms_more_rand.rda`
  - `PA_LMAms_L0_more_rand.rda`

```{r}

library(MakeR2)

#source("~/Dropbox/src/github.com/mattocci27/makeR/R/make_make.r")
make_fun(clean = TRUE)
plan <- read_make("Makefile")
vis_fun(plan)
plan$nodes %>% DT::datatable(.)

```



```

singularity shell ../dockerfiles/singularity/myenv_3.6.3.sif

docker run --rm -v $(pwd):/home/rstudio/LMAms  \
  -p 8787:8787  \
  -e PASSWORD=F85hPRItkcsaQ7lR6AHK \
  mattocci/myenv:3.6.3
```

```{r}
Rscript res_rand.r
Rscript util/r2_yml.r
Rscript util/res_para.r
Rscript util/get_loo.r
#Rscript fig_code/fig.r
#Rscript fig_code/fig_SI.r
```

```
singularity exec ../r-containers/myenv_3.6.3.sif \
  Rscript -e "library(rmarkdown); render('fig_code/fig.rmd')"

singularity shell ../r-containers/rmd-crossref_4.0.2.sif

singularity exec ../r-containers/rmd-crossref_4.0.2.sif \
  Rscript -e "library(rmarkdown); render('ms/LMAps_main_re.rmd', 'html_document')"
  

Rscript -e "library(rmarkdown); render('ms/LMAps_main_re.rmd', 'html_document')"
Rscript -e "library(rmarkdown); render('ms/LMAps_main_re.rmd', 'bookdown::word_document2')"
Rscript -e "library(rmarkdown); render('ms/LMAps_main_re.rmd', 'bookdown::pdf_book')"

singularity exec ../r-containers/rmd-crossref_4.0.2.sif \
  sh render.sh

```

