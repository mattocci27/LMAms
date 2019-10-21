### Codes

#### Data cleaning  
- `Rscript data_cleaning.r` to produce GL_data.csv and PA_data.csv

#### Analysis
- `sh ./model/model_hpc.sh` to run the main analysis on HPC
- `sh ./sim/sim_hpc.sh` to run the simulation analysis on HPC

#### Results

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

*07092018*

Problem:

Randomized data still produce some patterns.

Done:

- Different mean for functional group and use Normal.
- This worked for GL well but not for PA. PA produced clustered patterns.

Doing:
- Chinking Panama Normal null model again (n_inter = 2000) because the previous model did not converged well. 


*07072018*

Problem:
Randomized data still produce some patterns.

Done:

- Checking the original null model (shuffling A, R and LL across species but maintaining LMA values)

Doing:

- Check Rarea model alone

*07062018*

Problem:
Randomized data still produce some patterns.

Done:

- New Kikuzawa model. I added an additional scaling parameter (originally it is 0.5). This removed positive covariance between preLL and LL in the randomized datasets but still produced covariances in other trait combinations.

Doing:

- Checking the original null model (shuffling A, R and LL across species but maintaining LMA values)
