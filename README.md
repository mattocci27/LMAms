# Metabolic and structural leaf mass

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)


Code repostitory to run the analysis and generate the manuscript for Katabuchi et al. "Decomposing leaf mass into metabolic and structural components explains divergent patterns of trait variation within and among plant species"

Masatoshi Katabuchi,
Kaoru Kitajima,
S. Joseph Wright,
Sunshine A. Van Bael,
Jeanne L. D. Osnas and
Jeremy W. Lichstein

## Reproduce the results

Codes (R and STAN) and workflow are managed with the R package `targets` (https://github.com/ropensci/targets).

### Running code on local

To run analysis:

```bash
# To install R packages for the first run
# Rscript -e "renv::restore()"
Rscript run.R
```

To generate the manuscript:

```bash
make
```

Requirements:

- cmdstan 2.29.2
- quarto
- latexdiff
- R (4.2.0)
	- renv (`renv::restore()` will install all the R packages)

### Running code in Apptainer (Linux)

First, change `RENV_PATHS_CACHE` in `radian.def` and `tinytex.def` to your path (i.e.,
`
RENV_PATHS_CACHE=<your_path>"
`
).

To build Apptainer containers:

```bash
sudo apptainer build radian.sif radian.def
sudo apptainer build tinytex.sif tinytex.def
```

To run analysis:

```bash
# To install R packages for the first run
# apptainer exec radian.sif Rscript -e "renv::restore()"
apptainer exec radian.sif Rscript run.R
```

To generate the manuscript:

```bash
# To install R packages for the first run
# Rscript -e "renv::restore()"
apptainer exec tinytex.sif make
apptainer exec tinytex.sif make diff
```

Requirements:

- Apptainer (or singularity)
- cmdstan 2.29.2 (radian.sif does not contain cmdstan)

