# LMA


### Singularity

To build singularity image

```
time sudo singularity build singularity.sif singularity.def
```

### Analysis (MCMC)

```
singularity exec singularity.sif ./sh/run_model.sh
```

or

```
singularity shell singularity.sif
sh ./sh/run_model.sh
```

### Subsequent analyses and manuscript

```
make
```

