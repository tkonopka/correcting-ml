# correcting-ml

This repository holds analyses demonstrating package [mlensemble](https://github.com/tkonopka/mlensemble) for correcting machine learning models using model ensembles and calibration.


## Prep

The `prep` directory holds scripts that prepare datasets and perform compute-intensive operations (train models, train ensembles, etc.)


## Vignettes

After all the prep scripts have executed, vignettes can be compiled to create figures and additional tables.

```
render("correcting-ml-figures.Rmd")
render("correcting-ml-supplementary-figures.Rmd")
```

