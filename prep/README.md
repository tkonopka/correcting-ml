# prep

Data preparation scripts. 


## mnist datasets

The prep scripts for the mnist dataset are split into separate files. 
They should be run in the following order.

```
# download and prepare the original dataset and variants for analysis
source("prep_mnist_download.R")
source("prep_mnist_data.R")
# train machine learning models on the original adataset and its variants
source("prep_mnist_models.R")
# create model ensembles
source("prep_mnist_ensembles.R")
```


## testis datasets

Prep scripts for the testis analysis should be run in the following order.

```
# download and prepare dataset with single-cell gene expression
source("prep_testis_download.R")
source("prep_testis_data.R")
# train machine learning models and create ensembles
source("prep_testis_ensembles.R")
```

