# analysis of mnist dataset

# load traing/holdout/test splits
if (!exists("mnist_samples")) {
  mnist_samples <- fread(templates$mnist_samples)
}


# load subset of images (for examples)
mnist_head_file <- file.path(dirs$result, "mnist_head.Rda")
if (!exists("mnist")) {
  #' read a subset of the mnist images
  #'
  #' @param templates list with templates for file paths
  #' @param num_rows integer, number of images to load from each file
  #'
  #' @return list with several tables
  read_mnist_head <- function(templates, num_rows=50) {
    result <- list()
    variants <- c("original", "subset", "mislabel", "missing", "lost",
                  "top", "bottom", "left", "right",
                  "dropout", "noise", "erased", "spot",
                  paste0("bg_", seq(0, 100, by=10)),
                  paste0("na_", seq(0, 80, by=10)))
    for (v in variants) {
      vparts <- unlist(strsplit(v, "_"))
      if (length(vparts)==1) {
        vbase <- v
        vrep <- 1
      } else {
        vbase <- vparts[1]
        vrep <- vparts[2]
      }
      result[[v]] <- fread(glue(templates$mnist_data, VARIANT=vbase, REP=vrep),
                                 nrows=num_rows)
    }
    result
  }
  if (file.exists(mnist_head_file)) {
    load(mnist_head_file)
  } else {
    mnist <- read_mnist_head(templates)
    save(mnist, file=mnist_head_file)
  }
}


# load training/holdout assessments as function of hyper-parameter nrounds
if (!exists("mnist_nrounds")) {
 mnist_nrounds <- fread(templates$mnist_nrounds)
}


# load performance evaluations
if (!exists("mnist_performance")) {
  .template <- templates$mnist_performance
  mnist_performance <- list(
    ensemble=rbind(
      fread(glue(.template, TYPE="model")),
      fread(glue(.template, TYPE="ensemble")),
      fill=TRUE),
    calibration=fread(glue(.template, TYPE="ensemble_calibration")),
    imputation=fread(glue(.template, TYPE="ensemble_imputation"))
  )
}
