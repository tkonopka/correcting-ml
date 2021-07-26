# prepare ensembles of models for the mnist dataset

library(data.table)
library(glue)
library(xgboost)
library(mlensemble)
source("prep_mnist_common.R")
source(file.path("..", "R", "pixels.R"))

set.seed(86420)

# file paths and file path templates
mnist_model_template <- mnist_path("model_{VARIANT}_{REP}.Rda")
mnist_ensemble_template <- mnist_path("ensemble_{VARIANT}.Rda")
mnist_performance_file <- mnist_path("ensemble_performance.tsv.gz")
mnist_calibration_file <- mnist_path("ensemble_calibration_performance.tsv.gz")
mnist_imputation_file <- mnist_path("ensemble_imputation_performance.tsv.gz")


# define ensembles
mnist_variants <- c("original", "subset", "mislabel", "missing", "lost",
                    "top", "bottom", "left", "right",
                    "dropout", "noise", "erased", "spot", "ramp")


#' create and calibrate an ml_ensemble for a variant of the mnist dataset
#' (calibration always uses the mnist_original data (holdout)
create_calibrate_ensemble <- function(variant) {
  ensemble_file <- glue(mnist_ensemble_template, VARIANT=variant)
  if (file.exists(ensemble_file)) {
    load(ensemble_file)
    return(ensemble)
  }
  if (variant=="original") {
    load(glue(mnist_model_template, VARIANT=variant, REP=1))
    model_1 <- ml_model(model, name=paste0(variant, "_1"))
    ensemble <- calibrate(ml_ensemble()+model_1,
                          mnist_original_parts$holdout,
                          mnist_original_parts$holdout_label)
  } else {
    load(glue(mnist_model_template, VARIANT=variant, REP=1))
    model_1 <- ml_model(model, name=paste0(variant, "_1"))
    load(glue(mnist_model_template, VARIANT=variant, REP=2))
    model_2 <- ml_model(model, name=paste0(variant, "_2"))
    rm(model)
    ensemble <- calibrate(model_1 + model_2,
                          mnist_original_parts$holdout,
                          mnist_original_parts$holdout_label)
  }
  save(ensemble, file=ensemble_file)
  ensemble
}


if (!exists("mnist_ensembles")) {
  mnist_ensembles <- lapply(setNames(as.list(mnist_variants), mnist_variants),
                            create_calibrate_ensemble)
}


if (!exists("mnist_performance")) {
  make_mnist_performance <- function() {
    result <- data.table(
      expand.grid(list(variant=mnist_variants,
                       rep=c("ensemble_uncalibrated", "ensemble_calibrated")),
                  stringsAsFactors=FALSE
      ))
    for (v in mnist_variants) {
      .mop <- mnist_original_parts
      .ensemble <- mnist_ensembles[[v]]
      # evaluate calibrated model
      .perf <- eval_xgboost(.ensemble, .mop, datasets="original_test",
                            label_names=seq(0, 9))
      result[variant==v & rep=="ensemble_calibrated",
             names(.perf) := as.list(.perf)]
      .ensemble$calibration <- NA
      .perf <- eval_xgboost(.ensemble, .mop, datasets="original_test",
                            label_names=seq(0, 9))
      result[variant==v & rep=="ensemble_uncalibrated",
             names(.perf) := as.list(.perf)]

    }
    result
  }
  mnist_performance <- read_or_make(mnist_performance_file,
                                    make_mnist_performance)
}


# measure class balance with/without calibration
if (!exists("mnist_calibration")) {
  make_mnist_calibration <- function() {
    result <- data.table(
      expand.grid(list(variant=c("noise", "original", "subset"),
                       rep=c("ensemble_calibrated", "ensemble_recalibrated"),
                       bg=seq(0, 100, by=10)),
                  stringsAsFactors=FALSE))
    for (.bg in seq(0, 100, by=10)) {
      # load data at the specified bg noise level
      bg_data <- fread(glue(mnist_data_template, VARIANT="bg", REP=.bg))
      bg_data_parts <- split_dataset(
        as.matrix(bg_data[, mnist_pixel_columns, with=FALSE]),
        bg_data$label,
        mnist_samples$dataset)
      bg_data_parts$train <- NULL
      bg_data_parts$train_label <- NULL
      for (.v in unique(result$variant)) {
        .ensemble <- create_calibrate_ensemble(.v)
        .perf <- eval_class_balance(.ensemble, bg_data_parts, dataset="test",
                                    label_names=seq(0, 9))
        result[variant==.v & rep=="ensemble_calibrated" & bg==.bg,
               names(.perf) := as.list(.perf)]
        # ensemble recalibrated with noisy data
        .ensemble2 <- calibrate(.ensemble, bg_data_parts$holdout,
                                bg_data_parts$holdout_label)
        .perf2 <- eval_class_balance(.ensemble2, bg_data_parts, dataset="test",
                                     label_names=seq(0, 9))
        result[variant==.v & rep=="ensemble_recalibrated" & bg==.bg,
               names(.perf2) := as.list(.perf2)]
        rm(.perf, .perf2, .ensemble, .ensemble2)
      }
    }
    result
  }
  mnist_calibration <- read_or_make(mnist_calibration_file,
                                    make_mnist_calibration)
}


# measure class balance with/without imputation
if (!exists("mnist_imputation")) {
  make_mnist_imputation <- function() {
    result <- data.table(
      expand.grid(list(variant=c("missing", "original", "subset"),
                       rep=c("ensemble_calibrated", "ensemble_imputed"),
                       na=seq(0, 80, by=10)),
                  stringsAsFactors=FALSE))
    for (.na in seq(0, 80, by=10)) {
      print(.na)
      # load data at the specified bg noise level
      na_data <- fread(glue(mnist_data_template, VARIANT="na", REP=.na))
      na_data_parts <- split_dataset(
        as.matrix(na_data[, mnist_pixel_columns, with=FALSE]),
        na_data$label,
        mnist_samples$dataset)
      na_data_parts$train <- NULL
      na_data_parts$train_label <- NULL
      na_data_parts$test_imputed <-
        t(apply(na_data_parts$test, 1, impute_missing_pixels))
      na_data_parts$test_imputed_label <- na_data_parts$test_label
      for (.v in unique(result$variant)) {
        .ensemble <- create_calibrate_ensemble(.v)
        .perf <- eval_class_balance(.ensemble, na_data_parts, dataset="test",
                                    label_names=seq(0, 9))
        result[variant==.v & rep=="ensemble_calibrated" & na==.na,
               names(.perf) := as.list(.perf)]
        # ensemble recalibrated with noisy data
        .perf2 <- eval_class_balance(.ensemble, na_data_parts, dataset="test_imputed",
                                     label_names=seq(0, 9))
        names(.perf2) <- gsub("test_imputed", "test", names(.perf2))
        result[variant==.v & rep=="ensemble_imputed" & na==.na,
               names(.perf2) := as.list(.perf2)]
        rm(.perf, .perf2, .ensemble, .ensemble2)
      }
    }
    result
  }
  mnist_imputation <- read_or_make(mnist_imputation_file,
                                   make_mnist_imputation)
}

