# prepare models for the mnist dataset

library(data.table)
library(glue)
library(xgboost)
source("prep_mnist_common.R")

set.seed(86420)

# file paths and file path templates
mnist_samples_file <- mnist_path("samples.tsv.gz")
mnist_data_template <- mnist_path("data_{VARIANT}_{REP}.tsv.gz")
mnist_model_template <- mnist_path("model_{VARIANT}_{REP}.Rda")
mnist_performance_file <- mnist_path("model_performance.tsv.gz")
mnist_nrounds_file <- mnist_path("nrounds.tsv.gz")


# #############################################################################
# train xgboost models for different variants of the data


#' train xgboost models with a range of nround, evaluate performance
#' on a holdout set, find the best value of nround
#'
#' @param data matrix
#' @param label vector
#' @param sample_label character vector
#' @param num_class integer, number of classes to predict
#' @param try_nround integer vector, values for nround parameter
#'
#' @return numeric vector with performance for various nrounds
train_nround_series <- function(data, label, sample_label,
                        num_class=length(unique(label)),
                        try_nround=seq(5, 50, by=5)) {
  parts <- split_dataset(data, label, sample_label)
  result_holdout <- rep(0, length(try_nround))
  result_train <- rep(0, length(try_nround))
  for (i in seq_along(try_nround)) {
    .model <- xgboost(parts$train, label=parts$train_label,
                      num_class=num_class, objective="multi:softmax",
                      nround=try_nround[i], verbose=1)
    .prediction <- predict(.model, parts$holdout)
    result_holdout[i] <- mean(.prediction == parts$holdout_label)
    .prediction <- predict(.model, parts$train)
    result_train[i] <- mean(.prediction == parts$train_label)
    rm(.model, .prediction)
    gc()
  }
  data.table(nround=try_nround,
             score_train=result_train,
             score_holdout=result_holdout)
}


#' train an xgboost model to predict labels
#'
#' @param data matrix
#' @param label vector
#' @param sample_label character vector
#' @param try_nround integer vector, candidate values for nround
#'
#' @return xgboost model
train_xgboost <- function(data, label, sample_label,
                          try_nround=seq(5, 50, by=5)) {
  .nround <- try_nround
  .hit <- !is.na(label)
  num_class <- length(unique(label[.hit]))
  if (length(try_nround)>1) {
    .series <- train_nround_series(data[.hit, , drop=FALSE],
                                   label[.hit], sample_label[.hit],
                                   num_class, try_nround=try_nround)
    .nround <- .series$nround[which.max(.series$score_holdout)]
    gc()
  }
  parts <- split_dataset(data[.hit, , drop=FALSE],
                         label[.hit], sample_label[.hit])
  xgboost(parts$train, label=parts$train_label,
          num_class=num_class, objective="multi:softprob",
          nround=.nround, verbose=0)
}


#' wrapper to train an xgboost model on data
#'
#' @param dataset_label character, a label that describes a veriant of the
#' mnist dataset
#' @param try_nround integer vector, number of xgboost nrounds
#'
#' @return vector with n_iter and performance on various data sets
#' (models are saved to disk as a side effect)
train_eval_mnist_model <- function(variant, variant_rep, try_nround=30) {
  dataset <- fread(glue(mnist_data_template,
                        VARIANT=variant, REP=variant_rep))
  model_file <- glue(mnist_model_template,
                     VARIANT=variant, REP=variant_rep)
  pixels <- as.matrix(dataset[, mnist_pixel_columns, with=FALSE])
  pixels_label <- dataset$label
  rm(dataset); gc();
  # train the model
  if (!file.exists(model_file)) {
    model <- train_xgboost(pixels, pixels_label, mnist_samples$dataset,
                           try_nround=try_nround)
    save(model, file=model_file)
  }
  load(model_file)
  # evaluate it on various subsets, as well as the original test data
  data <- split_dataset(pixels, pixels_label, mnist_samples$dataset)
  data$original_test <- mnist_original_parts$test
  data$original_test_label <- mnist_original_parts$test_label
  c(
    n_iter=model$niter,
    eval_xgboost(model, data, c("train", "holdout", "test", "original_test"),
                 label_names=seq(0, 9))
  )
}


if (!exists("mnist_nrounds")) {
  make_mnist_nrounds <- function() {
    result <- data.table(variant=c("original", "subset", "subset"),
                           rep=c(1, 1, 2))
    result$v <- result$variant
    result$r <- result$rep
    get_nrounds_series <- function(.SD) {
      v <- .SD$v
      r <- .SD$r
      dataset <- fread(glue(mnist_data_template, VARIANT=v, REP=r))
      pixels <- as.matrix(dataset[, mnist_pixel_columns, with=FALSE])
      pixels_label <- dataset$label
      train_nround_series(pixels, pixels_label, mnist_samples$dataset,
                          try_nround=seq(2, 50, by=2))
    }
    result <- result[, get_nrounds_series(.SD), by=c("variant", "rep")]
  }
  if (!file.exists(mnist_nrounds_file)) {
    mnist_nrounds <- make_mnist_nrounds()
    fwrite(mnist_nrounds, file=mnist_nrounds_file, sep="\t")
  }
  mnist_nrounds <- fread(mnist_nrounds_file)
}


if (!exists("mnist_performance")) {
  make_mnist_performance <- function() {
    variants <- c("original", "subset", "mislabel", "missing", "lost",
                  "top", "bottom", "left", "right",
                  "dropout", "noise", "erased", "spot", "ramp")
    result <- data.table(variant=rep(variants, each=2),
                         rep=rep(1:2, length(variants)))
    result <- result[variant!="original" | rep==1]
    for (i in seq_along(result$variant)) {
      .variant <- result$variant[i]
      .rep <- result$rep[i]
      print(paste0(date(), "   ", .variant, " ", .rep))
      .result <- train_eval_mnist_model(.variant, .rep)
      result[i, names(.result) := as.list(.result)]
      gc()
    }
    result
  }
  mnist_performance <- make_mnist_performance()
  fwrite(mnist_performance, file=mnist_performance_file, sep="\t")
}

