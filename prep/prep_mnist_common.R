# some helper functions for used in mnist scripts

# file paths and file path templates
mnist_path <- function(x) {
  file.path("..", "results", paste0("mnist_", x))
}

mnist_samples_file <- mnist_path("samples.tsv.gz")
mnist_data_template <- mnist_path("data_{VARIANT}_{REP}.tsv.gz")

# load mnist data, train/holdout/test splits
if (!exists("mnist_samples")) {
  mnist_samples <- fread(mnist_samples_file)
}
if (!exists("mnist_original")) {
  mnist_original_file <- glue(mnist_data_template, VARIANT="original", REP=1)
  mnist_original <- fread(mnist_original_file)
}
mnist_pixel_columns <- grep("pixel", colnames(mnist_original), value=TRUE)


#' partition a dataset into train/holdout/test subsets
#'
#' @param data matrix
#' @param label vector
#' @param sample_label character vector with "train", "holdout", "test" codes
#'
#' @return list with matrices and vectors, e.g. $train, $train_label, etc.
split_dataset <- function(data, label, sample_label) {
  result <- list()
  for (type in unique(sample_label)) {
    items <- which(sample_label==type & !is.na(label))
    result[[type]] <- data[items, , drop=FALSE]
    result[[paste0(type, "_label")]] <- label[items]
  }
  result
}


#' evaluate xgboost model on a series of datasets
#'
#' @param model xgboost model
#' @param data list with matrices and expected label vectors,
#' e.g. $train, $train_label, etc.
#' @param datasets character vector, e.g. "train"
#' @param label_names vector to be used to translate index of predicted
#' label into labels described in the data object
#'
#' @return numeric vector with performance on each dataset
eval_xgboost <- function(model, data, datasets, label_names) {
  result <- setNames(rep(0, length(datasets)), datasets)
  for (.dataset in datasets) {
    .data <- data[[.dataset]]
    .label <- data[[paste0(.dataset, "_label")]]
    pred <- predict(model, .data)
    if (!is(pred, "matrix")) {
      pred <- matrix(pred, byrow=TRUE, nrow=nrow(.data))
    }
    pred_label <- label_names[apply(pred, 1, which.max)]
    result[.dataset] <- mean(pred_label==.label)
  }
  result
}


#' evaluate xgboost class inbalance
#'
#' @param model object that can be used with predict
#' @param data list with components e.g. $test and $test_label
#' @param dataset character, e.g. "train"
#' @param label_names vector to be used to translate index of predicted
#' label into labels described in the data object
#'
#' @return data table with
eval_class_balance <- function(model, data, dataset, label_names=0:9) {
  result <- setNames(c(0), dataset)
  .data <- data[[dataset]]
  .label <- data[[paste0(dataset, "_label")]]
  pred <- predict(model, .data)
  if (!is(pred, "matrix")) {
    pred <- matrix(pred, byrow=TRUE, nrow=nrow(.data))
  }
  pred_label <- label_names[apply(pred, 1, which.max)]
  result[dataset] <- mean(pred_label==.label)
  for (i in seq_len(ncol(pred))) {
    result[paste0("proportion_", label_names[i])] <- mean(pred_label == label_names[i])
  }
  result
}


#' either read a table from file, or generated it using a function
#'
#' @param path character, path to a file on disk
#' @param fun function, used to compute a table
#' @param ... other arguments, passed to fun
#'
#' @return content of data file on disk; if it doesn't exist,
#' the file is computed and saved to the specified location
read_or_make <- function(path, fun, ...) {
  if (file.exists(path)) {
    return(fread(path))
  }
  result <- fun(...)
  fwrite(result, file=path, sep="\t")
  result
}


if (!exists("mnist_original_parts")) {
  mnist_original_parts <- split_dataset(
    as.matrix(mnist_original[, mnist_pixel_columns, with=FALSE]),
    mnist_original$label,
    mnist_samples$dataset
  )
  mnist_original_parts$original_test <- mnist_original_parts$test
  mnist_original_parts$original_test_label <- mnist_original_parts$test_label
  mnist_original_parts$small <- mnist_original_parts$holdout[1:5,]
  mnist_original_parts$small_label <- mnist_original_parts$holdout_label[1:5]
}

