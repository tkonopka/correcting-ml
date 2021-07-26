# prepare models for cell-type classification

library(data.table)
library(glue)
library(umap)
library(xgboost)
library(mlensemble)
source("prep_mnist_common.R")
source("prep_testis_common.R")


data_dir <- file.path("..", "data", "emtab")
results_dir <- file.path("..", "results")
markers_file <- file.path("..", "data", "Sohni-markers-genes.tsv")
emtab_codes <- list(Sohni="E-GEOD-124263", Guo="E-GEOD-134144")
emtab_design_template <- file.path(data_dir, "ExpDesign-{EMTAB}.tsv")
testis_model_template <- testis_path("model_{VARIANT}.Rda")
testis_ensemble_template <- testis_path("ensemble_{VARIANT}.Rda")
testis_predictions_file <- testis_path("ensemble_predictions.tsv.gz")
testis_performance_file <- testis_path("ensemble_performance.tsv.gz")
testis_age_file <- testis_path("ensemble_age_performance.tsv.gz")
testis_balance_file <- testis_path("ensemble_balance_performance.tsv.gz")
set.seed(654321)


# load gene and signature data
if (!exists("testis_raw")) {
  testis_raw <- list(
    Sohni=fread(glue(testis_data_template, VARIANT="Sohni")),
    Guo=fread(glue(testis_data_template, VARIANT="Guo"))
  )
}
# standardize the tables
if (!exists("testis_data")) {
  testis_data <- lapply(testis_raw, function(x) {
    sig_cols <- grep("^sig_", colnames(x), value=TRUE)
    gene_cols <- grep("ENSG", colnames(x), value=TRUE)
    select_cols <- c("cell_id", "label", "age", "dataset", sig_cols, gene_cols)
    result <- x[label!="", select_cols, with=FALSE]
    temp <- copy(result$label)
    result$label <- rep("other", nrow(result))
    result$label[grep("Leydig", temp)] <- "L_PTM"
    result$label[grep("Sertoli", temp)] <- "S"
    result$label[temp %in% c("LC", "PTM")] <- "L_PTM"
    result$label[temp %in% "SC"] <- "S"
    result$label[grep("ndothelial", temp)] <- "E"
    result
  })
}


#' create a multi-class classifier by normalizing signature values
#'
#' @param x matrix, data frame, or data.table
#'
#' @return matrix with columns corresponding to "sig_" column in x
classify_by_signature <- function(x, ...) {
  sig_names <- grep("^sig", colnames(x), value=TRUE)
  if (is(x, "data.table")) {
    xm <- x[, sig_names, with=FALSE]
  } else {
    xm <- x[, sig_names, drop=FALSE]
  }
  xm <- as.matrix(xm)
  result <- xm / apply(xm, 1, sum, na.rm=TRUE)
  colnames(result) <- gsub("sig_", "", colnames(result))
  result
}


#' postprocess a multiclass prediction matrix into three cell types, plus other
#'
#' @param x matrix with multiclass classification
#'
#' @return matrix with four columns
post_three_other <- function(x) {
  result <- matrix(0, ncol=4, nrow=nrow(x),
                   dimnames=list(rownames(x),
                                 c("other", "S", "L_PTM", "E")))
  result[, "S"] <- x[, "SC"]
  result[, "L_PTM"] <- x[, "LC"] + x[, "PTM"]
  result[, "E"] <- x[, "Endothelial"]
  result[, "other"] <- 1- apply(result, 1, sum)
  result[, c("other", "E", "L_PTM", "S")]
}


# extract columns with signature data
testis_sig_features <- grep("sig_", colnames(testis_data$Sohni), value=TRUE)
testis_sig_genes <- grep("ENSG", colnames(testis_data$Sohni), value=TRUE)
testis_three_other <- c("other", "E", "L_PTM", "S")

# create dataset based on Guo - split into training/holdout/test
if (!exists("Guo_data")) {
  Guo_data <- split_dataset(
    testis_data$Guo[, c(testis_sig_features, testis_sig_genes), with=FALSE],
    testis_data$Guo$label,
    testis_data$Guo$dataset)
  for (.dataset in unique(testis_data$Guo$dataset)) {
    Guo_data[[.dataset]] <- as.matrix(Guo_data[[.dataset]])
  }
  rm(.dataset)
}
if (!exists("Guo_data_age")) {
  temp <- testis_data$Guo
  temp$dataset <- gsub(" year", "", paste0(temp$dataset, "_age_", temp$age))
  Guo_data_age <- split_dataset(
    temp[, c(testis_sig_features, testis_sig_genes), with=FALSE],
    temp$label, temp$dataset
  )
  for (.dataset in unique(temp$dataset)) {
    Guo_data_age[[.dataset]] <- as.matrix(Guo_data_age[[.dataset]])
  }
  rm(.dataset, temp)
}


# create an xgboost model for the Guo dataset
if (!exists("model")) {
  model_file <- glue(testis_model_template, VARIANT="Guo")
  if (file.exists(model_file)) {
    load(model_file)
  } else {
    model <- xgboost(Guo_data$train[, testis_sig_genes],
                     label=match(Guo_data$train_label, testis_three_other)-1,
                     num_class=length(testis_three_other),
                     objective="multi:softprob",
                     nround=10,
                     verbose=1)
    save(model, file=model_file)
  }
}


# create ensembles
create_calibrate_testis_ensemble <- function(variant=c("fun", "xgb", "funxgb")) {
  ensemble_file <- glue(testis_ensemble_template, VARIANT=variant)
  if (file.exists(ensemble_file)) {
    load(ensemble_file)
    return(ensemble)
  }
  testis_post_hook <- ml_hook(post_three_other, type="post")
  testis_hooks <- ml_hooks(list(testis_post_hook))
  m_fun <- ml_model(classify_by_signature,
                    name="signatures_fun",
                    feature_names=testis_sig_features,
                    label_names=gsub("sig_", "", testis_sig_features),
                    hooks=testis_hooks)
  m_xgb <- ml_model(model, name="expression_xgb",
                    feature_names=testis_sig_genes,
                    label_names=testis_three_other)
  if (variant=="fun") {
    ensemble <- ml_ensemble() + m_fun
  } else if (variant=="xgb") {
    ensemble <- ml_ensemble() + m_xgb
  } else {
    ensemble <- ml_ensemble() + m_xgb + m_fun
  }
  ensemble <- calibrate(ensemble,
                        as.matrix(Guo_data$holdout),
                        Guo_data$holdout_label)
  save(ensemble, file=ensemble_file)
  ensemble
}
testis_ensembles <- list(
  fun=create_calibrate_testis_ensemble("fun"),
  xgb=create_calibrate_testis_ensemble("xgb"),
  funxgb=create_calibrate_testis_ensemble("funxgb")
)


# get a matrix of all predictions
if (!exists("testis_predictions")) {
  make_testis_predictions <- function() {
    result <- testis_data$Guo[, c("cell_id", "dataset", "label", "age")]
    .columns <- c(testis_sig_features, testis_sig_genes)
    m <- as.matrix(testis_data$Guo[, .columns, with=FALSE])
    for (v in names(testis_ensembles)) {
      .ensemble <- testis_ensembles[[v]]
      .predictions <- predict(.ensemble, m)
      result[[paste0(v, "_calibrated")]] <-
        colnames(.predictions)[apply(.predictions, 1, which.max)]
      .ensemble$calibration <- NA
      .predictions <- predict(.ensemble, m)
      result[[paste0(v, "_uncalibrated")]] <-
        colnames(.predictions)[apply(.predictions, 1, which.max)]
    }
    result
  }
  testis_predictions <- read_or_make(testis_predictions_file,
                                     make_testis_predictions)
}


# re-evaluate predictions and measure performance (hit rate)
if (!exists("testis_performance")) {
  make_testis_performance <- function() {
    result <- data.table(expand.grid(list(variant=names(testis_ensembles),
                                          rep=c("uncalibrated", "calibrated"),
                                          train=0, holdout=0, test=0)))
    tht <- c("train", "holdout", "test")
    for (v in unique(result$variant)) {
      .parts <- Guo_data
      .ensemble <- testis_ensembles[[v]]
      # evaluate calibrated ensemble
      .perf <- eval_xgboost(.ensemble, .parts, datasets=tht,
                            label_names=testis_three_other)
      result[variant==v & rep=="calibrated", names(.perf) := as.list(.perf)]
      # evaluate uncalibrated ensemble
      .ensemble$calibration <- NA
      .perf <- eval_xgboost(.ensemble, .parts, datasets=tht,
                            label_names=testis_three_other)
      result[variant==v & rep=="uncalibrated", names(.perf) := as.list(.perf)]
    }
    result
  }
  testis_performance <- read_or_make(testis_performance_file,
                                     make_testis_performance)
}


# re-evaluate predictions, age-specific recalibration
if (!exists("testis_age")) {
  make_testis_age <- function() {
    test_datasets <- grep("test", names(Guo_data_age), value=TRUE)
    test_datasets <- unique(gsub("_label", "", test_datasets))
    holdout_datasets <- gsub("test_", "holdout_", test_datasets)
    paired_datasets <- data.table(holdout=holdout_datasets, test=test_datasets)
    result <- data.table(expand.grid(list(variant=names(testis_ensembles),
                                          rep=c("uncalibrated", "calibrated",
                                                "recalibrated"))))
    for (.dataset in test_datasets) {
      result[[.dataset]] <- 0
    }
    for (v in unique(result$variant)) {
      .parts <- Guo_data_age
      .ensemble <- testis_ensembles[[v]]
      # evaluate calibrated ensemble
      .perf <- eval_xgboost(.ensemble, .parts, datasets=test_datasets,
                                  label_names=testis_three_other)
      result[variant==v & rep=="calibrated", names(.perf) := as.list(.perf)]
      .ensemble$calibration <- NA
      .perf <- eval_xgboost(.ensemble, .parts, datasets=test_datasets,
                            label_names=testis_three_other)
      result[variant==v & rep=="uncalibrated", names(.perf) := as.list(.perf)]
      for (i in seq_len(nrow(paired_datasets))) {
        .holdout <- paired_datasets$holdout[i]
        .test <- paired_datasets$test[i]
        .ensemble2 <- calibrate(.ensemble, .parts[[.holdout]],
                                .parts[[paste0(.holdout, "_label")]])
        .perf2 <- eval_xgboost(.ensemble2, .parts, datasets=.test,
                               label_names=testis_three_other)
        result[variant==v & rep=="recalibrated", names(.perf2) := as.list(.perf2)]
      }
    }
    result
  }
  testis_age <- read_or_make(testis_age_file, make_testis_age)
}


# re-evaluate predictions and measure class balance
if (!exists("testis_balance")) {
  make_testis_balance <- function() {
    result <- data.table(expand.grid(list(variant=names(testis_ensembles),
                                          rep=c("uncalibrated", "calibrated"),
                                          test=0)))
    for (v in unique(result$variant)) {
      .parts <- Guo_data
      .ensemble <- testis_ensembles[[v]]
      # evaluate calibrated ensemble
      .perf <- eval_class_balance(.ensemble, .parts, dataset="test",
                                  label_names=testis_three_other)
      result[variant==v & rep=="calibrated", names(.perf) := as.list(.perf)]
      # evaluate uncalibrated ensemble
      .ensemble$calibration <- NA
      .perf <- eval_class_balance(.ensemble, .parts, dataset="test",
                                  label_names=testis_three_other)
      result[variant==v & rep=="uncalibrated", names(.perf) := as.list(.perf)]
    }
    result
  }
  testis_balance <- read_or_make(testis_balance_file, make_testis_balance)
}

