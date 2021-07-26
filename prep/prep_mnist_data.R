# prepare mnist datasets

library(data.table)
library(glue)

data_dir <- file.path("..", "data", "mnist")
results_dir <- file.path("..", "results")
mnist_raw_file <- file.path("..", "data", "mnist", "mnist_original.csv.gz")
mnist <- fread(mnist_raw_file, sep=",", header=FALSE)
mnist_labels <- mnist$V1

set.seed(654321)


# create a split into training/holdout/test datasets
# BUT - ensure that the first 10 items are in training data)
mnist_samples_file <- file.path(results_dir, "mnist_samples.tsv.gz")
if (!file.exists(mnist_samples_file)) {
  mnist_split <- data.table(index=seq_len(nrow(mnist)),
                            label=mnist$V1,
                            dataset="train")
  test_items <- sample(seq(11, nrow(mnist)),
                       20000, replace=FALSE)
  holdout_items <- sample(setdiff(seq(11, nrow(mnist)), test_items),
                          10000, replace=FALSE)
  mnist_split$dataset[test_items] <- "test"
  mnist_split$dataset[holdout_items] <- "holdout"
  fwrite(mnist_split, file=mnist_samples_file, sep="\t")
}


# datasets with perturbations

pixels <- as.matrix(mnist[, 2:ncol(mnist)])
colnames(pixels) <- paste0("pixel_", seq_len(ncol(pixels)))


#' perturb mnist pixels and save dataset
#'
#' @param file_path character, path to output dataset
#' @param complement_path character, path to a complement dataset
#' @param perturb_fun function that creates a perturbation on mnist pixels
#' @param ... other arguments passed to perturb_fun
#'
#' @return NULL
perturb_pixels_save <- function(file_path, perturb_fun, ...) {
  if (!file.exists(file_path)) {
    result <- data.table(perturb_fun(pixels, ...))
    result$index <- seq_len(nrow(pixels))
    result$label <- mnist_labels
    setcolorder(result, c("index", "label", colnames(pixels)))
    fwrite(result, file=file_path, sep="\t")
  }
  invisible(NULL)
}


#' perturb mnist labels and save dataset
#'
#' @param file_path character, path to output dataset
#' @param what character, code for what type of perturbation to perform
#' @param complement logical, set TRUE to get a complementary perturbation
#' @param seed integer, seed for random number generation
#'
#' @return NULL
perturb_labels_save <- function(file_path, what=c("mislabel", "subset", "ramp"),
                                complement=FALSE, seed=NA) {
  what <- match.arg(what)
  set.seed(seed)
  if (!file.exists(file_path)) {
    result <- data.table(pixels)
    result$index <- seq_len(nrow(pixels))
    result$label <- mnist_labels
    n <- length(mnist_labels)
    if (what=="ramp") {
      targets <- sample(seq_along(mnist_labels), n/2,
                        prob=result$label+1, replace=FALSE)
    } else {
      targets <- sample(seq_along(mnist_labels), n/2, replace=FALSE)
    }
    if (complement) {
      targets <- setdiff(seq_along(mnist_labels), targets)
    }
    if (what=="mislabel") {
      result$label[targets] <- sample(mnist_labels, n/2, replace=FALSE)
    } else {
      result$label[targets] <- NA
    }
    setcolorder(result, c("index", "label", colnames(pixels)))
    fwrite(result, file=file_path, sep="\t")
  }
  set.seed(seed)
  invisible(NULL)
}


#' mask certain features in/out in a matrix
#'
#' @param x matrix, each row encoding a square image, columns named
#' pixel_1, pixel_2, etc.
#' @param mask character, code of which features to mask;
#' a mask="top" means that the top part of the image is preserved
#' @param complement logical, set TRUE to get a complement dataset
#'
#' @return matrix of the same dimension as x, but with values in
#' certain columns set to NA
mask_pixels <- function(x, mask=c("top", "bottom", "left", "right", "none"),
                        complement=FALSE) {
  mask <- match.arg(mask)
  square <- matrix(paste0("pixel_", seq_len(ncol(x))),
                   nrow=sqrt(ncol(x)), byrow=TRUE)
  d <- ncol(square)
  if (complement) {
    mask_complement <- c(top="bottom", bottom="top", left="right",
                        right="left", none="none")
    mask <- mask_complement[mask]
  }
  if (mask=="top") {
    features <- as.character(tail(square, d/2))
  } else if (mask=="bottom") {
    features <- as.character(head(square, d/2))
  } else if (mask=="left") {
    features <- as.character(tail(t(square), d/2))
  } else if (mask=="right") {
    features <- as.character(head(t(square), d/2))
  } else {
    features <- c()
  }
  result <- copy(x)
  if (length(features)>0) {
    result[, features] <- NA
  }
  result
}


#' get a matrix of distances between pixels
#'
#' @param n total number of pixels, will assume creates a square image
#'
#' @return matrix of distances
get_pixel_distances <- function(n) {
  nsq <- ceiling(sqrt(n))
  coords <- cbind(x=floor((-1+seq_len(n)) / nsq),
                  y=(-1+seq_len(n)) %% nsq)
  as.matrix(dist(coords))
}


#' reset a superpixel in a square image
#'
#' @param x integer vector for a square image
#' @param value integer, use 0 to set pixel values to zero,
#' or set 1 to use values from the image
#'
#' @return integer vector of same size as x
reset_circle <- function(x, value=0, pixel_distances=NULL) {
  x_nonzero <- x[x>0]
  nz <- length(x_nonzero)
  r <- ceiling(sqrt(nz/pi))
  # pick a point in the image, identify indexes that are within a radius
  center_index <- sample(seq_along(x), 1, prob=x)
  target_indexes <- which(pixel_distances[, center_index]< r)
  # create a new vector
  result <- copy(x)
  if (is.na(value) | value==0) {
    result[target_indexes] <- value
  } else if (!is.na(value) & nz>0) {
    result[target_indexes] <- sample(x_nonzero, length(target_indexes),
                                     replace=TRUE)
  }
  result
}


#' reset some pixels in a matrix to 0
#'
#' @param x matrix, each row encoding a square image, column named as
#' pixel_1, pixel_2, etc.
#' @param reset character, type of reset transformation
#' @param value integer, use 0 to reset to zero, or 1 to reset to new
#' values that originate from that pixels distribution
#' @param proportion numeric, proportion of pixels to target
#'
#' @return matrix of same size as x, but with some pixels set to 0
reset_pixels <- function(x, reset=c("random", "circle"), value=0,
                         proportion=0.5) {
  reset <- match.arg(reset)
  result <- copy(x)
  indexes <- seq_len(nrow(x))
  n <- length(indexes)
  if (reset=="random") {
    for (column in colnames(x)) {
      targets <- sample(indexes, round(proportion*n), replace=FALSE)
      if (is.na(value) | value==0) {
        result[targets, column] <- value
      } else if (!is.na(value) & sum(x[, column])>0) {
        target_values <- x[, column]
        target_values <- target_values[target_values>0]
        result[targets, column] <- sample(target_values, length(targets),
                                          replace=TRUE)
      }
    }
  } else if (reset=="circle") {
    pixel_distances <- get_pixel_distances(ncol(x))
    for (i in seq_len(nrow(x))) {
      result[i, ] <- reset_circle(x[i, ], value=value,
                                  pixel_distances=pixel_distances)
    }
  }
  result
}


#' introduce background noise to a set proportion of pixels
#'
#' @param x matrix, each row encoding a square image, column named as
#' pixel_1, pixel_2, etc.
#' @param value numeric, proportion of pixels to change to a random value
#'
#' @return matrix of same size as x, but a proportion of pixels are noise
bg_pixels <- function(x, value=0) {
  result <- copy(x)
  n_targets <- round(value*ncol(x))
  if (n_targets==0) return(result)
  col_indexes <- seq_len(ncol(x))
  for (i in seq_len(nrow(x))) {
    i_noise <- round(runif(n_targets, 0, 255))
    i_targets <- sample(col_indexes, n_targets, replace=FALSE)
    result[i, i_targets] <- pmin(255, result[i, i_targets] + i_noise)
  }
  result
}


# create datasets with top/bottom/left/right parts masked out
mnist_data_template <-
  file.path(results_dir, "mnist_data_{VARIANT}_{REP}.tsv.gz")
for (mask in c("bottom", "top", "left", "right")) {
  perturb_pixels_save(glue(mnist_data_template, VARIANT=mask, REP=1),
                      mask_pixels, mask=mask, complement=FALSE)
  perturb_pixels_save(glue(mnist_data_template, VARIANT=mask, REP=2),
                      mask_pixels, mask=mask, complement=TRUE)
}
perturb_pixels_save(glue(mnist_data_template, VARIANT="original", REP=1),
                    mask_pixels, mask="none")


# create datasets with some pixels reset or missing
perturb_settings <- rbindlist(list(
  list("random", 0, "dropout"),
  list("random", 1, "noise"),
  list("random", NA, "missing"),
  list("circle", 0, "erased"),
  list("circle", 1, "spot"),
  list("circle", NA, "lost")
))
colnames(perturb_settings) <- c("reset", "value", "variant")
for (i in seq_len(nrow(perturb_settings))) {
  variant <- perturb_settings$variant[i]
  # for reset_pixels, just generate two versions
  perturb_pixels_save(glue(mnist_data_template, VARIANT=variant, REP=1),
                      reset_pixels,
                      reset=perturb_settings$reset[i],
                      value=perturb_settings$value[i])
  perturb_pixels_save(glue(mnist_data_template, VARIANT=variant, REP=2),
                      reset_pixels,
                      reset=perturb_settings$reset[i],
                      value=perturb_settings$value[i])
}

# create dataset with some labels switched
for (variant in c("mislabel", "subset", "ramp")) {
  variant_seed <- round(runif(1, 1, 1e7))
  perturb_labels_save(glue(mnist_data_template, VARIANT=variant, REP=1),
                      what=variant, complement=FALSE, seed=variant_seed)
  perturb_labels_save(glue(mnist_data_template, VARIANT=variant, REP=2),
                      what=variant, complement=TRUE, seed=variant_seed)
}


# create datasets with a series of background noise levels
for (bg in seq(0, 100, by=10)) {
  perturb_pixels_save(glue(mnist_data_template, VARIANT="bg", REP=bg),
                      bg_pixels, value=bg/100)
}
# create datasets with a series of missingness levels
for (miss in seq(0, 80, by=10)) {
  perturb_pixels_save(glue(mnist_data_template, VARIANT="na", REP=miss),
                      reset_pixels, reset="random", value=NA,
                      proportion=miss/100)
}
