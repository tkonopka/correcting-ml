# download MNIST digit dataset

library(data.table)
library(curl)
library(utils)

# urls with raw datasets
url_mnist <- "http://yann.lecun.com/exdb/mnist/"
files_images <- c("train-images-idx3-ubyte.gz", "t10k-images-idx3-ubyte.gz")
files_labels <- c("train-labels-idx1-ubyte.gz", "t10k-labels-idx1-ubyte.gz")

# locations for local files
data_dir <- file.path("..", "data", "mnist")
mnist_file <- file.path(data_dir, "mnist_original.csv.gz")

# download all files
lapply(c(files_images, files_labels), function(x) {
  url <- paste0(url_mnist, x)
  local_path <- file.path(data_dir, x)
  if (!file.exists(local_path)) {
    curl_download(url, destfile=local_path)
  }
})


# arrange the raw data into a matrix format
if (!file.exists(mnist_file)) {
  result <- lapply(c("train", "t10k"), function(x) {
    labels_file <- file.path(data_dir, paste0(x, "-labels-idx1-ubyte.gz"))
    images_file <- file.path(data_dir, paste0(x, "-images-idx3-ubyte.gz"))
    # read labels and images
    con <- gzfile(labels_file, "rb")
    preamble <- readBin(con, "int", size=1, n=8, signed=FALSE)
    labels <- readBin(con, "int", size=1, n=100000, signed=FALSE)
    close(con)
    con <- gzfile(images_file, "rb")
    preamble <- readBin(con, "int", size=1, n=16, signed=FALSE)
    images <- readBin(con, "int", size=1, n=1e8, signed=FALSE)
    close(con)
    # put labels and the image data side-by-side
    cbind(labels, matrix(images, ncol=784, byrow=TRUE))
  })
  result <- rbind(result[[1]], result[[2]])
  fwrite(result, file=mnist_file, sep=",", row.names=FALSE, col.names=FALSE)
}
