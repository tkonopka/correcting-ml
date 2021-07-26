# some helper functions for used in testis scripts

# file paths and file path templates
testis_path <- function(x) {
  file.path("..", "results", paste0("testis_", x))
}

testis_data_template <- testis_path("data_{VARIANT}.tsv.gz")

