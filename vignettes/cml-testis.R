# analysis of testis datasets

# load umap based on Sohni
if (!exists("testis_umap")) {
  testis_umap <- fread(templates$testis_umap)
}


# load performance on test data (uncalibrated, calibrated, recalibrated by sample)
if (!exists("testis_age")) {
  testis_age <- fread(templates$testis_age)
}

if (!exists("testis_performance")) {
  testis_performance <- fread(templates$testis_performance)
}

if (!exists("testis_data")) {
  testis_data_template <- file.path(dirs$result, "testis_data_{VARIANT}.tsv.gz")
  testis_data <- lapply(list(
    Sohni=glue(testis_data_template, VARIANT="Sohni"),
    Guo=glue(testis_data_template, VARIANT="Guo")
  ), fread)
}
