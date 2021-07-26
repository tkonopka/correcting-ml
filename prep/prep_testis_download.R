# download datasets from Single-cell Expression Atlas

library(data.table)
library(glue)
library(curl)
library(utils)
library(R.utils)

# urls for downloading
design_url_template <- paste0("https://www.ebi.ac.uk/gxa/sc/experiment/",
                              "{EMTAB}/download?fileType=experiment-design")
counts_url_template <- paste0("https://www.ebi.ac.uk/gxa/sc/experiment/",
                              "{EMTAB}/download/zip?fileType=normalised")

# local paths
data_dir <- file.path("..", "data", "emtab")
emtab_mtx_template <-
  file.path(data_dir, "{EMTAB}.aggregated_filtered_normalised_counts.mtx.gz")
emtab_zip_template <- file.path(data_dir, "{EMTAB}-normalised-files.zip")
emtab_design_template <- file.path(data_dir, "ExpDesign-{EMTAB}.tsv")

# datasets to download
emtab_codes <- list(Sohni="E-GEOD-124263", Guo="E-GEOD-134144")


# download experiment-design files
lapply(emtab_codes, function(x) {
  url <- glue(design_url_template, EMTAB=x)
  local_path <- glue(emtab_design_template, EMTAB=x)
  if (!file.exists(local_path)) {
    curl_download(url, destfile=local_path)
  }
})

# download normalized counts files
lapply(emtab_codes, function(x) {
  url <- glue(counts_url_template, EMTAB=x)
  local_path <- glue(emtab_zip_template, EMTAB=x)
  if (!file.exists(local_path)) {
    curl_download(url, destfile=local_path)
  }
})

# unzip archives, then gzip individual files
lapply(emtab_codes, function(x) {
  zip_path <- glue(emtab_zip_template, EMTAB=x)
  mtx_path <- glue(emtab_mtx_template, EMTAB=x)
  if (!file.exists(mtx_path)) {
    unzip(zip_path, exdir=dirname(zip_path))
    gzip(gsub(".gz", "", mtx_path))
    gzip(gsub(".gz", "_rows", mtx_path))
    gzip(gsub(".gz", "_cols", mtx_path))
  }
})
