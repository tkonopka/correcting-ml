# configuration variables


# ############################################################################
# libraries/packages

suppressMessages(library(data.table))
suppressMessages(library(Rcssplot))
suppressMessages(library(glue))
suppressMessages(library(shape))



# ############################################################################
# paths to directories

# assumes the script is executes from vignettes/
dirs <- list(
  R=file.path("..", "R"),
  data=file.path("..", "data"),
  results=file.path("..", "results")
)


# ############################################################################
# custom functions from R directory

.rfiles <- c("plot_general", "plot_mnist", "plot_label", "plot_schematic",
             "plot_performance", "plot_slide", "plot_bars", "plot_series",
             "plot_stacks", "plot_umap")
for (.rfile in .rfiles) {
  source(file.path(dirs$R, paste0(.rfile, ".R")))
}
rm(.rfile, .rfiles)




# ############################################################################
# constants, thesholds, paths, etc

# path to graphics styles
RcssDefaultStyle <- Rcss("cml.css")

# labels to designate plot panels
panel_labels <- letters


# templates for files
templates <- list(
  mnist_samples=file.path(dirs$result, "mnist_samples.tsv.gz"),
  mnist_data=file.path(dirs$result, "mnist_data_{VARIANT}_{REP}.tsv.gz"),
  mnist_nrounds=file.path(dirs$result, "mnist_nrounds.tsv.gz"),
  mnist_performance=file.path(dirs$result, "mnist_{TYPE}_performance.tsv.gz"),
  testis_umap=file.path(dirs$result, "testis_umap_Sohni.tsv.gz"),
  testis_performance=file.path(dirs$result, "testis_ensemble_performance.tsv.gz"),
  testis_age=file.path(dirs$result, "testis_ensemble_age_performance.tsv.gz")
)


