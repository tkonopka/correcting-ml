# prepare datasets from single-cell RNA-seq from testis

library(data.table)
library(glue)
library(umap)
source("prep_testis_common.R")


data_dir <- file.path("..", "data", "emtab")
results_dir <- file.path("..", "results")
markers_file <- file.path("..", "data", "Sohni-markers-genes.tsv")
emtab_codes <- list(Sohni="E-GEOD-124263", Guo="E-GEOD-134144")
emtab_mtx_template <-
  file.path(data_dir, "{EMTAB}.aggregated_filtered_normalised_counts.mtx")
emtab_design_template <- file.path(data_dir, "ExpDesign-{EMTAB}.tsv")

set.seed(654321)
umap_seed <- 654321


# load marker genes & gene sets
if (!exists("markers_tab")) {
  markers_tab <- fread(markers_file)
  markers_tab <- markers_tab[!is.na(gene_id) & gene_id != ""]
}


#' read a sparse expression matrix, subset to a small number of genes,
#' and create a data frame
#'
#' @param prefix character, path to mtx file
#' @param gene_ids character vector, gene_ids to subset to
#'
#' @return data table with cell_id and gene ids in columns
read_mtx <- function(prefix, gene_ids=NULL) {
  row_names <- fread(paste0(prefix, "_rows.gz"), header=FALSE)$V1
  col_names <- fread(paste0(prefix, "_cols.gz"), header=FALSE)$V1
  result <- fread(paste0(prefix, ".gz"), skip=2)
  setnames(result, c("V1", "V2", "V3"), c("gene_id", "cell_id", "value"))
  result$gene_id <- row_names[result$gene_id]
  if (!is.null(gene_ids)) {
    result <- result[gene_id %in% gene_ids]
  }
  result$cell_id <- col_names[result$cell_id]
  result <- dcast(cell_id~gene_id, data=result, value.var="value")
  result[is.na(result)] <- 0.0
  result
}


# load experimental design specification
if (!exists("emtab_design")) {
  emtab_design <- lapply(emtab_codes, function(x) {
    fread(glue(emtab_design_template, EMTAB=x))
  })
}


# load expression data
if (!exists("emtab_data")) {
  emtab_data <- lapply(emtab_codes, function(x) {
    read_mtx(glue(emtab_mtx_template, EMTAB=x), markers_tab$gene_id)
  })
}
gc()


if (!exists("emtab_signatures")) {
  emtab_signatures <- lapply(emtab_data, function(x) {
    xm <- as.matrix(x[, grep("ENSG", colnames(x)), with=FALSE])
    xm <- log2(1+xm)
    signatures <- split(markers_tab$gene_id, markers_tab$cell_type)
    result <- matrix(0, ncol=length(signatures), nrow=nrow(x),
                     dimnames=list(x$cell_id,
                                   paste0("sig_", names(signatures))))
    for (sig in names(signatures)) {
      sig_genes <- intersect(signatures[[sig]], colnames(xm))
      if (length(sig_genes)>0) {
        result[, paste0("sig_", sig)] <-
          apply(xm[, sig_genes, drop=FALSE], 1, mean)
      }
    }
    top_sig <- apply(result, 1, which.max)
    result <- data.table(result)
    result$cell_id <- x$cell_id
    result$top_sig <- names(signatures)[top_sig]
    setcolorder(result, c("cell_id", "top_sig"))
    result
  })
}


# create Sohni and Guo datasets
if (!exists("Sohni")) {
  temp <- copy(emtab_design$Sohni)
  setnames(temp, "Assay", "cell_id")
  setnames(temp, "Factor Value[age]", "age")
  temp <- merge(temp[, c("cell_id", "age")], emtab_signatures$Sohni,
                by="cell_id")
  Sohni <- merge(emtab_data$Sohni, temp, by="cell_id")
  Sohni$dataset <- sample(c("train", "holdout", "test"), nrow(Sohni),
                          prob=c(0.55, 0.15, 0.3), replace=TRUE)
  Sohni$label <- NA
  setcolorder(Sohni, c("cell_id", "label", "dataset", "top_sig"))
  fwrite(Sohni, file=glue(testis_data_template, VARIANT="Sohni"), sep="\t")
  rm(temp)
}
if (!exists("Guo")) {
  temp <- copy(emtab_design$Guo)
  setnames(temp, "Factor Value[inferred cell type - ontology labels]", "label")
  setnames(temp, "Assay", "cell_id")
  setnames(temp, "Factor Value[age]", "age")
  temp <- merge(temp[, c("cell_id", "label", "age")], emtab_signatures$Guo,
                by="cell_id")
  Guo <- merge(emtab_data$Guo, temp, by="cell_id")
  Guo$dataset <- sample(c("train", "holdout", "test"), nrow(Guo),
                        prob=c(0.55, 0.15, 0.3), replace=TRUE)
  setcolorder(Guo, c("cell_id", "label", "dataset"))
  fwrite(Guo, file=glue(testis_data_template, VARIANT="Guo"), sep="\t")
  rm(temp)
}


if (!exists("Sohni_umap")) {
  Sohni_matrix <- as.matrix(Sohni[, grep("ENSG", colnames(Sohni)), with=FALSE])
  rownames(Sohni_matrix) <- Sohni$cell_id
  Sohni_matrix <- log2(1+Sohni_matrix)
  Sohni_umap <- umap(Sohni_matrix, random_state=umap_seed)
  Sohni_layout <- data.table(Sohni_umap$layout)
  setnames(Sohni_layout, c("V1", "V2"), c("UMAP_1", "UMAP_2"))
  Sohni_layout$cell_id <- rownames(Sohni_umap$layout)
  Sohni_layout <- merge(Sohni_layout, Sohni[, c("cell_id", "top_sig")],
                        by="cell_id")
  setcolorder(Sohni_layout, c("cell_id", "UMAP_1", "UMAP_2"))
  fwrite(Sohni_layout, file=testis_path("umap_Sohni.tsv.gz"), sep="\t")
}
