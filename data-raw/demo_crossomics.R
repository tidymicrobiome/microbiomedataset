source_urls <- c(
  metadata = paste0(
    "https://raw.githubusercontent.com/borenstein-lab/",
    "microbiome-metabolome-curated-data/main/",
    "data/processed_data/SINHA_CRC_2016/metadata.tsv"
  ),
  genera = paste0(
    "https://raw.githubusercontent.com/borenstein-lab/",
    "microbiome-metabolome-curated-data/main/",
    "data/processed_data/SINHA_CRC_2016/genera.tsv"
  ),
  metabolome = paste0(
    "https://raw.githubusercontent.com/borenstein-lab/",
    "microbiome-metabolome-curated-data/main/",
    "data/processed_data/SINHA_CRC_2016/mtb.tsv"
  ),
  metabolite_map = paste0(
    "https://raw.githubusercontent.com/borenstein-lab/",
    "microbiome-metabolome-curated-data/main/",
    "data/processed_data/SINHA_CRC_2016/mtb.map.tsv"
  )
)


download_if_missing <- function(url, path) {
  if (!file.exists(path)) {
    utils::download.file(url = url, destfile = path, mode = "wb", quiet = TRUE)
  }
  path
}


root_dir <- normalizePath(".", winslash = "/", mustWork = TRUE)
source_dir <- file.path(root_dir, "data-raw", "sources", "SINHA_CRC_2016")
dir.create(source_dir, recursive = TRUE, showWarnings = FALSE)

invisible(download_if_missing(
  source_urls[["metadata"]],
  file.path(source_dir, "metadata.tsv")
))
invisible(download_if_missing(
  source_urls[["genera"]],
  file.path(source_dir, "genera.tsv")
))
invisible(download_if_missing(
  source_urls[["metabolome"]],
  file.path(source_dir, "mtb.tsv")
))
invisible(download_if_missing(
  source_urls[["metabolite_map"]],
  file.path(source_dir, "mtb.map.tsv")
))

library(devtools)
devtools::load_all(root_dir, quiet = TRUE)
library(massdataset)

metadata <- read.delim(
  file.path(source_dir, "metadata.tsv"),
  check.names = FALSE,
  stringsAsFactors = FALSE
)
genera <- read.delim(
  file.path(source_dir, "genera.tsv"),
  check.names = FALSE,
  stringsAsFactors = FALSE
)
metabolome <- read.delim(
  file.path(source_dir, "mtb.tsv"),
  check.names = FALSE,
  stringsAsFactors = FALSE
)
metabolite_map <- read.delim(
  file.path(source_dir, "mtb.map.tsv"),
  check.names = FALSE,
  stringsAsFactors = FALSE,
  na.strings = c("", "NA")
)

metadata$Study.Group <- as.character(metadata$Study.Group)
selected_samples <- metadata |>
  split(metadata$Study.Group) |>
  lapply(function(x) x[order(x$Sample), , drop = FALSE]) |>
  lapply(utils::head, 20) |>
  do.call(what = rbind)
selected_sample_ids <- selected_samples$Sample

genera_selected <- genera[genera$Sample %in% selected_sample_ids, , drop = FALSE]
genera_selected <- genera_selected[match(selected_sample_ids, genera_selected$Sample), , drop = FALSE]
taxonomy_strings <- setdiff(colnames(genera_selected), "Sample")
taxonomy_table <- parse_borenstein_taxonomy(taxonomy_strings)
taxa_keep <- !is.na(taxonomy_table$Genus) & taxonomy_strings != "Unclassified"
taxonomy_table <- taxonomy_table[taxa_keep, , drop = FALSE]
taxonomy_strings <- taxonomy_strings[taxa_keep]

genera_matrix <- t(as.matrix(genera_selected[, taxonomy_strings, drop = FALSE]))
storage.mode(genera_matrix) <- "numeric"
colnames(genera_matrix) <- selected_sample_ids
taxa_order <- order(rowMeans(genera_matrix), decreasing = TRUE)
taxa_keep_ids <- taxonomy_strings[taxa_order][seq_len(min(60, length(taxa_order)))]
genera_matrix <- genera_matrix[taxa_keep_ids, , drop = FALSE]
taxonomy_table <- taxonomy_table[taxa_keep_ids, , drop = FALSE]

microbiome_variable_info <- data.frame(
  variable_id = taxa_keep_ids,
  lineage = taxa_keep_ids,
  taxonomy_table,
  source_study = "SINHA_CRC_2016",
  stringsAsFactors = FALSE,
  check.names = FALSE
)
rownames(microbiome_variable_info) <- microbiome_variable_info$variable_id

sample_info <- selected_samples[, c(
  "Sample",
  "Subject",
  "Study.Group",
  "Age",
  "Age.Units",
  "Gender",
  "BMI",
  "race",
  "hosp1",
  "hosp2"
)]
colnames(sample_info) <- c(
  "sample_id",
  "subject_id",
  "study_group",
  "age",
  "age_units",
  "gender",
  "bmi",
  "race",
  "hospital_1",
  "hospital_2"
)
sample_info$study_group_label <- paste0("group_", sample_info$study_group)
sample_info$source_study <- "SINHA_CRC_2016"
sample_info$class <- "subject"

microbiome_data <- create_microbiome_dataset(
  expression_data = as.data.frame(genera_matrix, check.names = FALSE),
  sample_info = sample_info,
  variable_info = microbiome_variable_info
)

metabolome_selected <- metabolome[metabolome$Sample %in% selected_sample_ids, , drop = FALSE]
metabolome_selected <- metabolome_selected[match(selected_sample_ids, metabolome_selected$Sample), , drop = FALSE]

metabolite_map$Compound <- as.character(metabolite_map$Compound)
metabolite_candidates <- metabolite_map[
  metabolite_map$High.Confidence.Annotation %in% TRUE &
    !grepl("^X_", metabolite_map$Compound) &
    !is.na(metabolite_map$HMDB),
  ,
  drop = FALSE
]

metabolite_matrix <- t(as.matrix(metabolome_selected[, metabolite_candidates$Compound, drop = FALSE]))
storage.mode(metabolite_matrix) <- "numeric"
colnames(metabolite_matrix) <- selected_sample_ids
metabolite_sd <- apply(metabolite_matrix, 1, stats::sd)
metabolite_sd[is.na(metabolite_sd)] <- 0
metabolite_keep <- names(sort(metabolite_sd, decreasing = TRUE))[seq_len(min(48, length(metabolite_sd)))]
metabolite_matrix <- metabolite_matrix[metabolite_keep, , drop = FALSE]

metabolome_variable_info <- data.frame(
  variable_id = metabolite_keep,
  source_study = "SINHA_CRC_2016",
  stringsAsFactors = FALSE
)

metabolome_data <- massdataset::create_mass_dataset(
  expression_data = as.data.frame(metabolite_matrix, check.names = FALSE),
  sample_info = sample_info,
  variable_info = metabolome_variable_info
)

metabolite_annotation <- metabolite_map[
  match(metabolite_keep, metabolite_map$Compound),
  ,
  drop = FALSE
]
metabolite_annotation <- data.frame(
  variable_id = metabolite_annotation$Compound,
  compound_name = metabolite_annotation$Compound,
  hmdb_id = metabolite_annotation$HMDB,
  kegg_id = metabolite_annotation$KEGG,
  annotation_level = ifelse(
    metabolite_annotation$High.Confidence.Annotation,
    "high_confidence",
    "low_confidence"
  ),
  source_study = "SINHA_CRC_2016",
  stringsAsFactors = FALSE
)
metabolome_data@annotation_table <- standardize_metabolite_annotation(
  metabolite_annotation,
  variable_info = metabolome_data@variable_info
)

sample_link <- data.frame(
  microbiome_sample_id = sample_info$sample_id,
  metabolome_sample_id = sample_info$sample_id,
  pair_id = sample_info$sample_id,
  stringsAsFactors = FALSE
)

demo_crossomics <- list(
  microbiome_data = microbiome_data,
  metabolome_data = metabolome_data,
  sample_link = sample_link,
  selection = list(
    study_id = "SINHA_CRC_2016",
    sample_n = nrow(sample_info),
    taxon_n = nrow(microbiome_data@expression_data),
    metabolite_n = nrow(metabolome_data@expression_data),
    sample_rule = paste(
      "First 20 samples from each Study.Group after sorting by sample id."
    ),
    taxon_rule = "Top genera by mean relative abundance among selected samples.",
    metabolite_rule = paste(
      "Top high-confidence annotated metabolites by standard deviation,",
      "excluding X_-prefixed unknowns."
    )
  ),
  source = list(
    dataset = "SINHA_CRC_2016",
    repository = "borenstein-lab/microbiome-metabolome-curated-data"
  )
)

save(
  demo_crossomics,
  file = file.path(root_dir, "data", "demo_crossomics.rda"),
  compress = "xz"
)
