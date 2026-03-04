#' Paired Microbiome-Metabolome Demo Data
#'
#' A lightweight paired cross-omics demo derived from the
#' `SINHA_CRC_2016` study in the Borenstein curated microbiome-metabolome
#' collection. The object is a named list with a `microbiome_dataset`,
#' a `mass_dataset`, and a `sample_link` table sharing the same sample ids.
#'
#' The packaged subset keeps 40 samples, 60 genus-level microbiome features,
#' and 48 annotated metabolites so the tutorials and tests stay fast.
#'
#' @name demo_crossomics
#' @docType data
#' @keywords datasets
#' @examples
#' data("demo_crossomics", package = "microbiomedataset")
#'
#' names(demo_crossomics)
#' class(demo_crossomics$microbiome_data)
#' class(demo_crossomics$metabolome_data)
"demo_crossomics"
