#' Calculate Total Abundance per Sample
#'
#' @param object A `microbiome_dataset` object.
#' @param na.rm Should missing values be removed before summation?
#'
#' @return A numeric vector named by `sample_id`.
#' @export
#' @examples
#' data("global_patterns", package = "microbiomedataset")
#'
#' x <- sample_sums(global_patterns)
#' head(x)
sample_sums <-
  function(object,
           na.rm = TRUE) {
    x <- extract_expression_data(object)
    stats::setNames(
      apply(x, 2, sum, na.rm = na.rm),
      colnames(x)
    )
  }


#' Calculate Total Abundance per Taxon
#'
#' @param object A `microbiome_dataset` object.
#' @param na.rm Should missing values be removed before summation?
#'
#' @return A numeric vector named by `variable_id`.
#' @export
#' @examples
#' data("global_patterns", package = "microbiomedataset")
#'
#' x <- taxa_sums(global_patterns)
#' head(x)
taxa_sums <-
  function(object,
           na.rm = TRUE) {
    x <- extract_expression_data(object)
    stats::setNames(
      apply(x, 1, sum, na.rm = na.rm),
      rownames(x)
    )
  }


#' Calculate Taxon Prevalence across Samples
#'
#' @param object A `microbiome_dataset` object.
#' @param detection Minimum abundance required to count as present.
#' @param proportion Return prevalence as a proportion instead of counts?
#'
#' @return A numeric vector named by `variable_id`.
#' @export
#' @examples
#' data("global_patterns", package = "microbiomedataset")
#'
#' x <- taxa_prevalence(global_patterns, detection = 0)
#' head(x)
taxa_prevalence <-
  function(object,
           detection = 0,
           proportion = FALSE) {
    x <- extract_expression_data(object)
    prevalence <- apply(x, 1, function(y) {
      sum(y > detection, na.rm = TRUE)
    })
    if (proportion) {
      prevalence <- prevalence / ncol(x)
    }
    stats::setNames(prevalence, rownames(x))
  }


#' Filter Taxa by Abundance or Prevalence
#'
#' @param object A `microbiome_dataset` object.
#' @param min_prevalence Minimum prevalence required to keep a taxon.
#' @param min_prevalence_prop Minimum prevalence proportion required to keep a taxon.
#' @param min_abundance Minimum total abundance required to keep a taxon.
#' @param detection Detection threshold used for prevalence.
#'
#' @return A `microbiome_dataset` object.
#' @export
#' @examples
#' data("global_patterns", package = "microbiomedataset")
#'
#' x <- filter_taxa(
#'   global_patterns,
#'   min_prevalence = 5,
#'   min_abundance = 100
#' )
#' dim(x@expression_data)
filter_taxa <-
  function(object,
           min_prevalence = NULL,
           min_prevalence_prop = NULL,
           min_abundance = NULL,
           detection = 0) {
    keep <- rep(TRUE, nrow(object@expression_data))
    names(keep) <- rownames(object@expression_data)
    
    if (!is.null(min_prevalence)) {
      keep <- keep & taxa_prevalence(object, detection = detection) >= min_prevalence
    }
    
    if (!is.null(min_prevalence_prop)) {
      keep <- keep & taxa_prevalence(object, detection = detection, proportion = TRUE) >= min_prevalence_prop
    }
    
    if (!is.null(min_abundance)) {
      keep <- keep & taxa_sums(object) >= min_abundance
    }
    
    object <- prune_taxa(
      object = object,
      variable_id = names(keep)[keep]
    )
    
    append_process_parameter(
      object = object,
      process_name = "filter_taxa",
      function_name = "filter_taxa()",
      parameter = list(
        min_prevalence = min_prevalence,
        min_prevalence_prop = min_prevalence_prop,
        min_abundance = min_abundance,
        detection = detection
      )
    )
  }


#' Filter Taxa by a Taxonomic Rank
#'
#' Keep or remove taxa using values in a taxonomy column from `variable_info`.
#'
#' @param object A `microbiome_dataset` object.
#' @param taxonomic_rank A taxonomy rank in `variable_info`.
#' @param taxa Character vector of taxa labels used for filtering.
#' @param keep Should the provided taxa be kept? Defaults to `TRUE`.
#' @param na_keep Should taxa with missing rank be kept? Defaults to `FALSE`.
#'
#' @return A `microbiome_dataset` object.
#' @export
#' @examples
#' data("global_patterns", package = "microbiomedataset")
#'
#' x <- filter_taxa_by_rank(
#'   global_patterns,
#'   taxonomic_rank = "Phylum",
#'   taxa = c("Bacteroidetes", "Firmicutes")
#' )
#' dim(x@expression_data)
filter_taxa_by_rank <-
  function(object,
           taxonomic_rank = c("Kingdom",
                              "Phylum",
                              "Class",
                              "Order",
                              "Family",
                              "Genus",
                              "Species"),
           taxa,
           keep = TRUE,
           na_keep = FALSE) {
    taxonomic_rank <- match.arg(taxonomic_rank)
    
    if (missing(taxa)) {
      stop("taxa must be provided.")
    }
    
    variable_info <- extract_variable_info(object)
    rank_values <- as.character(variable_info[[taxonomic_rank]])
    target_taxa <- as.character(taxa)
    keep_index <- rank_values %in% target_taxa
    
    if (na_keep) {
      keep_index[is.na(rank_values)] <- TRUE
    }
    
    object <- prune_taxa(
      object = object,
      variable_id = variable_info$variable_id[keep_index],
      keep = keep
    )
    
    append_process_parameter(
      object = object,
      process_name = "filter_taxa_by_rank",
      function_name = "filter_taxa_by_rank()",
      parameter = list(
        taxonomic_rank = taxonomic_rank,
        taxa = target_taxa,
        keep = keep,
        na_keep = na_keep
      )
    )
  }


#' Filter Samples by Library Size
#'
#' @param object A `microbiome_dataset` object.
#' @param min_abundance Minimum total abundance required to keep a sample.
#' @param max_abundance Maximum total abundance allowed to keep a sample.
#'
#' @return A `microbiome_dataset` object.
#' @export
#' @examples
#' data("global_patterns", package = "microbiomedataset")
#'
#' x <- filter_samples(global_patterns, min_abundance = 1000)
#' dim(x@expression_data)
filter_samples <-
  function(object,
           min_abundance = NULL,
           max_abundance = NULL) {
    keep <- rep(TRUE, ncol(object@expression_data))
    names(keep) <- colnames(object@expression_data)
    totals <- sample_sums(object)
    
    if (!is.null(min_abundance)) {
      keep <- keep & totals >= min_abundance
    }
    
    if (!is.null(max_abundance)) {
      keep <- keep & totals <= max_abundance
    }
    
    object <- prune_samples(
      object = object,
      sample_id = names(keep)[keep]
    )
    
    append_process_parameter(
      object = object,
      process_name = "filter_samples",
      function_name = "filter_samples()",
      parameter = list(
        min_abundance = min_abundance,
        max_abundance = max_abundance
      )
    )
  }


#' Filter Samples by a Sample Metadata Group
#'
#' Keep or remove samples using values from a column in `sample_info`.
#'
#' @param object A `microbiome_dataset` object.
#' @param group_by A column in `sample_info`.
#' @param groups Character vector of group labels used for filtering.
#' @param keep Should the provided groups be kept? Defaults to `TRUE`.
#' @param na_keep Should samples with missing group values be kept? Defaults to
#'   `FALSE`.
#'
#' @return A `microbiome_dataset` object.
#' @export
#' @examples
#' data("global_patterns", package = "microbiomedataset")
#'
#' x <- filter_samples_by_group(
#'   global_patterns,
#'   group_by = "SampleType",
#'   groups = c("Soil", "Ocean")
#' )
#' dim(x@expression_data)
filter_samples_by_group <-
  function(object,
           group_by,
           groups,
           keep = TRUE,
           na_keep = FALSE) {
    if (missing(group_by)) {
      stop("group_by must be provided.")
    }
    
    if (missing(groups)) {
      stop("groups must be provided.")
    }
    
    sample_info <- extract_sample_info(object)
    
    if (!group_by %in% colnames(sample_info)) {
      stop(group_by, " must be in sample_info.")
    }
    
    group_values <- as.character(sample_info[[group_by]])
    target_groups <- as.character(groups)
    keep_index <- group_values %in% target_groups
    
    if (na_keep) {
      keep_index[is.na(group_values)] <- TRUE
    }
    
    object <- prune_samples(
      object = object,
      sample_id = sample_info$sample_id[keep_index],
      keep = keep
    )
    
    append_process_parameter(
      object = object,
      process_name = "filter_samples_by_group",
      function_name = "filter_samples_by_group()",
      parameter = list(
        group_by = group_by,
        groups = target_groups,
        keep = keep,
        na_keep = na_keep
      )
    )
  }
