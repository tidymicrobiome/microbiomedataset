#' Create a Joint Microbiome-Metabolome Dataset
#'
#' Build a paired cross-omics object from `microbiome_dataset` and
#' `mass_dataset`, keeping an explicit sample link table between omics.
#'
#' @param microbiome_data A `microbiome_dataset` object.
#' @param metabolome_data A `mass_dataset` object.
#' @param sample_link Optional data.frame with columns `microbiome_sample_id`
#'   and `metabolome_sample_id`. If missing, shared sample ids are inferred.
#' @param feature_link Optional taxon-metabolite link table.
#' @param pathway_link Optional pathway/reaction link table.
#' @param align_samples Whether to prune both omics to the linked samples.
#'
#' @return A `microbiome_metabolome_dataset` object.
#' @export
#' @examples
#' data("global_patterns", package = "microbiomedataset")
#'
#' metabolome_expression <- as.data.frame(matrix(
#'   seq_len(6 * ncol(global_patterns@expression_data)),
#'   nrow = 6,
#'   ncol = ncol(global_patterns@expression_data)
#' ))
#' rownames(metabolome_expression) <- paste0("M", seq_len(nrow(metabolome_expression)))
#' colnames(metabolome_expression) <- global_patterns@sample_info$sample_id
#'
#' metabolome_data <- massdataset::create_mass_dataset(
#'   expression_data = metabolome_expression,
#'   sample_info = global_patterns@sample_info,
#'   variable_info = data.frame(variable_id = rownames(metabolome_expression))
#' )
#'
#' x <- create_microbiome_metabolome_dataset(
#'   microbiome_data = global_patterns,
#'   metabolome_data = metabolome_data
#' )
#' x
create_microbiome_metabolome_dataset <- function(microbiome_data,
                                                 metabolome_data,
                                                 sample_link = NULL,
                                                 feature_link = NULL,
                                                 pathway_link = NULL,
                                                 align_samples = TRUE) {
  if (!methods::is(microbiome_data, "microbiome_dataset")) {
    stop("microbiome_data must be a microbiome_dataset.")
  }
  if (!methods::is(metabolome_data, "mass_dataset")) {
    stop("metabolome_data must be a mass_dataset.")
  }
  
  if (is.null(sample_link)) {
    sample_link <- build_crossomics_sample_link(microbiome_data, metabolome_data)
  } else {
    sample_link <- normalize_crossomics_sample_link(sample_link)
  }
  
  if (nrow(sample_link) == 0) {
    stop("No shared samples were found between microbiome_data and metabolome_data.")
  }
  
  if (methods::.hasSlot(metabolome_data, "annotation_table") &&
      !is.null(metabolome_data@annotation_table)) {
    metabolome_data@annotation_table <- standardize_metabolite_annotation(
      metabolome_data@annotation_table,
      variable_info = metabolome_data@variable_info
    )
  }
  
  feature_link <- normalize_crossomics_feature_link(feature_link)
  pathway_link <- normalize_crossomics_pathway_link(pathway_link)
  
  check_result <- check_crossomics_dataset(
    microbiome_data = microbiome_data,
    metabolome_data = metabolome_data,
    sample_link = sample_link,
    feature_link = feature_link,
    pathway_link = pathway_link
  )
  if (!identical(check_result, "all good.")) {
    stop(check_result)
  }
  
  if (isTRUE(align_samples)) {
    aligned <- align_crossomics_samples(
      microbiome_data = microbiome_data,
      metabolome_data = metabolome_data,
      sample_link = sample_link
    )
    microbiome_data <- aligned$microbiome_data
    metabolome_data <- aligned$metabolome_data
    sample_link <- aligned$sample_link
  }
  
  parameter <- new(
    Class = "tidymass_parameter",
    pacakge_name = "microbiomedataset",
    function_name = "create_microbiome_metabolome_dataset()",
    parameter = list(aligned_samples = isTRUE(align_samples)),
    time = Sys.time()
  )
  
  new(
    Class = "microbiome_metabolome_dataset",
    microbiome_data = microbiome_data,
    metabolome_data = metabolome_data,
    sample_link = sample_link,
    feature_link = feature_link,
    pathway_link = pathway_link,
    process_info = list(create_microbiome_metabolome_dataset = parameter),
    version = as.character(utils::packageVersion("microbiomedataset"))
  )
}


#' Check a Joint Microbiome-Metabolome Dataset
#'
#' Validate cross-omics sample links and optional feature/pathway link tables.
#'
#' @param microbiome_data A `microbiome_dataset` object.
#' @param metabolome_data A `mass_dataset` object.
#' @param sample_link A sample link table.
#' @param feature_link Optional feature link table.
#' @param pathway_link Optional pathway link table.
#'
#' @return `"all good."` when the inputs are valid, otherwise an error string.
#' @export
#' @examples
#' data("global_patterns", package = "microbiomedataset")
#'
#' metabolome_expression <- as.data.frame(matrix(
#'   seq_len(4 * ncol(global_patterns@expression_data)),
#'   nrow = 4,
#'   ncol = ncol(global_patterns@expression_data)
#' ))
#' rownames(metabolome_expression) <- paste0("M", seq_len(nrow(metabolome_expression)))
#' colnames(metabolome_expression) <- global_patterns@sample_info$sample_id
#'
#' metabolome_data <- massdataset::create_mass_dataset(
#'   expression_data = metabolome_expression,
#'   sample_info = global_patterns@sample_info,
#'   variable_info = data.frame(variable_id = rownames(metabolome_expression))
#' )
#' sample_link <- data.frame(
#'   microbiome_sample_id = global_patterns@sample_info$sample_id,
#'   metabolome_sample_id = global_patterns@sample_info$sample_id
#' )
#'
#' check_crossomics_dataset(
#'   microbiome_data = global_patterns,
#'   metabolome_data = metabolome_data,
#'   sample_link = sample_link
#' )
check_crossomics_dataset <- function(microbiome_data,
                                     metabolome_data,
                                     sample_link,
                                     feature_link = NULL,
                                     pathway_link = NULL) {
  if (!methods::is(microbiome_data, "microbiome_dataset")) {
    return("error: microbiome_data must be a microbiome_dataset.")
  }
  if (!methods::is(metabolome_data, "mass_dataset")) {
    return("error: metabolome_data must be a mass_dataset.")
  }
  
  sample_link <- normalize_crossomics_sample_link(sample_link)
  microbiome_samples <- as.character(microbiome_data@sample_info$sample_id)
  metabolome_samples <- as.character(metabolome_data@sample_info$sample_id)
  
  if (anyDuplicated(sample_link$microbiome_sample_id) > 0) {
    return("error: sample_link contains duplicated microbiome_sample_id values.")
  }
  if (anyDuplicated(sample_link$metabolome_sample_id) > 0) {
    return("error: sample_link contains duplicated metabolome_sample_id values.")
  }
  if (!all(sample_link$microbiome_sample_id %in% microbiome_samples)) {
    return("error: sample_link contains microbiome_sample_id values absent from microbiome_data.")
  }
  if (!all(sample_link$metabolome_sample_id %in% metabolome_samples)) {
    return("error: sample_link contains metabolome_sample_id values absent from metabolome_data.")
  }
  
  if (!is.null(feature_link)) {
    feature_link <- normalize_crossomics_feature_link(feature_link)
    microbiome_variables <- as.character(microbiome_data@variable_info$variable_id)
    metabolome_variables <- as.character(metabolome_data@variable_info$variable_id)
    if (!all(feature_link$taxon_id %in% microbiome_variables)) {
      return("error: feature_link contains taxon_id values absent from microbiome_data.")
    }
    if (!all(feature_link$metabolite_id %in% metabolome_variables)) {
      return("error: feature_link contains metabolite_id values absent from metabolome_data.")
    }
  }
  
  if (!is.null(pathway_link)) {
    pathway_link <- normalize_crossomics_pathway_link(pathway_link)
    if (nrow(pathway_link) == 0) {
      return("error: pathway_link must contain at least one row when provided.")
    }
  }
  
  "all good."
}


#' Align Samples Between Microbiome and Metabolome Data
#'
#' Prune both omics objects to the linked sample pairs.
#'
#' @param object A `microbiome_metabolome_dataset` object.
#' @param microbiome_data A `microbiome_dataset` object. Used when `object` is
#'   not supplied.
#' @param metabolome_data A `mass_dataset` object. Used when `object` is not
#'   supplied.
#' @param sample_link Optional sample link table used when `object` is not
#'   supplied.
#'
#' @return A `microbiome_metabolome_dataset` object with aligned samples.
#' @export
#' @examples
#' data("global_patterns", package = "microbiomedataset")
#'
#' metabolome_expression <- as.data.frame(matrix(
#'   seq_len(4 * ncol(global_patterns@expression_data)),
#'   nrow = 4,
#'   ncol = ncol(global_patterns@expression_data)
#' ))
#' rownames(metabolome_expression) <- paste0("M", seq_len(nrow(metabolome_expression)))
#' colnames(metabolome_expression) <- global_patterns@sample_info$sample_id
#'
#' metabolome_data <- massdataset::create_mass_dataset(
#'   expression_data = metabolome_expression,
#'   sample_info = global_patterns@sample_info,
#'   variable_info = data.frame(variable_id = rownames(metabolome_expression))
#' )
#'
#' x <- create_microbiome_metabolome_dataset(global_patterns, metabolome_data, align_samples = FALSE)
#' x <- align_samples_between_omics(x)
#' nrow(x@sample_link)
align_samples_between_omics <- function(object = NULL,
                                        microbiome_data = NULL,
                                        metabolome_data = NULL,
                                        sample_link = NULL) {
  resolved <- resolve_crossomics_input(
    object = object,
    microbiome_data = microbiome_data,
    metabolome_data = metabolome_data,
    sample_link = sample_link,
    align_samples = FALSE
  )
  
  aligned <- align_crossomics_samples(
    microbiome_data = resolved$microbiome_data,
    metabolome_data = resolved$metabolome_data,
    sample_link = resolved$sample_link
  )
  
  if (is.null(object)) {
    return(aligned)
  }
  
  object@microbiome_data <- aligned$microbiome_data
  object@metabolome_data <- aligned$metabolome_data
  object@sample_link <- aligned$sample_link
  
  append_crossomics_process_parameter(
    object = object,
    process_name = "align_samples_between_omics",
    function_name = "align_samples_between_omics()",
    parameter = list(n_linked_samples = nrow(object@sample_link))
  )
}


#' Extract Matched Cross-Omics Matrices
#'
#' Build matched microbiome and metabolome matrices using the explicit sample
#' link table stored in a `microbiome_metabolome_dataset`.
#'
#' @param object A `microbiome_metabolome_dataset` object.
#' @param microbiome_data A `microbiome_dataset` object. Used when `object` is
#'   not supplied.
#' @param metabolome_data A `mass_dataset` object. Used when `object` is not
#'   supplied.
#' @param sample_link Optional sample link table used when `object` is not
#'   supplied.
#' @param microbiome_rank Optional taxonomy rank used to aggregate microbiome
#'   abundance before extraction.
#' @param microbiome_transform Optional transformation method passed to
#'   [transform_counts()].
#' @param metabolome_transform Metabolome transformation method.
#'
#' @return A list containing matched matrices and metadata.
#' @export
#' @examples
#' data("demo_crossomics", package = "microbiomedataset")
#'
#' x <- create_microbiome_metabolome_dataset(
#'   microbiome_data = demo_crossomics$microbiome_data,
#'   metabolome_data = demo_crossomics$metabolome_data,
#'   sample_link = demo_crossomics$sample_link
#' )
#' y <- extract_crossomics_matrix(x, microbiome_rank = "Genus")
#' names(y)
extract_crossomics_matrix <- function(object = NULL,
                                      microbiome_data = NULL,
                                      metabolome_data = NULL,
                                      sample_link = NULL,
                                      microbiome_rank = NULL,
                                      microbiome_transform = c("none", "relative", "log", "presence_absence", "clr"),
                                      metabolome_transform = c("none", "log")) {
  resolved <- resolve_crossomics_input(
    object = object,
    microbiome_data = microbiome_data,
    metabolome_data = metabolome_data,
    sample_link = sample_link
  )
  
  microbiome_transform <- match.arg(microbiome_transform)
  metabolome_transform <- match.arg(metabolome_transform)
  
  microbiome_object <- resolved$microbiome_data
  if (!is.null(microbiome_rank)) {
    microbiome_object <- summarise_taxa(microbiome_object, taxonomic_rank = microbiome_rank)
  }
  if (!identical(microbiome_transform, "none")) {
    microbiome_object <- transform_counts(microbiome_object, method = microbiome_transform)
  }
  
  microbiome_matrix <- as.matrix(massdataset::extract_expression_data(microbiome_object))
  metabolome_matrix <- as.matrix(resolved$metabolome_data@expression_data)
  
  sample_link <- resolved$sample_link
  microbiome_matrix <- microbiome_matrix[, sample_link$microbiome_sample_id, drop = FALSE]
  metabolome_matrix <- metabolome_matrix[, sample_link$metabolome_sample_id, drop = FALSE]
  colnames(microbiome_matrix) <- sample_link$pair_id
  colnames(metabolome_matrix) <- sample_link$pair_id
  
  if (identical(metabolome_transform, "log")) {
    metabolome_matrix <- log10(metabolome_matrix + 1)
  }
  
  microbiome_sample_info <- massdataset::extract_sample_info(microbiome_object)
  microbiome_sample_info <- microbiome_sample_info[
    match(sample_link$microbiome_sample_id, microbiome_sample_info$sample_id),
    ,
    drop = FALSE
  ]
  metabolome_sample_info <- resolved$metabolome_data@sample_info
  metabolome_sample_info <- metabolome_sample_info[
    match(sample_link$metabolome_sample_id, metabolome_sample_info$sample_id),
    ,
    drop = FALSE
  ]
  
  sample_info <- sample_link %>%
    dplyr::left_join(
      suffix_metadata_columns(microbiome_sample_info, "sample_id", "_microbiome"),
      by = c("microbiome_sample_id" = "sample_id")
    ) %>%
    dplyr::left_join(
      suffix_metadata_columns(metabolome_sample_info, "sample_id", "_metabolome"),
      by = c("metabolome_sample_id" = "sample_id")
    )
  
  metabolome_annotation_table <- standardize_metabolite_annotation(
    resolved$metabolome_data@annotation_table,
    variable_info = resolved$metabolome_data@variable_info
  )
  
  list(
    microbiome_matrix = microbiome_matrix,
    metabolome_matrix = metabolome_matrix,
    sample_info = sample_info,
    microbiome_variable_info = microbiome_object@variable_info,
    metabolome_variable_info = resolved$metabolome_data@variable_info,
    metabolome_annotation_table = metabolome_annotation_table,
    sample_link = sample_link
  )
}


#' Melt Matched Cross-Omics Features
#'
#' Export matched microbiome and metabolome data as tidy long tables.
#'
#' @param object A `microbiome_metabolome_dataset` object.
#' @param microbiome_data A `microbiome_dataset` object. Used when `object` is
#'   not supplied.
#' @param metabolome_data A `mass_dataset` object. Used when `object` is not
#'   supplied.
#' @param sample_link Optional sample link table used when `object` is not
#'   supplied.
#' @param microbiome_rank Optional taxonomy rank used to aggregate microbiome
#'   abundance before melting.
#'
#' @return A list with `microbiome` and `metabolome` tibbles.
#' @export
#' @examples
#' data("demo_crossomics", package = "microbiomedataset")
#'
#' x <- create_microbiome_metabolome_dataset(
#'   microbiome_data = demo_crossomics$microbiome_data,
#'   metabolome_data = demo_crossomics$metabolome_data,
#'   sample_link = demo_crossomics$sample_link
#' )
#' y <- melt_crossomics_features(x, microbiome_rank = "Genus")
#' names(y)
melt_crossomics_features <- function(object = NULL,
                                     microbiome_data = NULL,
                                     metabolome_data = NULL,
                                     sample_link = NULL,
                                     microbiome_rank = NULL) {
  extracted <- extract_crossomics_matrix(
    object = object,
    microbiome_data = microbiome_data,
    metabolome_data = metabolome_data,
    sample_link = sample_link,
    microbiome_rank = microbiome_rank
  )
  
  microbiome_long <- as.data.frame(extracted$microbiome_matrix, check.names = FALSE) %>%
    tibble::rownames_to_column(var = "taxon_id") %>%
    tidyr::pivot_longer(cols = -taxon_id, names_to = "pair_id", values_to = "abundance") %>%
    dplyr::left_join(extracted$sample_info, by = "pair_id") %>%
    dplyr::left_join(
      extracted$microbiome_variable_info %>%
        as.data.frame(check.names = FALSE),
      by = c("taxon_id" = "variable_id")
    )
  
  metabolome_long <- as.data.frame(extracted$metabolome_matrix, check.names = FALSE) %>%
    tibble::rownames_to_column(var = "metabolite_id") %>%
    tidyr::pivot_longer(cols = -metabolite_id, names_to = "pair_id", values_to = "abundance") %>%
    dplyr::left_join(extracted$sample_info, by = "pair_id") %>%
    dplyr::left_join(
      extracted$metabolome_variable_info %>%
        as.data.frame(check.names = FALSE),
      by = c("metabolite_id" = "variable_id")
    )
  
  if (!is.null(extracted$metabolome_annotation_table)) {
    metabolome_long <- metabolome_long %>%
      dplyr::left_join(
        as.data.frame(extracted$metabolome_annotation_table, check.names = FALSE),
        by = c("metabolite_id" = "variable_id")
      )
  }
  
  list(
    microbiome = microbiome_long,
    metabolome = metabolome_long
  )
}


#' Prepare Matched Data for Correlation Analysis
#'
#' Build filtered microbiome and metabolome matrices suitable for correlation
#' analysis.
#'
#' @param object A `microbiome_metabolome_dataset` object.
#' @param microbiome_data A `microbiome_dataset` object. Used when `object` is
#'   not supplied.
#' @param metabolome_data A `mass_dataset` object. Used when `object` is not
#'   supplied.
#' @param sample_link Optional sample link table used when `object` is not
#'   supplied.
#' @param microbiome_rank Optional taxonomy rank used to aggregate microbiome
#'   abundance before extraction.
#' @param microbiome_transform Microbiome transformation method.
#' @param metabolome_transform Metabolome transformation method.
#' @param min_taxa_prevalence Minimum number of non-zero samples required for a
#'   microbiome feature.
#' @param min_taxa_abundance Minimum mean abundance required for a microbiome
#'   feature.
#' @param min_metabolite_prevalence Minimum number of non-zero samples required
#'   for a metabolite.
#' @param min_metabolite_sd Minimum standard deviation required for a
#'   metabolite.
#'
#' @return A list containing filtered matrices and metadata.
#' @export
#' @examples
#' data("demo_crossomics", package = "microbiomedataset")
#'
#' x <- prepare_correlation_data(
#'   microbiome_data = demo_crossomics$microbiome_data,
#'   metabolome_data = demo_crossomics$metabolome_data,
#'   sample_link = demo_crossomics$sample_link,
#'   microbiome_rank = "Genus"
#' )
#' dim(x$microbiome_matrix)
prepare_correlation_data <- function(object = NULL,
                                     microbiome_data = NULL,
                                     metabolome_data = NULL,
                                     sample_link = NULL,
                                     microbiome_rank = "Genus",
                                     microbiome_transform = c("relative", "log", "presence_absence", "clr", "none"),
                                     metabolome_transform = c("log", "none"),
                                     min_taxa_prevalence = 0,
                                     min_taxa_abundance = 0,
                                     min_metabolite_prevalence = 0,
                                     min_metabolite_sd = 0) {
  microbiome_transform <- match.arg(microbiome_transform)
  metabolome_transform <- match.arg(metabolome_transform)
  extracted <- extract_crossomics_matrix(
    object = object,
    microbiome_data = microbiome_data,
    metabolome_data = metabolome_data,
    sample_link = sample_link,
    microbiome_rank = microbiome_rank,
    microbiome_transform = microbiome_transform,
    metabolome_transform = metabolome_transform
  )
  
  extracted$microbiome_matrix <- filter_crossomics_matrix_rows(
    extracted$microbiome_matrix,
    min_prevalence = min_taxa_prevalence,
    min_mean = min_taxa_abundance,
    min_sd = 0
  )
  extracted$metabolome_matrix <- filter_crossomics_matrix_rows(
    extracted$metabolome_matrix,
    min_prevalence = min_metabolite_prevalence,
    min_mean = -Inf,
    min_sd = min_metabolite_sd
  )
  
  extracted$microbiome_variable_info <- extracted$microbiome_variable_info[
    match(rownames(extracted$microbiome_matrix), extracted$microbiome_variable_info$variable_id),
    ,
    drop = FALSE
  ]
  extracted$metabolome_variable_info <- extracted$metabolome_variable_info[
    match(rownames(extracted$metabolome_matrix), extracted$metabolome_variable_info$variable_id),
    ,
    drop = FALSE
  ]
  if (!is.null(extracted$metabolome_annotation_table)) {
    extracted$metabolome_annotation_table <- extracted$metabolome_annotation_table[
      match(rownames(extracted$metabolome_matrix), extracted$metabolome_annotation_table$variable_id),
      ,
      drop = FALSE
    ]
  }
  
  extracted
}


#' Calculate Microbe-Metabolite Correlations
#'
#' Compute pairwise correlations between microbiome taxa and metabolites on
#' matched samples.
#'
#' @param object A `microbiome_metabolome_dataset` object.
#' @param microbiome_data A `microbiome_dataset` object. Used when `object` is
#'   not supplied.
#' @param metabolome_data A `mass_dataset` object. Used when `object` is not
#'   supplied.
#' @param sample_link Optional sample link table used when `object` is not
#'   supplied.
#' @param microbiome_rank Taxonomy rank for microbiome aggregation.
#' @param microbiome_transform Microbiome transformation method.
#' @param metabolome_transform Metabolome transformation method.
#' @param method Correlation method.
#' @param ... Additional arguments passed through the convenience wrappers.
#' @param p_adjust_method Multiple-testing correction method.
#' @param min_taxa_prevalence Minimum number of non-zero samples required for a
#'   microbiome feature.
#' @param min_taxa_abundance Minimum mean abundance required for a microbiome
#'   feature.
#' @param min_metabolite_prevalence Minimum number of non-zero samples required
#'   for a metabolite.
#' @param min_metabolite_sd Minimum standard deviation required for a
#'   metabolite.
#'
#' @return A `microbe_metabolite_association` object.
#' @export
#' @examples
#' data("demo_crossomics", package = "microbiomedataset")
#'
#' x <- calculate_correlation(
#'   microbiome_data = demo_crossomics$microbiome_data,
#'   metabolome_data = demo_crossomics$metabolome_data,
#'   sample_link = demo_crossomics$sample_link,
#'   microbiome_rank = "Genus",
#'   metabolome_transform = "none",
#'   method = "spearman"
#' )
#' class(x)
calculate_correlation <- function(object = NULL,
                                  microbiome_data = NULL,
                                  metabolome_data = NULL,
                                  sample_link = NULL,
                                  microbiome_rank = "Genus",
                                  microbiome_transform = c("relative", "log", "presence_absence", "clr", "none"),
                                  metabolome_transform = c("log", "none"),
                                  method = c("spearman", "pearson", "sparcc"),
                                  p_adjust_method = "BH",
                                  min_taxa_prevalence = 0,
                                  min_taxa_abundance = 0,
                                  min_metabolite_prevalence = 0,
                                  min_metabolite_sd = 0) {
  microbiome_transform <- match.arg(microbiome_transform)
  metabolome_transform <- match.arg(metabolome_transform)
  method <- match.arg(method)
  
  extracted <- prepare_correlation_data(
    object = object,
    microbiome_data = microbiome_data,
    metabolome_data = metabolome_data,
    sample_link = sample_link,
    microbiome_rank = microbiome_rank,
    microbiome_transform = microbiome_transform,
    metabolome_transform = metabolome_transform,
    min_taxa_prevalence = min_taxa_prevalence,
    min_taxa_abundance = min_taxa_abundance,
    min_metabolite_prevalence = min_metabolite_prevalence,
    min_metabolite_sd = min_metabolite_sd
  )
  
  result <- calculate_correlation_table(
    microbiome_matrix = extracted$microbiome_matrix,
    metabolome_matrix = extracted$metabolome_matrix,
    method = method,
    p_adjust_method = p_adjust_method
  )
  result$taxonomic_rank <- microbiome_rank
  result$method <- method
  
  microbiome_variable_info <- extracted$microbiome_variable_info %>%
    as.data.frame(check.names = FALSE)
  metabolome_variable_info <- extracted$metabolome_variable_info %>%
    as.data.frame(check.names = FALSE)
  
  result <- result %>%
    dplyr::left_join(
      suffix_metadata_columns(microbiome_variable_info, "variable_id", "_microbiome"),
      by = c("taxon_id" = "variable_id")
    ) %>%
    dplyr::left_join(
      suffix_metadata_columns(metabolome_variable_info, "variable_id", "_metabolome"),
      by = c("metabolite_id" = "variable_id")
    )
  
  if (!is.null(extracted$metabolome_annotation_table)) {
    result <- result %>%
      dplyr::left_join(
        dplyr::rename_with(
          as.data.frame(extracted$metabolome_annotation_table, check.names = FALSE),
          ~ paste0(.x, "_annotation"),
          -variable_id
        ),
        by = c("metabolite_id" = "variable_id")
      )
  }
  
  new(
    Class = "microbe_metabolite_association",
    result = result,
    method = method,
    microbiome_rank = microbiome_rank,
    metabolome_transform = metabolome_transform
  )
}


#' @rdname calculate_correlation
#' @export
calculate_correlation_spearman <- function(...) {
  calculate_correlation(..., method = "spearman")
}


#' @rdname calculate_correlation
#' @export
calculate_correlation_pearson <- function(...) {
  calculate_correlation(..., method = "pearson")
}


#' @rdname calculate_correlation
#' @export
calculate_correlation_sparcc <- function(...) {
  calculate_correlation(..., method = "sparcc")
}


#' Associate Microbiome Taxa with Metabolites
#'
#' Compute pairwise microbe-metabolite associations on matched samples.
#'
#' @param object A `microbiome_metabolome_dataset` object.
#' @param microbiome_data A `microbiome_dataset` object. Used when `object` is
#'   not supplied.
#' @param metabolome_data A `mass_dataset` object. Used when `object` is not
#'   supplied.
#' @param sample_link Optional sample link table used when `object` is not
#'   supplied.
#' @param microbiome_rank Taxonomy rank for microbiome aggregation.
#' @param microbiome_transform Microbiome transformation method.
#' @param metabolome_transform Metabolome transformation method.
#' @param method Correlation method.
#' @param p_adjust_method Multiple-testing correction method.
#'
#' @return A `microbe_metabolite_association` object.
#' @export
#' @examples
#' data("demo_crossomics", package = "microbiomedataset")
#'
#' x <- create_microbiome_metabolome_dataset(
#'   microbiome_data = demo_crossomics$microbiome_data,
#'   metabolome_data = demo_crossomics$metabolome_data,
#'   sample_link = demo_crossomics$sample_link
#' )
#' y <- associate_microbe_metabolite(x, microbiome_rank = "Genus")
#' class(y)
associate_microbe_metabolite <- function(object = NULL,
                                         microbiome_data = NULL,
                                         metabolome_data = NULL,
                                         sample_link = NULL,
                                         microbiome_rank = "Genus",
                                         microbiome_transform = c("relative", "log", "presence_absence", "clr", "none"),
                                         metabolome_transform = c("log", "none"),
                                         method = c("spearman", "pearson", "sparcc"),
                                         p_adjust_method = "BH") {
  calculate_correlation(
    object = object,
    microbiome_data = microbiome_data,
    metabolome_data = metabolome_data,
    sample_link = sample_link,
    microbiome_rank = microbiome_rank,
    microbiome_transform = microbiome_transform,
    metabolome_transform = metabolome_transform,
    method = method,
    p_adjust_method = p_adjust_method
  )
}


#' Extract a Tidy Correlation Table
#'
#' Return the tidy result table from a correlation result object.
#'
#' @param object A `microbe_metabolite_association` object.
#'
#' @return A data.frame.
#' @export
#' @examples
#' data("demo_crossomics", package = "microbiomedataset")
#'
#' x <- calculate_correlation(
#'   microbiome_data = demo_crossomics$microbiome_data,
#'   metabolome_data = demo_crossomics$metabolome_data,
#'   sample_link = demo_crossomics$sample_link,
#'   metabolome_transform = "none"
#' )
#' head(extract_correlation_table(x))
extract_correlation_table <- function(object) {
  if (!methods::is(object, "microbe_metabolite_association")) {
    stop("object must be a microbe_metabolite_association.")
  }
  object@result
}


#' Filter Correlation Results
#'
#' Keep only correlation edges that pass user-defined score and significance
#' thresholds.
#'
#' @param object A `microbe_metabolite_association` object.
#' @param min_abs_correlation Minimum absolute correlation estimate.
#' @param max_q_value Maximum q value.
#' @param direction Direction of correlation to keep.
#'
#' @return A `microbe_metabolite_association` object.
#' @export
#' @examples
#' data("demo_crossomics", package = "microbiomedataset")
#'
#' x <- calculate_correlation(
#'   microbiome_data = demo_crossomics$microbiome_data,
#'   metabolome_data = demo_crossomics$metabolome_data,
#'   sample_link = demo_crossomics$sample_link,
#'   metabolome_transform = "none"
#' )
#' y <- filter_correlation(x, min_abs_correlation = 0.3, max_q_value = 0.2)
#' y
filter_correlation <- function(object,
                               min_abs_correlation = 0,
                               max_q_value = 1,
                               direction = c("any", "positive", "negative")) {
  if (!methods::is(object, "microbe_metabolite_association")) {
    stop("object must be a microbe_metabolite_association.")
  }
  direction <- match.arg(direction)
  result <- object@result %>%
    dplyr::filter(abs(.data$estimate) >= min_abs_correlation) %>%
    dplyr::filter(is.na(.data$q_value) | .data$q_value <= max_q_value)
  
  if (!identical(direction, "any")) {
    result <- result %>% dplyr::filter(.data$direction == direction)
  }
  
  object@result <- result
  object
}


#' Build a Microbe-Metabolite Correlation Network
#'
#' Convert correlation results into a bipartite network object.
#'
#' @param object A `microbe_metabolite_association` object.
#' @param min_abs_correlation Minimum absolute correlation estimate.
#' @param max_q_value Maximum q value.
#' @param direction Direction of correlation to keep.
#' @param top_n Maximum number of edges to keep.
#' @param keep_isolated Whether isolated nodes should be kept.
#'
#' @return A `microbe_metabolite_network` object.
#' @export
#' @examples
#' data("demo_crossomics", package = "microbiomedataset")
#'
#' x <- calculate_correlation(
#'   microbiome_data = demo_crossomics$microbiome_data,
#'   metabolome_data = demo_crossomics$metabolome_data,
#'   sample_link = demo_crossomics$sample_link,
#'   metabolome_transform = "none"
#' )
#' y <- build_correlation_network(x, min_abs_correlation = 0.2, max_q_value = 0.5)
#' y
build_correlation_network <- function(object,
                                      min_abs_correlation = 0,
                                      max_q_value = 1,
                                      direction = c("any", "positive", "negative"),
                                      top_n = Inf,
                                      keep_isolated = FALSE) {
  if (!methods::is(object, "microbe_metabolite_association")) {
    stop("object must be a microbe_metabolite_association.")
  }
  filtered <- filter_correlation(
    object,
    min_abs_correlation = min_abs_correlation,
    max_q_value = max_q_value,
    direction = direction
  )@result
  filtered <- filtered %>%
    dplyr::arrange(dplyr::desc(abs(.data$estimate)))
  if (is.finite(top_n)) {
    filtered <- filtered %>% dplyr::slice_head(n = top_n)
  }
  
  edge_data <- filtered %>%
    dplyr::transmute(
      from = .data$taxon_id,
      to = .data$metabolite_id,
      weight = .data$estimate,
      q_value = .data$q_value,
      direction = .data$direction,
      method = .data$method
    )
  
  node_data <- dplyr::bind_rows(
    filtered %>%
      dplyr::transmute(
        name = .data$taxon_id,
        block = "microbiome",
        label = .data$taxon_id,
        annotation = dplyr::coalesce(.data$Genus_microbiome, .data$Family_microbiome, .data$Phylum_microbiome)
      ),
    filtered %>%
      dplyr::transmute(
        name = .data$metabolite_id,
        block = "metabolome",
        label = dplyr::coalesce(.data$compound_name_annotation, .data$metabolite_id),
        annotation = dplyr::coalesce(.data$kegg_id_annotation, .data$hmdb_id_annotation)
      )
  ) %>%
    dplyr::distinct()
  node_data$type <- node_data$block == "metabolome"
  
  if (!isTRUE(keep_isolated) && nrow(edge_data) == 0) {
    node_data <- node_data[0, , drop = FALSE]
  }
  
  new(
    Class = "microbe_metabolite_network",
    node_data = node_data,
    edge_data = edge_data,
    method = object@method,
    microbiome_rank = object@microbiome_rank,
    thresholds = list(
      min_abs_correlation = min_abs_correlation,
      max_q_value = max_q_value,
      direction = match.arg(direction),
      top_n = top_n
    )
  )
}


#' Extract Network Tables
#'
#' Return node and edge tables from a network object.
#'
#' @param object A `microbe_metabolite_network` object.
#'
#' @return A named list with `nodes` and `edges`.
#' @export
extract_network_table <- function(object) {
  if (!methods::is(object, "microbe_metabolite_network")) {
    stop("object must be a microbe_metabolite_network.")
  }
  list(nodes = object@node_data, edges = object@edge_data)
}


#' Convert a Correlation Network to tbl_graph
#'
#' @param object A `microbe_metabolite_network` object.
#'
#' @return A `tbl_graph` object.
#' @export
convert_network_graph <- function(object) {
  if (!methods::is(object, "microbe_metabolite_network")) {
    stop("object must be a microbe_metabolite_network.")
  }
  build_correlation_tbl_graph(object)
}


#' Calculate Network Centrality
#'
#' Add node centrality metrics to a correlation network.
#'
#' @param object A `microbe_metabolite_network` object.
#'
#' @return A data.frame of node centrality metrics.
#' @export
calculate_network_centrality <- function(object) {
  graph_object <- build_correlation_tbl_graph(object)
  graph_object %>%
    tidygraph::activate("nodes") %>%
    dplyr::mutate(
      degree = tidygraph::centrality_degree(),
      betweenness = tidygraph::centrality_betweenness(),
      closeness = tidygraph::centrality_closeness()
    ) %>%
    as.data.frame()
}


#' Detect Network Modules
#'
#' Run community detection on a microbe-metabolite network.
#'
#' @param object A `microbe_metabolite_network` object.
#' @param method Community detection method.
#'
#' @return A data.frame of node-module assignments.
#' @export
detect_network_modules <- function(object,
                                   method = c("louvain", "walktrap")) {
  method <- match.arg(method)
  graph_object <- build_correlation_tbl_graph(object) %>% tidygraph::activate("nodes")
  graph_object <- switch(
    method,
    louvain = dplyr::mutate(graph_object, module = tidygraph::group_louvain()),
    walktrap = dplyr::mutate(graph_object, module = tidygraph::group_walktrap())
  )
  as.data.frame(graph_object)
}


#' Summarize Network Modules
#'
#' Aggregate module composition from a network module assignment table.
#'
#' @param module_table Output from [microbiomedataset::detect_network_modules()].
#'
#' @return A summary data.frame.
#' @export
summarise_network_modules <- function(module_table) {
  module_table <- as.data.frame(module_table, check.names = FALSE)
  if (!all(c("module", "block") %in% colnames(module_table))) {
    stop("module_table must contain module and block columns.")
  }
  module_table %>%
    dplyr::group_by(.data$module) %>%
    dplyr::summarise(
      n_nodes = dplyr::n(),
      n_microbiome = sum(.data$block == "microbiome"),
      n_metabolome = sum(.data$block == "metabolome"),
      .groups = "drop"
    ) %>%
    dplyr::arrange(.data$module)
}


#' Plot a Correlation Network
#'
#' Visualize a correlation-derived bipartite network.
#'
#' @param object A `microbe_metabolite_network` or
#'   `microbe_metabolite_association` object.
#' @param top_n Number of edges to keep when `object` is a correlation result.
#' @param min_abs_correlation Minimum absolute edge weight to keep when
#'   `object` is a correlation result.
#' @param max_q_value Maximum q value to keep when `object` is a correlation
#'   result.
#' @param direction Direction of edges to keep when `object` is a correlation
#'   result.
#'
#' @return A `ggplot` object.
#' @export
plot_correlation_network <- function(object,
                                     top_n = 40,
                                     min_abs_correlation = 0,
                                     max_q_value = 1,
                                     direction = c("any", "positive", "negative")) {
  if (methods::is(object, "microbe_metabolite_association")) {
    object <- build_correlation_network(
      object,
      min_abs_correlation = min_abs_correlation,
      max_q_value = max_q_value,
      direction = direction,
      top_n = top_n
    )
  }
  if (!methods::is(object, "microbe_metabolite_network")) {
    stop("object must be a microbe_metabolite_network or microbe_metabolite_association.")
  }
  
  if (nrow(object@edge_data) == 0) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0, y = 0, label = "No edges available") +
        ggplot2::theme_void()
    )
  }
  
  graph_object <- build_correlation_tbl_graph(object)
  ggraph::ggraph(graph_object, layout = "bipartite") +
    ggraph::geom_edge_link(ggplot2::aes(width = abs(.data$weight), color = .data$weight), alpha = 0.7) +
    ggraph::geom_node_point(ggplot2::aes(color = .data$block), size = 3) +
    ggraph::geom_node_text(ggplot2::aes(label = .data$label), repel = TRUE, size = 3) +
    ggraph::scale_edge_colour_gradient2(low = "#2166ac", mid = "#f7f7f7", high = "#b2182b", guide = "none") +
    ggplot2::theme_void()
}


#' Integrate Matched Microbiome and Metabolome Data
#'
#' Wrapper for multiblock integration methods on matched omics matrices.
#'
#' @param object A `microbiome_metabolome_dataset` object.
#' @param microbiome_data A `microbiome_dataset` object. Used when `object` is
#'   not supplied.
#' @param metabolome_data A `mass_dataset` object. Used when `object` is not
#'   supplied.
#' @param sample_link Optional sample link table used when `object` is not
#'   supplied.
#' @param method Integration method.
#' @param microbiome_rank Taxonomy rank for microbiome aggregation.
#' @param ncomp Number of latent components.
#'
#' @return A `crossomics_integration` object.
#' @export
#' @examples
#' data("global_patterns", package = "microbiomedataset")
#'
#' data("demo_crossomics", package = "microbiomedataset")
#'
#' if (requireNamespace("mixOmics", quietly = TRUE)) {
#'   x <- integrate_microbe_metabolite(
#'     microbiome_data = demo_crossomics$microbiome_data,
#'     metabolome_data = demo_crossomics$metabolome_data,
#'     sample_link = demo_crossomics$sample_link,
#'     method = "spls"
#'   )
#'   class(x)
#' }
integrate_microbe_metabolite <- function(object = NULL,
                                         microbiome_data = NULL,
                                         metabolome_data = NULL,
                                         sample_link = NULL,
                                         method = c("spls", "diablo"),
                                         microbiome_rank = "Genus",
                                         ncomp = 2) {
  method <- match.arg(method)
  if (!requireNamespace("mixOmics", quietly = TRUE)) {
    stop("Package 'mixOmics' is required for integrate_microbe_metabolite().")
  }
  resolved <- resolve_crossomics_input(
    object = object,
    microbiome_data = microbiome_data,
    metabolome_data = metabolome_data,
    sample_link = sample_link
  )
  
  extracted <- extract_crossomics_matrix(
    object = resolved$object,
    microbiome_rank = microbiome_rank,
    microbiome_transform = "relative",
    metabolome_transform = "log"
  )
  
  X <- t(extracted$microbiome_matrix)
  Y <- t(extracted$metabolome_matrix)
  keep_X <- apply(X, 2, stats::sd, na.rm = TRUE) > 0
  keep_Y <- apply(Y, 2, stats::sd, na.rm = TRUE) > 0
  X <- X[, keep_X, drop = FALSE]
  Y <- Y[, keep_Y, drop = FALSE]
  if (nrow(X) < 3 || ncol(X) < 3 || ncol(Y) < 3) {
    stop(
      "Integration requires at least 3 matched samples and at least 3 features in each omics block after preprocessing."
    )
  }
  ncomp <- min(as.integer(ncomp), ncol(X), ncol(Y))
  
  fit <- switch(
    method,
    spls = mixOmics::spls(X, Y, ncomp = ncomp),
    diablo = mixOmics::block.spls(
      X = list(microbiome = X, metabolome = Y),
      Y = seq_len(nrow(X)),
      ncomp = ncomp
    )
  )
  
  build_crossomics_integration_result(
    fit = fit,
    method = method,
    microbiome_rank = microbiome_rank,
    sample_info = extracted$sample_info
  )
}


#' Infer Mechanistic Microbe-Metabolite Links
#'
#' Combine statistical association results with a pathway link table and
#' standardized metabolite annotation to create a mechanism-oriented evidence
#' table.
#'
#' @param object A `microbiome_metabolome_dataset` object.
#' @param microbiome_data A `microbiome_dataset` object. Used when `object` is
#'   not supplied.
#' @param metabolome_data A `mass_dataset` object. Used when `object` is not
#'   supplied.
#' @param sample_link Optional sample link table used when `object` is not
#'   supplied.
#' @param pathway_link Optional pathway link table used when `object` is not
#'   supplied.
#' @param association_result Optional `microbe_metabolite_association` result. If
#'   missing, associations are computed internally.
#' @param microbiome_rank Taxonomy rank for microbiome aggregation.
#' @param q_value_cutoff Significance threshold used to keep statistical links.
#'
#' @return A `crossomics_mechanism` object.
#' @export
#' @examples
#' data("global_patterns", package = "microbiomedataset")
#'
#' data("demo_crossomics", package = "microbiomedataset")
#'
#' pathway_link <- data.frame(
#'   taxon_id = demo_crossomics$microbiome_data@variable_info$Genus[1],
#'   metabolite_id = demo_crossomics$metabolome_data@variable_info$variable_id[1],
#'   pathway_id = "pathway_1",
#'   evidence_type = "annotation_overlap"
#' )
#'
#' x <- create_microbiome_metabolome_dataset(
#'   demo_crossomics$microbiome_data,
#'   demo_crossomics$metabolome_data,
#'   sample_link = demo_crossomics$sample_link,
#'   pathway_link = pathway_link
#' )
#' y <- infer_metabolic_link(x, microbiome_rank = "Genus")
#' class(y)
infer_metabolic_link <- function(object = NULL,
                                 microbiome_data = NULL,
                                 metabolome_data = NULL,
                                 sample_link = NULL,
                                 pathway_link = NULL,
                                 association_result = NULL,
                                 microbiome_rank = "Genus",
                                 q_value_cutoff = 0.1) {
  resolved <- resolve_crossomics_input(
    object = object,
    microbiome_data = microbiome_data,
    metabolome_data = metabolome_data,
    sample_link = sample_link,
    pathway_link = pathway_link
  )
  
  if (is.null(association_result)) {
    association_result <- associate_microbe_metabolite(
      object = resolved$object,
      microbiome_rank = microbiome_rank
    )
  }
  if (!methods::is(association_result, "microbe_metabolite_association")) {
    stop("association_result must be a microbe_metabolite_association.")
  }
  if (is.null(resolved$pathway_link)) {
    stop("No pathway_link is available. Provide pathway_link or a joint object containing it.")
  }
  
  pathway_link <- normalize_crossomics_pathway_link(resolved$pathway_link)
  metabolome_annotation_table <- standardize_metabolite_annotation(
    resolved$metabolome_data@annotation_table,
    variable_info = resolved$metabolome_data@variable_info
  )
  result <- association_result@result %>%
    dplyr::filter(.data$q_value <= q_value_cutoff | is.na(.data$q_value)) %>%
    dplyr::left_join(pathway_link, by = c("taxon_id", "metabolite_id")) %>%
    dplyr::left_join(
      dplyr::rename_with(
        metabolome_annotation_table,
        ~ paste0(.x, "_annotation"),
        -variable_id
      ),
      by = c("metabolite_id" = "variable_id")
    )
  
  for (column in c(
    "pathway_id_annotation",
    "pathway_name_annotation",
    "compound_name_annotation",
    "annotation_level_annotation"
  )) {
    if (!column %in% colnames(result)) {
      result[[column]] <- NA_character_
    }
  }
  
  result <- result %>%
    dplyr::mutate(
      evidence_type = dplyr::coalesce(.data$evidence_type, "association_only"),
      pathway_id = dplyr::coalesce(.data$pathway_id, .data$pathway_id_annotation),
      pathway_name = dplyr::coalesce(.data$pathway_name, .data$pathway_name_annotation),
      compound_name = dplyr::coalesce(.data$compound_name, .data$compound_name_annotation),
      annotation_level = dplyr::coalesce(.data$annotation_level, .data$annotation_level_annotation),
      mechanism_tier = dplyr::case_when(
        !is.na(.data$pathway_id) & !is.na(.data$annotation_level) ~ "pathway_plus_annotation",
        !is.na(.data$pathway_id) ~ "pathway_only",
        TRUE ~ "association_only"
      ),
      mechanism_score = abs(.data$estimate) * dplyr::if_else(is.na(.data$q_value), 0, 1 - .data$q_value)
    ) %>%
    dplyr::arrange(dplyr::desc(.data$mechanism_score))
  
  new(
    Class = "crossomics_mechanism",
    result = result,
    method = "pathway_linked_association",
    evidence_type = "statistical_plus_annotation"
  )
}


#' Plot Cross-Omics Integration
#'
#' Visualize sample coordinates from a `crossomics_integration` object and
#' optionally overlay top microbiome/metabolome loading arrows.
#'
#' @param object A `crossomics_integration` object.
#' @param axes Numeric vector of length 2 specifying plotted components.
#' @param color_by Optional column name in `sample_info`.
#' @param show_loading Should top feature loadings be displayed?
#' @param loading_block Which loading block(s) should be displayed.
#' @param top_n Number of features per block to plot when `show_loading = TRUE`.
#' @param loading_scale Multiplier used to scale loading arrows.
#'
#' @return A `ggplot` object.
#' @export
plot_crossomics_integration <- function(object,
                                        axes = c(1, 2),
                                        color_by = NULL,
                                        show_loading = FALSE,
                                        loading_block = c("both", "microbiome", "metabolome"),
                                        top_n = 10,
                                        loading_scale = 1) {
  if (!methods::is(object, "crossomics_integration")) {
    stop("object must be a crossomics_integration.")
  }
  
  loading_block <- match.arg(loading_block)
  axes <- as.integer(axes)
  axis_columns <- paste0("Axis", axes)
  if (!all(axis_columns %in% colnames(object@sample_coord))) {
    stop("Requested axes are not available in object@sample_coord.")
  }
  
  p <- ggplot2::ggplot(
    object@sample_coord,
    ggplot2::aes(x = .data[[axis_columns[1]]], y = .data[[axis_columns[2]]])
  )
  
  if (!is.null(color_by) && color_by %in% colnames(object@sample_coord)) {
    p <- p + ggplot2::geom_point(ggplot2::aes(color = .data[[color_by]]), size = 2.5, alpha = 0.85)
  } else {
    p <- p + ggplot2::geom_point(size = 2.5, alpha = 0.85, color = "#1b6f6a")
  }
  
  if (isTRUE(show_loading)) {
    loading_data <- collect_crossomics_loading_plot_data(
      object = object,
      axes = axes,
      loading_block = loading_block,
      top_n = top_n,
      loading_scale = loading_scale
    )
    if (nrow(loading_data) > 0) {
      p <- p +
        ggplot2::geom_segment(
          data = loading_data,
          ggplot2::aes(
            x = 0,
            y = 0,
            xend = .data$xend,
            yend = .data$yend,
            linetype = .data$block
          ),
          inherit.aes = FALSE,
          color = "#c45508",
          arrow = ggplot2::arrow(length = grid::unit(0.12, "inches"))
        ) +
        ggplot2::geom_text(
          data = loading_data,
          ggplot2::aes(x = .data$xend, y = .data$yend, label = .data$feature_id),
          inherit.aes = FALSE,
          size = 3,
          vjust = -0.3
        )
    }
  }
  
  p +
    ggplot2::labs(
      x = axis_columns[1],
      y = axis_columns[2],
      color = color_by,
      linetype = "Loading block"
    ) +
    ggplot2::theme_bw()
}


#' Extract Cross-Omics Integration Result
#'
#' Convert a `crossomics_integration` object into a tidy list of sample
#' coordinates and feature loadings.
#'
#' @param object A `crossomics_integration` object.
#'
#' @return A list with `sample_coord`, `microbiome_loading`, and
#'   `metabolome_loading`.
#' @export
extract_crossomics_integration <- function(object) {
  if (!methods::is(object, "crossomics_integration")) {
    stop("object must be a crossomics_integration.")
  }
  list(
    sample_coord = object@sample_coord,
    microbiome_loading = object@microbiome_loading,
    metabolome_loading = object@metabolome_loading
  )
}


#' Plot Cross-Omics Loading Heatmap
#'
#' Display top loadings from one integration block as a heatmap-like tile plot.
#'
#' @param object A `crossomics_integration` object.
#' @param block Which block should be plotted.
#' @param axes Optional axes to include.
#' @param top_n Number of features to display.
#'
#' @return A `ggplot` object.
#' @export
plot_crossomics_loading_heatmap <- function(object,
                                            block = c("microbiome", "metabolome"),
                                            axes = NULL,
                                            top_n = 20) {
  if (!methods::is(object, "crossomics_integration")) {
    stop("object must be a crossomics_integration.")
  }
  block <- match.arg(block)
  loading_df <- if (identical(block, "microbiome")) object@microbiome_loading else object@metabolome_loading
  axis_columns <- grep("^Axis", colnames(loading_df), value = TRUE)
  if (!is.null(axes)) {
    axis_columns <- paste0("Axis", axes)
  }
  axis_columns <- intersect(axis_columns, colnames(loading_df))
  if (length(axis_columns) == 0) {
    stop("No valid loading axes are available.")
  }
  
  plot_data <- loading_df %>%
    dplyr::mutate(score = rowSums(abs(dplyr::across(dplyr::all_of(axis_columns))), na.rm = TRUE)) %>%
    dplyr::arrange(dplyr::desc(.data$score)) %>%
    dplyr::slice_head(n = top_n) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(axis_columns),
      names_to = "axis",
      values_to = "loading"
    )
  
  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = .data$axis, y = stats::reorder(.data$feature_id, .data$score), fill = .data$loading)
  ) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient2(low = "#2166ac", mid = "#f7f7f7", high = "#b2182b") +
    ggplot2::labs(x = "Component", y = NULL, fill = "Loading") +
    ggplot2::theme_bw()
}


#' Plot Cross-Omics Feature Network
#'
#' Visualize links between microbiome and metabolome features using loading
#' similarity or association scores.
#'
#' @param object A `crossomics_integration` or `microbe_metabolite_association`
#'   object.
#' @param axes Axes used to derive loading-based edge weights when `object` is a
#'   `crossomics_integration`.
#' @param top_n Number of edges to retain.
#' @param min_weight Minimum absolute edge weight to keep.
#'
#' @return A `ggplot` object.
#' @export
plot_crossomics_network <- function(object,
                                    axes = c(1, 2),
                                    top_n = 40,
                                    min_weight = 0) {
  if (methods::is(object, "crossomics_integration")) {
    axes <- paste0("Axis", axes)
    micro <- object@microbiome_loading
    meta <- object@metabolome_loading
    axes <- intersect(axes, intersect(colnames(micro), colnames(meta)))
    if (length(axes) == 0) {
      stop("Requested axes are not available in the integration object.")
    }
    edge_data <- merge(
      micro[, c("feature_id", axes), drop = FALSE],
      meta[, c("feature_id", axes), drop = FALSE],
      by = NULL,
      suffixes = c("_microbiome", "_metabolome")
    )
    edge_data$weight <- apply(edge_data, 1, function(x) {
      sum(as.numeric(x[paste0(axes, "_microbiome")]) * as.numeric(x[paste0(axes, "_metabolome")]))
    })
    edge_data <- edge_data %>%
      dplyr::transmute(
        from = .data$feature_id_microbiome,
        to = .data$feature_id_metabolome,
        weight = .data$weight
      )
  } else if (methods::is(object, "microbe_metabolite_association")) {
    edge_data <- object@result %>%
      dplyr::transmute(
        from = .data$taxon_id,
        to = .data$metabolite_id,
        weight = .data$estimate
      )
  } else {
    stop("object must be a crossomics_integration or microbe_metabolite_association.")
  }
  
  edge_data <- edge_data %>%
    dplyr::filter(abs(.data$weight) >= min_weight) %>%
    dplyr::arrange(dplyr::desc(abs(.data$weight))) %>%
    dplyr::slice_head(n = top_n)
  
  if (nrow(edge_data) == 0) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0, y = 0, label = "No edges available") +
        ggplot2::theme_void()
    )
  }
  
  node_data <- dplyr::bind_rows(
    data.frame(name = unique(edge_data$from), block = "microbiome", stringsAsFactors = FALSE),
    data.frame(name = unique(edge_data$to), block = "metabolome", stringsAsFactors = FALSE)
  ) %>%
    dplyr::distinct()
  node_data$type <- node_data$block == "metabolome"
  edge_data$from <- match(edge_data$from, node_data$name)
  edge_data$to <- match(edge_data$to, node_data$name)
  
  graph_object <- tidygraph::tbl_graph(nodes = node_data, edges = edge_data, directed = FALSE)
  
  ggraph::ggraph(graph_object, layout = "bipartite") +
    ggraph::geom_edge_link(ggplot2::aes(width = abs(.data$weight), color = .data$weight), alpha = 0.7) +
    ggraph::geom_node_point(ggplot2::aes(color = .data$block), size = 3) +
    ggraph::geom_node_text(ggplot2::aes(label = .data$name), repel = TRUE, size = 3) +
    ggraph::scale_edge_colour_gradient2(low = "#2166ac", mid = "#f7f7f7", high = "#b2182b", guide = "none") +
    ggplot2::theme_void()
}


#' Summarize Pathway-Level Mechanisms
#'
#' Aggregate mechanism evidence from taxon-metabolite links to pathway-level
#' summaries, optionally incorporating taxon-to-pathway mappings.
#'
#' @param mechanism_result A `crossomics_mechanism` object.
#' @param taxon_pathway_link Optional taxon-to-pathway link table.
#'
#' @return A pathway summary data.frame.
#' @export
summarise_pathways <- function(mechanism_result,
                               taxon_pathway_link = NULL) {
  if (!methods::is(mechanism_result, "crossomics_mechanism")) {
    stop("mechanism_result must be a crossomics_mechanism.")
  }
  taxon_pathway_link <- standardize_taxon_pathway_link(taxon_pathway_link)
  result <- mechanism_result@result
  if (!is.null(taxon_pathway_link)) {
    result <- result %>%
      dplyr::left_join(
        taxon_pathway_link,
        by = c("taxon_id", "pathway_id"),
        suffix = c("", "_taxon")
      ) %>%
      dplyr::mutate(
        pathway_name = dplyr::coalesce(.data$pathway_name, .data$pathway_name_taxon),
        evidence_type = dplyr::coalesce(.data$evidence_type, .data$evidence_type_taxon)
      )
  }
  
  result %>%
    dplyr::filter(!is.na(.data$pathway_id)) %>%
    dplyr::group_by(.data$pathway_id, .data$pathway_name) %>%
    dplyr::summarise(
      n_taxa = dplyr::n_distinct(.data$taxon_id),
      n_metabolites = dplyr::n_distinct(.data$metabolite_id),
      mean_mechanism_score = mean(.data$mechanism_score, na.rm = TRUE),
      max_mechanism_score = if (all(is.na(.data$mechanism_score))) NA_real_ else max(.data$mechanism_score, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(.data$max_mechanism_score))
}


#' @keywords internal
filter_crossomics_matrix_rows <- function(x,
                                          min_prevalence = 0,
                                          min_mean = -Inf,
                                          min_sd = 0) {
  x <- as.matrix(x)
  if (nrow(x) == 0) {
    return(x)
  }
  prevalence <- rowSums(x > 0, na.rm = TRUE)
  mean_abundance <- rowMeans(x, na.rm = TRUE)
  sd_abundance <- apply(x, 1, stats::sd, na.rm = TRUE)
  sd_abundance[is.na(sd_abundance)] <- 0
  keep <- prevalence >= min_prevalence &
    mean_abundance >= min_mean &
    sd_abundance >= min_sd
  x[keep, , drop = FALSE]
}


#' @keywords internal
calculate_correlation_table <- function(microbiome_matrix,
                                        metabolome_matrix,
                                        method,
                                        p_adjust_method) {
  if (nrow(microbiome_matrix) == 0 || nrow(metabolome_matrix) == 0) {
    return(data.frame(
      taxon_id = character(),
      metabolite_id = character(),
      estimate = numeric(),
      p_value = numeric(),
      n_samples = integer(),
      q_value = numeric(),
      direction = character(),
      stringsAsFactors = FALSE
    ))
  }
  if (identical(method, "sparcc")) {
    correlation_matrix <- calculate_sparcc_crossblock(
      microbiome_matrix = microbiome_matrix,
      metabolome_matrix = metabolome_matrix
    )
    result <- as.data.frame(as.table(correlation_matrix), stringsAsFactors = FALSE)
    colnames(result) <- c("taxon_id", "metabolite_id", "estimate")
    result$p_value <- NA_real_
    result$n_samples <- ncol(microbiome_matrix)
  } else {
    association_rows <- vector("list", length = nrow(microbiome_matrix) * nrow(metabolome_matrix))
    row_index <- 0L
    for (i in seq_len(nrow(microbiome_matrix))) {
      microbe_values <- as.numeric(microbiome_matrix[i, ])
      for (j in seq_len(nrow(metabolome_matrix))) {
        metabolite_values <- as.numeric(metabolome_matrix[j, ])
        complete_index <- stats::complete.cases(microbe_values, metabolite_values)
        test_result <- if (sum(complete_index) >= 3 &&
                           stats::sd(microbe_values[complete_index]) > 0 &&
                           stats::sd(metabolite_values[complete_index]) > 0) {
          suppressWarnings(
            stats::cor.test(
              x = microbe_values[complete_index],
              y = metabolite_values[complete_index],
              method = method
            )
          )
        } else {
          NULL
        }
        row_index <- row_index + 1L
        association_rows[[row_index]] <- data.frame(
          taxon_id = rownames(microbiome_matrix)[i],
          metabolite_id = rownames(metabolome_matrix)[j],
          estimate = if (is.null(test_result)) NA_real_ else unname(test_result$estimate),
          p_value = if (is.null(test_result)) NA_real_ else test_result$p.value,
          n_samples = sum(complete_index),
          stringsAsFactors = FALSE
        )
      }
    }
    result <- dplyr::bind_rows(association_rows)
  }
  
  result$q_value <- stats::p.adjust(result$p_value, method = p_adjust_method)
  result$direction <- dplyr::case_when(
    is.na(result$estimate) ~ NA_character_,
    result$estimate > 0 ~ "positive",
    result$estimate < 0 ~ "negative",
    TRUE ~ "neutral"
  )
  result
}


#' @keywords internal
calculate_sparcc_crossblock <- function(microbiome_matrix,
                                        metabolome_matrix) {
  if (!requireNamespace("SpiecEasi", quietly = TRUE)) {
    stop("Package 'SpiecEasi' is required for method = 'sparcc'.")
  }
  combined_matrix <- rbind(microbiome_matrix, metabolome_matrix)
  sparcc_fit <- SpiecEasi::sparcc(t(combined_matrix))
  if (!"Cor" %in% names(sparcc_fit)) {
    stop("SpiecEasi::sparcc() did not return the expected Cor matrix.")
  }
  sparcc_fit$Cor[
    rownames(microbiome_matrix),
    rownames(metabolome_matrix),
    drop = FALSE
  ]
}


#' @keywords internal
build_correlation_tbl_graph <- function(object) {
  node_data <- object@node_data
  edge_data <- object@edge_data
  if (nrow(node_data) == 0 || nrow(edge_data) == 0) {
    stop("object does not contain any edges to convert into a graph.")
  }
  edge_tbl <- edge_data
  edge_tbl$from <- match(edge_tbl$from, node_data$name)
  edge_tbl$to <- match(edge_tbl$to, node_data$name)
  tidygraph::tbl_graph(nodes = node_data, edges = edge_tbl, directed = FALSE)
}


#' @keywords internal
resolve_crossomics_input <- function(object = NULL,
                                     microbiome_data = NULL,
                                     metabolome_data = NULL,
                                     sample_link = NULL,
                                     feature_link = NULL,
                                     pathway_link = NULL,
                                     align_samples = TRUE) {
  if (!is.null(object)) {
    if (!methods::is(object, "microbiome_metabolome_dataset")) {
      stop("object must be a microbiome_metabolome_dataset.")
    }
    return(list(
      microbiome_data = object@microbiome_data,
      metabolome_data = object@metabolome_data,
      sample_link = normalize_crossomics_sample_link(object@sample_link),
      feature_link = object@feature_link,
      pathway_link = object@pathway_link,
      object = object
    ))
  }
  
  if (is.null(microbiome_data) || is.null(metabolome_data)) {
    stop("Provide either object or both microbiome_data and metabolome_data.")
  }
  if (!methods::is(microbiome_data, "microbiome_dataset")) {
    stop("microbiome_data must be a microbiome_dataset.")
  }
  if (!methods::is(metabolome_data, "mass_dataset")) {
    stop("metabolome_data must be a mass_dataset.")
  }
  
  created <- create_microbiome_metabolome_dataset(
    microbiome_data = microbiome_data,
    metabolome_data = metabolome_data,
    sample_link = sample_link,
    feature_link = feature_link,
    pathway_link = pathway_link,
    align_samples = align_samples
  )
  
  list(
    microbiome_data = created@microbiome_data,
    metabolome_data = created@metabolome_data,
    sample_link = normalize_crossomics_sample_link(created@sample_link),
    feature_link = created@feature_link,
    pathway_link = created@pathway_link,
    object = created
  )
}


#' @keywords internal
build_crossomics_integration_result <- function(fit,
                                                method,
                                                microbiome_rank,
                                                sample_info) {
  if (identical(method, "spls")) {
    microbiome_variates <- as.data.frame(fit$variates$X)
    metabolome_variates <- as.data.frame(fit$variates$Y)
    microbiome_loading <- as.data.frame(fit$loadings$X)
    metabolome_loading <- as.data.frame(fit$loadings$Y)
  } else {
    microbiome_variates <- as.data.frame(fit$variates$microbiome)
    metabolome_variates <- as.data.frame(fit$variates$metabolome)
    microbiome_loading <- as.data.frame(fit$loadings$microbiome)
    metabolome_loading <- as.data.frame(fit$loadings$metabolome)
  }
  
  ncomp <- min(ncol(microbiome_variates), ncol(metabolome_variates))
  common_samples <- intersect(rownames(microbiome_variates), rownames(metabolome_variates))
  microbiome_variates <- microbiome_variates[common_samples, seq_len(ncomp), drop = FALSE]
  metabolome_variates <- metabolome_variates[common_samples, seq_len(ncomp), drop = FALSE]
  sample_coord <- as.data.frame(
    (as.matrix(microbiome_variates) + as.matrix(metabolome_variates)) / 2,
    check.names = FALSE
  )
  colnames(sample_coord) <- paste0("Axis", seq_len(ncol(sample_coord)))
  sample_coord$pair_id <- rownames(sample_coord)
  sample_coord <- dplyr::left_join(sample_coord, sample_info, by = "pair_id")
  
  microbiome_loading <- as.data.frame(microbiome_loading[, seq_len(ncomp), drop = FALSE], check.names = FALSE)
  colnames(microbiome_loading) <- paste0("Axis", seq_len(ncol(microbiome_loading)))
  microbiome_loading$feature_id <- rownames(microbiome_loading)
  microbiome_loading$block <- "microbiome"
  
  metabolome_loading <- as.data.frame(metabolome_loading[, seq_len(ncomp), drop = FALSE], check.names = FALSE)
  colnames(metabolome_loading) <- paste0("Axis", seq_len(ncol(metabolome_loading)))
  metabolome_loading$feature_id <- rownames(metabolome_loading)
  metabolome_loading$block <- "metabolome"
  
  new(
    Class = "crossomics_integration",
    sample_coord = sample_coord,
    microbiome_loading = microbiome_loading,
    metabolome_loading = metabolome_loading,
    sample_info = sample_info,
    method = method,
    microbiome_rank = if (is.null(microbiome_rank)) "" else microbiome_rank,
    ncomp = as.integer(ncomp)
  )
}


#' @keywords internal
collect_crossomics_loading_plot_data <- function(object,
                                                 axes,
                                                 loading_block,
                                                 top_n,
                                                 loading_scale) {
  axis_columns <- paste0("Axis", axes)
  loading_frames <- list()
  if (loading_block %in% c("both", "microbiome")) {
    loading_frames[["microbiome"]] <- object@microbiome_loading
  }
  if (loading_block %in% c("both", "metabolome")) {
    loading_frames[["metabolome"]] <- object@metabolome_loading
  }
  
  loading_data <- lapply(loading_frames, function(df) {
    if (!all(axis_columns %in% colnames(df))) {
      return(NULL)
    }
    df %>%
      dplyr::mutate(score = abs(.data[[axis_columns[1]]]) + abs(.data[[axis_columns[2]]])) %>%
      dplyr::arrange(dplyr::desc(.data$score)) %>%
      dplyr::slice_head(n = top_n) %>%
      dplyr::mutate(
        xend = .data[[axis_columns[1]]] * loading_scale,
        yend = .data[[axis_columns[2]]] * loading_scale
      )
  })
  
  dplyr::bind_rows(loading_data)
}
