#' Extract Cross-Omics Association Results
#'
#' Return the tidy association table from a
#' `microbe_metabolite_association` object.
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
#' head(extract_association_result(x))
extract_association_result <- function(object) {
  if (!methods::is(object, "microbe_metabolite_association")) {
    stop("object must be a microbe_metabolite_association.")
  }
  object@result
}


#' Convert Association Results to a Tibble
#'
#' @param object A `microbe_metabolite_association` object.
#'
#' @return A tibble.
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
#' class(as_tibble_association(x))
as_tibble_association <- function(object) {
  tibble::as_tibble(
    extract_association_result(object),
    .name_repair = "unique"
  )
}


#' Extract Network Results
#'
#' Return the node and edge tables from a `microbe_metabolite_network` object.
#'
#' @param object A `microbe_metabolite_network` object.
#' @param what Which part of the result to extract.
#'
#' @return A data.frame or named list of data.frames.
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
#' x <- build_correlation_network(x, min_abs_correlation = 0.2, max_q_value = 1)
#' names(extract_network_result(x, what = "both"))
extract_network_result <- function(object,
                                   what = c("both", "nodes", "edges")) {
  if (!methods::is(object, "microbe_metabolite_network")) {
    stop("object must be a microbe_metabolite_network.")
  }
  what <- match.arg(what)
  switch(
    what,
    both = list(nodes = object@node_data, edges = object@edge_data),
    nodes = object@node_data,
    edges = object@edge_data
  )
}


#' Convert Network Results to a Tibble
#'
#' @param object A `microbe_metabolite_network` object.
#' @param what Which part of the result to convert.
#'
#' @return A tibble or named list of tibbles.
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
#' x <- build_correlation_network(x, min_abs_correlation = 0.2, max_q_value = 1)
#' class(as_tibble_network(x, what = "nodes"))
as_tibble_network <- function(object,
                              what = c("both", "nodes", "edges")) {
  what <- match.arg(what)
  result <- extract_network_result(object, what = what)
  if (identical(what, "both")) {
    return(list(
      nodes = tibble::as_tibble(result$nodes, .name_repair = "unique"),
      edges = tibble::as_tibble(result$edges, .name_repair = "unique")
    ))
  }
  tibble::as_tibble(result, .name_repair = "unique")
}


#' Extract Differential Abundance Results
#'
#' Return the tidy result table from a `differential_abundance_result` object.
#'
#' @param object A `differential_abundance_result` object.
#'
#' @return A data.frame.
#' @export
#' @examples
#' data("global_patterns", package = "microbiomedataset")
#'
#' x0 <- prune_taxa(global_patterns, variable_id = global_patterns@variable_info$variable_id[1:120])
#' genus_object <- summarise_taxa(x0, taxonomic_rank = "Genus")
#' result_table <- data.frame(
#'   variable_id = genus_object@variable_info$variable_id[1:6],
#'   estimate = seq(-1, 1, length.out = 6)
#' )
#' x <- create_differential_abundance_result(
#'   result = cbind(result_table, genus_object@variable_info[1:6, ]),
#'   taxonomic_rank = "Genus"
#' )
#' head(extract_differential_abundance_result(x))
extract_differential_abundance_result <- function(object) {
  if (!methods::is(object, "differential_abundance_result")) {
    stop("object must be a differential_abundance_result.")
  }
  object@result
}


#' Convert Differential Abundance Results to a Tibble
#'
#' @param object A `differential_abundance_result` object.
#'
#' @return A tibble.
#' @export
#' @examples
#' data("global_patterns", package = "microbiomedataset")
#'
#' x0 <- prune_taxa(global_patterns, variable_id = global_patterns@variable_info$variable_id[1:120])
#' genus_object <- summarise_taxa(x0, taxonomic_rank = "Genus")
#' result_table <- data.frame(
#'   variable_id = genus_object@variable_info$variable_id[1:6],
#'   estimate = seq(-1, 1, length.out = 6)
#' )
#' x <- create_differential_abundance_result(
#'   result = cbind(result_table, genus_object@variable_info[1:6, ]),
#'   taxonomic_rank = "Genus"
#' )
#' class(as_tibble_differential_abundance(x))
as_tibble_differential_abundance <- function(object) {
  tibble::as_tibble(
    extract_differential_abundance_result(object),
    .name_repair = "unique"
  )
}


#' Extract Mechanism Results
#'
#' Return the tidy result table from a `crossomics_mechanism` object.
#'
#' @param object A `crossomics_mechanism` object.
#'
#' @return A data.frame.
#' @export
#' @examples
#' data("demo_crossomics", package = "microbiomedataset")
#'
#' pathway_link <- standardize_pathway_link(
#'   data.frame(
#'     taxon_id = summarise_taxa(
#'       demo_crossomics$microbiome_data,
#'       taxonomic_rank = "Genus"
#'     )@variable_info$variable_id[1],
#'     metabolite_id = demo_crossomics$metabolome_data@annotation_table$variable_id[1],
#'     pathway_id = "pathway_a",
#'     stringsAsFactors = FALSE
#'   )
#' )
#' x <- infer_metabolic_link(
#'   microbiome_data = demo_crossomics$microbiome_data,
#'   metabolome_data = demo_crossomics$metabolome_data,
#'   sample_link = demo_crossomics$sample_link,
#'   pathway_link = pathway_link,
#'   microbiome_rank = "Genus",
#'   q_value_cutoff = 1
#' )
#' head(extract_mechanism_result(x))
extract_mechanism_result <- function(object) {
  if (!methods::is(object, "crossomics_mechanism")) {
    stop("object must be a crossomics_mechanism.")
  }
  object@result
}


#' Convert Mechanism Results to a Tibble
#'
#' @param object A `crossomics_mechanism` object.
#'
#' @return A tibble.
#' @export
#' @examples
#' data("demo_crossomics", package = "microbiomedataset")
#'
#' pathway_link <- standardize_pathway_link(
#'   data.frame(
#'     taxon_id = summarise_taxa(
#'       demo_crossomics$microbiome_data,
#'       taxonomic_rank = "Genus"
#'     )@variable_info$variable_id[1],
#'     metabolite_id = demo_crossomics$metabolome_data@annotation_table$variable_id[1],
#'     pathway_id = "pathway_a",
#'     stringsAsFactors = FALSE
#'   )
#' )
#' x <- infer_metabolic_link(
#'   microbiome_data = demo_crossomics$microbiome_data,
#'   metabolome_data = demo_crossomics$metabolome_data,
#'   sample_link = demo_crossomics$sample_link,
#'   pathway_link = pathway_link,
#'   microbiome_rank = "Genus",
#'   q_value_cutoff = 1
#' )
#' class(as_tibble_mechanism(x))
as_tibble_mechanism <- function(object) {
  tibble::as_tibble(
    extract_mechanism_result(object),
    .name_repair = "unique"
  )
}


#' Convert Cross-Omics Integration Results to Tibbles
#'
#' @param object A `crossomics_integration` object.
#' @param block Which block to convert.
#'
#' @return A tibble or named list of tibbles.
#' @export
#' @examples
#' data("demo_crossomics", package = "microbiomedataset")
#'
#' if (requireNamespace("mixOmics", quietly = TRUE)) {
#'   x <- integrate_microbe_metabolite(
#'     microbiome_data = demo_crossomics$microbiome_data,
#'     metabolome_data = demo_crossomics$metabolome_data,
#'     sample_link = demo_crossomics$sample_link,
#'     method = "spls"
#'   )
#'   class(as_tibble_crossomics_integration(x, block = "sample_coord"))
#' }
as_tibble_crossomics_integration <- function(object,
                                             block = c(
                                               "all",
                                               "sample_coord",
                                               "microbiome_loading",
                                               "metabolome_loading"
                                             )) {
  if (!methods::is(object, "crossomics_integration")) {
    stop("object must be a crossomics_integration.")
  }
  block <- match.arg(block)
  if (identical(block, "all")) {
    return(list(
      sample_coord = tibble::as_tibble(object@sample_coord, .name_repair = "unique"),
      microbiome_loading = tibble::as_tibble(object@microbiome_loading, .name_repair = "unique"),
      metabolome_loading = tibble::as_tibble(object@metabolome_loading, .name_repair = "unique")
    ))
  }
  tibble::as_tibble(slot(object, block), .name_repair = "unique")
}


#' Extract Longitudinal Mixed-Effect Results
#'
#' Return the tidy result table from a `longitudinal_mixed_effect_result`
#' object.
#'
#' @rdname mixed_effect_result_api
#' @aliases extract_longitudinal_mixed_effect_result
#' @param object A `longitudinal_mixed_effect_result` object.
#'
#' @return A data.frame.
#' @export
#' @examples
#' x <- create_longitudinal_mixed_effect_result(
#'   result = data.frame(
#'     feature = paste0("Taxon", 1:4),
#'     term = "time",
#'     estimate = c(0.4, -0.2, 0.1, 0.3)
#'   ),
#'   time_variable = "time"
#' )
#' head(extract_longitudinal_mixed_effect_result(x))
extract_longitudinal_mixed_effect_result <- function(object) {
  if (!methods::is(object, "longitudinal_mixed_effect_result")) {
    stop("object must be a longitudinal_mixed_effect_result.")
  }
  object@result
}


#' Convert Longitudinal Mixed-Effect Results to a Tibble
#'
#' @rdname mixed_effect_result_api
#' @aliases as_tibble_longitudinal_mixed_effect
#' @param object A `longitudinal_mixed_effect_result` object.
#'
#' @return A tibble.
#' @export
#' @examples
#' x <- create_longitudinal_mixed_effect_result(
#'   result = data.frame(
#'     feature = paste0("Taxon", 1:4),
#'     term = "time",
#'     estimate = c(0.4, -0.2, 0.1, 0.3)
#'   ),
#'   time_variable = "time"
#' )
#' class(as_tibble_longitudinal_mixed_effect(x))
as_tibble_longitudinal_mixed_effect <- function(object) {
  tibble::as_tibble(
    extract_longitudinal_mixed_effect_result(object),
    .name_repair = "unique"
  )
}
