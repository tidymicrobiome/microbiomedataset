#' Create a Differential Abundance Result Object
#'
#' Wrap a tidy differential abundance table in a standard result class used by
#' plotting helpers.
#'
#' @param result A data.frame containing at least `variable_id` and `estimate`.
#' @param method Differential abundance method label.
#' @param taxonomic_rank Taxonomic level represented in the result.
#' @param group Optional comparison label.
#'
#' @return A `differential_abundance_result` object.
#' @export
#' @examples
#' data("global_patterns", package = "microbiomedataset")
#'
#' x0 <- prune_taxa(global_patterns, variable_id = global_patterns@variable_info$variable_id[1:200])
#' genus_object <- summarise_taxa(x0, taxonomic_rank = "Genus")
#' result_table <- data.frame(
#'   variable_id = genus_object@variable_info$variable_id[1:10],
#'   estimate = seq(-2, 2, length.out = 10),
#'   p_value = seq(0.01, 0.1, length.out = 10),
#'   q_value = seq(0.02, 0.2, length.out = 10)
#' )
#' da_result <- create_differential_abundance_result(
#'   result = cbind(result_table, genus_object@variable_info[1:10, ]),
#'   method = "external",
#'   taxonomic_rank = "Genus"
#' )
#' da_result
create_differential_abundance_result <- function(result,
                                                 method = "external",
                                                 taxonomic_rank = "",
                                                 group = "") {
  result <- as.data.frame(result, check.names = FALSE)
  if (!all(c("variable_id", "estimate") %in% colnames(result))) {
    stop("result must contain at least variable_id and estimate columns.")
  }
  if (!"p_value" %in% colnames(result)) {
    result$p_value <- NA_real_
  }
  if (!"q_value" %in% colnames(result)) {
    result$q_value <- stats::p.adjust(result$p_value, method = "BH")
  }
  if (!"direction" %in% colnames(result)) {
    result$direction <- dplyr::case_when(
      is.na(result$estimate) ~ NA_character_,
      result$estimate > 0 ~ "positive",
      result$estimate < 0 ~ "negative",
      TRUE ~ "neutral"
    )
  }
  result$variable_id <- as.character(result$variable_id)
  new(
    Class = "differential_abundance_result",
    result = result,
    method = as.character(method),
    taxonomic_rank = as.character(taxonomic_rank),
    group = as.character(group)
  )
}


#' Plot a Correlation Heatmap
#'
#' Draw a heatmap of top microbe-metabolite correlations.
#'
#' @param object A `microbe_metabolite_association` object.
#' @param top_n_taxa Number of taxa retained.
#' @param top_n_metabolites Number of metabolites retained.
#' @param value Column mapped to fill.
#'
#' @return A `ggplot` object.
#' @export
#' @examples
#' data("demo_crossomics", package = "microbiomedataset")
#'
#' correlation <- calculate_correlation(
#'   microbiome_data = demo_crossomics$microbiome_data,
#'   metabolome_data = demo_crossomics$metabolome_data,
#'   sample_link = demo_crossomics$sample_link,
#'   microbiome_rank = "Genus",
#'   metabolome_transform = "none"
#' )
#' plot_correlation_heatmap(correlation, top_n_taxa = 8, top_n_metabolites = 8)
plot_correlation_heatmap <- function(object,
                                     top_n_taxa = 20,
                                     top_n_metabolites = 20,
                                     value = c("estimate", "q_value")) {
  if (!methods::is(object, "microbe_metabolite_association")) {
    stop("object must be a microbe_metabolite_association.")
  }
  value <- match.arg(value)
  result <- object@result
  top_taxa <- result %>%
    dplyr::group_by(.data$taxon_id) %>%
    dplyr::summarise(score = max(abs(.data$estimate), na.rm = TRUE), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(.data$score)) %>%
    dplyr::slice_head(n = top_n_taxa) %>%
    dplyr::pull(.data$taxon_id)
  top_metabolites <- result %>%
    dplyr::group_by(.data$metabolite_id) %>%
    dplyr::summarise(score = max(abs(.data$estimate), na.rm = TRUE), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(.data$score)) %>%
    dplyr::slice_head(n = top_n_metabolites) %>%
    dplyr::pull(.data$metabolite_id)
  plot_data <- result %>%
    dplyr::filter(.data$taxon_id %in% top_taxa, .data$metabolite_id %in% top_metabolites) %>%
    dplyr::mutate(
      taxon_id = factor(.data$taxon_id, levels = rev(unique(top_taxa))),
      metabolite_label = dplyr::coalesce(.data$compound_name_annotation, .data$metabolite_id),
      metabolite_label = factor(.data$metabolite_label, levels = unique(.data$metabolite_label))
    )
  
  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = .data$metabolite_label, y = .data$taxon_id, fill = .data[[value]])
  ) +
    ggplot2::geom_tile() +
    {
      if (identical(value, "estimate")) {
        ggplot2::scale_fill_gradient2(low = "#2166ac", mid = "#f7f7f7", high = "#b2182b")
      } else {
        ggplot2::scale_fill_gradient(low = "#f7fbff", high = "#08306b", trans = "reverse")
      }
    } +
    ggplot2::labs(x = "Metabolite", y = "Taxon", fill = value) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}


#' Plot Network Modules
#'
#' Visualize network modules from a correlation network.
#'
#' @param object A `microbe_metabolite_network` object.
#' @param method Community detection method.
#'
#' @return A `ggplot` object.
#' @export
plot_network_modules <- function(object,
                                 method = c("louvain", "walktrap")) {
  if (!methods::is(object, "microbe_metabolite_network")) {
    stop("object must be a microbe_metabolite_network.")
  }
  module_table <- detect_network_modules(object, method = match.arg(method))
  graph_object <- build_correlation_tbl_graph(object) %>%
    tidygraph::activate("nodes") %>%
    dplyr::left_join(module_table[, c("name", "module"), drop = FALSE], by = "name")
  ggraph::ggraph(graph_object, layout = "bipartite") +
    ggraph::geom_edge_link(ggplot2::aes(width = abs(.data$weight)), alpha = 0.3, color = "grey60") +
    ggraph::geom_node_point(ggplot2::aes(color = as.factor(.data$module), shape = .data$block), size = 3) +
    ggraph::geom_node_text(ggplot2::aes(label = .data$label), repel = TRUE, size = 3) +
    ggplot2::labs(color = "Module", shape = "Block") +
    ggplot2::theme_void()
}


#' Plot a Network Module Summary
#'
#' Summarize module sizes and block composition as a bar chart.
#'
#' @param object A `microbe_metabolite_network` object or a module summary
#'   table produced by [summarise_network_modules()].
#' @param method Community detection method used when `object` is a network.
#' @param value Summary metric displayed on the x-axis.
#' @param top_n Maximum number of modules shown.
#'
#' @return A `ggplot` object.
#' @export
#' @examples
#' data("demo_crossomics", package = "microbiomedataset")
#'
#' correlation <- calculate_correlation(
#'   microbiome_data = demo_crossomics$microbiome_data,
#'   metabolome_data = demo_crossomics$metabolome_data,
#'   sample_link = demo_crossomics$sample_link,
#'   microbiome_rank = "Genus",
#'   metabolome_transform = "none"
#' )
#' network <- build_correlation_network(
#'   correlation,
#'   min_abs_correlation = 0.2,
#'   max_q_value = 1,
#'   top_n = 20
#' )
#' plot_module_summary(network)
plot_module_summary <- function(object,
                                method = c("louvain", "walktrap"),
                                value = c("n_nodes", "n_microbiome", "n_metabolome"),
                                top_n = 15) {
  value <- match.arg(value)
  if (methods::is(object, "microbe_metabolite_network")) {
    module_tbl <- detect_network_modules(object, method = match.arg(method))
    summary_tbl <- summarise_network_modules(module_tbl)
  } else {
    summary_tbl <- as.data.frame(object, check.names = FALSE)
  }
  if (!all(c("module", "n_nodes", "n_microbiome", "n_metabolome") %in% colnames(summary_tbl))) {
    stop("object must be a microbe_metabolite_network or a module summary table.")
  }
  plot_data <- summary_tbl %>%
    dplyr::arrange(dplyr::desc(.data[[value]])) %>%
    dplyr::slice_head(n = top_n) %>%
    dplyr::mutate(module = stats::reorder(as.factor(.data$module), .data[[value]]))
  
  ggplot2::ggplot(plot_data, ggplot2::aes(x = .data[[value]], y = .data$module, fill = .data[[value]])) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::labs(x = value, y = "Module") +
    ggplot2::theme_bw()
}


#' Build a Mechanism Network
#'
#' Convert a mechanism result into a layered graph.
#'
#' @param object A `crossomics_mechanism` object.
#' @param top_n Maximum number of links kept.
#'
#' @return A `microbe_metabolite_network` object.
#' @export
build_mechanism_network <- function(object,
                                    top_n = 30) {
  if (!methods::is(object, "crossomics_mechanism")) {
    stop("object must be a crossomics_mechanism.")
  }
  result <- object@result %>%
    dplyr::filter(!is.na(.data$pathway_id)) %>%
    dplyr::arrange(dplyr::desc(.data$mechanism_score)) %>%
    dplyr::slice_head(n = top_n)
  edge_data <- dplyr::bind_rows(
    result %>%
      dplyr::transmute(from = .data$taxon_id, to = .data$pathway_id, weight = .data$mechanism_score, edge_type = "taxon_pathway"),
    result %>%
      dplyr::transmute(from = .data$pathway_id, to = .data$metabolite_id, weight = .data$mechanism_score, edge_type = "pathway_metabolite")
  ) %>%
    dplyr::distinct()
  node_data <- dplyr::bind_rows(
    result %>% dplyr::transmute(name = .data$taxon_id, block = "microbiome", label = .data$taxon_id, annotation = .data$taxonomic_rank),
    result %>% dplyr::transmute(name = .data$pathway_id, block = "pathway", label = dplyr::coalesce(.data$pathway_name, .data$pathway_id), annotation = .data$evidence_type),
    result %>% dplyr::transmute(name = .data$metabolite_id, block = "metabolome", label = dplyr::coalesce(.data$compound_name, .data$metabolite_id), annotation = .data$annotation_level)
  ) %>%
    dplyr::distinct()
  node_data$type <- node_data$block != "microbiome"
  new(
    Class = "microbe_metabolite_network",
    node_data = node_data,
    edge_data = edge_data,
    method = "mechanism",
    microbiome_rank = "mixed",
    thresholds = list(top_n = top_n)
  )
}


#' Plot a Mechanism Network
#'
#' Visualize a taxon-pathway-metabolite mechanism network.
#'
#' @param object A `crossomics_mechanism` or `microbe_metabolite_network`
#'   object.
#' @param top_n Maximum number of links kept when `object` is a mechanism
#'   result.
#'
#' @return A `ggplot` object.
#' @export
plot_mechanism_network <- function(object,
                                   top_n = 30) {
  if (methods::is(object, "crossomics_mechanism")) {
    object <- build_mechanism_network(object, top_n = top_n)
  }
  if (!methods::is(object, "microbe_metabolite_network")) {
    stop("object must be a crossomics_mechanism or microbe_metabolite_network.")
  }
  edge_data <- object@edge_data
  node_data <- object@node_data
  if (!"edge_type" %in% colnames(edge_data)) {
    stop("object does not contain mechanism edge metadata.")
  }
  edge_tbl <- edge_data
  edge_tbl$from <- match(edge_tbl$from, node_data$name)
  edge_tbl$to <- match(edge_tbl$to, node_data$name)
  graph_object <- tidygraph::tbl_graph(nodes = node_data, edges = edge_tbl, directed = FALSE)
  ggraph::ggraph(graph_object, layout = "fr") +
    ggraph::geom_edge_link(ggplot2::aes(edge_colour = .data$edge_type, edge_width = .data$weight), alpha = 0.45) +
    ggraph::geom_node_point(ggplot2::aes(color = .data$block), size = 3) +
    ggraph::geom_node_text(ggplot2::aes(label = .data$label), repel = TRUE, size = 3) +
    ggraph::scale_edge_colour_manual(values = c(taxon_pathway = "#1b9e77", pathway_metabolite = "#d95f02")) +
    ggplot2::labs(color = NULL) +
    ggplot2::theme_void()
}


#' Plot a Pathway Summary
#'
#' Visualize pathway-level mechanism summaries as a ranked bar chart.
#'
#' @param object A `crossomics_mechanism` object or a pathway summary table.
#' @param taxon_pathway_link Optional taxon-to-pathway link table used when
#'   `object` is a mechanism result.
#' @param value Summary metric to display.
#' @param top_n Maximum number of pathways displayed.
#'
#' @return A `ggplot` object.
#' @export
#' @examples
#' data("demo_crossomics", package = "microbiomedataset")
#'
#' association <- calculate_correlation(
#'   microbiome_data = demo_crossomics$microbiome_data,
#'   metabolome_data = demo_crossomics$metabolome_data,
#'   sample_link = demo_crossomics$sample_link,
#'   microbiome_rank = "Genus",
#'   metabolome_transform = "none"
#' )
#' pathway_link <- standardize_pathway_link(
#'   data.frame(
#'     taxon_id = summarise_taxa(
#'       demo_crossomics$microbiome_data,
#'       taxonomic_rank = "Genus"
#'     )@variable_info$variable_id[1],
#'     metabolite_id = demo_crossomics$metabolome_data@annotation_table$variable_id[1],
#'     pathway_id = "pathway_a",
#'     pathway_name = "Pathway A",
#'     stringsAsFactors = FALSE
#'   )
#' )
#' mechanism <- infer_metabolic_link(
#'   microbiome_data = demo_crossomics$microbiome_data,
#'   metabolome_data = demo_crossomics$metabolome_data,
#'   sample_link = demo_crossomics$sample_link,
#'   pathway_link = pathway_link,
#'   association_result = association,
#'   microbiome_rank = "Genus",
#'   q_value_cutoff = 1
#' )
#' plot_pathway_summary(mechanism, top_n = 10)
plot_pathway_summary <- function(object,
                                 taxon_pathway_link = NULL,
                                 value = c("max_mechanism_score", "mean_mechanism_score", "n_taxa", "n_metabolites"),
                                 top_n = 15) {
  value <- match.arg(value)
  if (methods::is(object, "crossomics_mechanism")) {
    summary_tbl <- summarise_pathways(
      mechanism_result = object,
      taxon_pathway_link = taxon_pathway_link
    )
  } else {
    summary_tbl <- as.data.frame(object, check.names = FALSE)
  }
  if (!all(c("pathway_id", "n_taxa", "n_metabolites", "mean_mechanism_score", "max_mechanism_score") %in% colnames(summary_tbl))) {
    stop("object must be a crossomics_mechanism or a pathway summary table.")
  }
  plot_data <- summary_tbl %>%
    dplyr::mutate(pathway_label = dplyr::coalesce(.data$pathway_name, .data$pathway_id)) %>%
    dplyr::arrange(dplyr::desc(.data[[value]])) %>%
    dplyr::slice_head(n = top_n) %>%
    dplyr::mutate(pathway_label = stats::reorder(.data$pathway_label, .data[[value]]))
  
  ggplot2::ggplot(plot_data, ggplot2::aes(x = .data[[value]], y = .data$pathway_label, fill = .data[[value]])) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::labs(x = value, y = "Pathway") +
    ggplot2::theme_bw()
}


#' Plot a Differential Abundance Volcano Plot
#'
#' @param object A `differential_abundance_result` object.
#' @param q_value_cutoff Significance threshold.
#' @param label_top_n Number of top features to label.
#'
#' @return A `ggplot` object.
#' @export
plot_differential_volcano <- function(object,
                                      q_value_cutoff = 0.1,
                                      label_top_n = 10) {
  if (!methods::is(object, "differential_abundance_result")) {
    stop("object must be a differential_abundance_result.")
  }
  plot_data <- object@result %>%
    dplyr::mutate(
      minus_log10_q = -log10(pmax(.data$q_value, 1e-300)),
      significant = ifelse(is.na(.data$q_value), "no", ifelse(.data$q_value <= q_value_cutoff, "yes", "no"))
    )
  label_data <- plot_data %>%
    dplyr::arrange(dplyr::desc(abs(.data$estimate) * .data$minus_log10_q)) %>%
    dplyr::slice_head(n = label_top_n)
  ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$estimate, y = .data$minus_log10_q, color = .data$significant)) +
    ggplot2::geom_point(alpha = 0.8) +
    ggplot2::geom_text(
      data = label_data,
      ggplot2::aes(label = .data$variable_id),
      size = 3,
      check_overlap = TRUE,
      show.legend = FALSE,
      nudge_y = 0.1
    ) +
    ggplot2::labs(x = "Effect size", y = expression(-log[10](q~value)), color = "Significant") +
    ggplot2::theme_bw()
}


#' Plot Differential Effect Sizes
#'
#' @param object A `differential_abundance_result` object.
#' @param top_n Number of features displayed.
#'
#' @return A `ggplot` object.
#' @export
plot_differential_effect_size <- function(object,
                                          top_n = 20) {
  if (!methods::is(object, "differential_abundance_result")) {
    stop("object must be a differential_abundance_result.")
  }
  plot_data <- object@result %>%
    dplyr::arrange(dplyr::desc(abs(.data$estimate))) %>%
    dplyr::slice_head(n = top_n) %>%
    dplyr::mutate(variable_id = stats::reorder(.data$variable_id, .data$estimate))
  ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$variable_id, y = .data$estimate, fill = .data$direction)) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::labs(x = NULL, y = "Effect size", fill = "Direction") +
    ggplot2::theme_bw()
}


#' Plot a Differential Abundance Cladogram
#'
#' @param object A `differential_abundance_result` object.
#' @param top_n Number of features retained for the cladogram.
#'
#' @return A `ggplot` object.
#' @export
plot_differential_cladogram <- function(object,
                                        top_n = 30) {
  if (!methods::is(object, "differential_abundance_result")) {
    stop("object must be a differential_abundance_result.")
  }
  taxonomy_cols <- intersect(microbiome_taxonomy_ranks(), colnames(object@result))
  if (length(taxonomy_cols) == 0) {
    stop("object@result must contain taxonomy columns to draw a cladogram.")
  }
  plot_result <- object@result %>%
    dplyr::arrange(dplyr::desc(abs(.data$estimate))) %>%
    dplyr::slice_head(n = top_n)
  variable_info <- plot_result[, c("variable_id", taxonomy_cols), drop = FALSE]
  taxa_tree <- rebuild_taxa_tree(variable_info)
  if (is.null(taxa_tree)) {
    stop("Unable to build a taxonomy tree from the supplied result.")
  }
  link_tbl <- build_taxa_tree_link(variable_info, taxa_tree)
  node_scores <- plot_result %>%
    dplyr::left_join(link_tbl[, c("variable_id", "node", "node_label"), drop = FALSE], by = "variable_id") %>%
    dplyr::group_by(.data$node, .data$node_label) %>%
    dplyr::summarise(
      estimate = mean(.data$estimate, na.rm = TRUE),
      direction = dplyr::first(.data$direction),
      .groups = "drop"
    )
  tree_tbl <- as.data.frame(tidytree::as_tibble(taxa_tree)) %>%
    dplyr::left_join(node_scores, by = "node")
  graph_object <- tidygraph::as_tbl_graph(tidytree::as.phylo(taxa_tree))
  graph_object <- graph_object %>%
    tidygraph::activate("nodes") %>%
    dplyr::left_join(
      node_scores[, c("node_label", "estimate"), drop = FALSE],
      by = c("name" = "node_label")
    )
  ggraph::ggraph(graph_object, layout = "dendrogram", circular = TRUE) +
    ggraph::geom_edge_diagonal(alpha = 0.35) +
    ggraph::geom_node_point(ggplot2::aes(color = .data$estimate), size = 2.2, na.rm = TRUE) +
    ggraph::geom_node_text(
      ggplot2::aes(label = ifelse(!is.na(.data$estimate), .data$name, "")),
      repel = TRUE,
      size = 2.8
    ) +
    ggplot2::scale_color_gradient2(low = "#2166ac", mid = "#f7f7f7", high = "#b2182b") +
    ggplot2::theme_void()
}
