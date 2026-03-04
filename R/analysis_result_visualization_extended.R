#' Plot Network Centrality
#'
#' Visualize node centrality scores from a correlation network.
#'
#' @param object A `microbe_metabolite_network` object.
#' @param metric Centrality metric to display.
#' @param top_n Number of nodes displayed.
#'
#' @return A `ggplot` object.
#' @export
#' @examples
#' data("demo_crossomics", package = "microbiomedataset")
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
#' plot_network_centrality(network)
plot_network_centrality <- function(object,
                                    metric = c("degree", "betweenness", "closeness"),
                                    top_n = 20) {
  metric <- match.arg(metric)
  plot_data <- calculate_network_centrality(object) %>%
    dplyr::arrange(dplyr::desc(.data[[metric]])) %>%
    dplyr::slice_head(n = top_n) %>%
    dplyr::mutate(label = stats::reorder(.data$label, .data[[metric]]))
  
  ggplot2::ggplot(plot_data, ggplot2::aes(x = .data[[metric]], y = .data$label, fill = .data$block)) +
    ggplot2::geom_col() +
    ggplot2::labs(x = metric, y = NULL, fill = "Block") +
    ggplot2::theme_bw()
}


#' Plot Module Composition
#'
#' Summarize node block composition across network modules.
#'
#' @param object A `microbe_metabolite_network` object or a module assignment table.
#' @param method Community detection method used when `object` is a network.
#'
#' @return A `ggplot` object.
#' @export
plot_module_composition <- function(object,
                                    method = c("louvain", "walktrap")) {
  module_table <- if (methods::is(object, "microbe_metabolite_network")) {
    detect_network_modules(object, method = match.arg(method))
  } else {
    as.data.frame(object, check.names = FALSE)
  }
  if (!all(c("module", "block") %in% colnames(module_table))) {
    stop("object must be a microbe_metabolite_network or a module assignment table.")
  }
  plot_data <- module_table %>%
    dplyr::count(.data$module, .data$block, name = "n")
  ggplot2::ggplot(plot_data, ggplot2::aes(x = as.factor(.data$module), y = .data$n, fill = .data$block)) +
    ggplot2::geom_col(position = "stack") +
    ggplot2::labs(x = "Module", y = "Nodes", fill = "Block") +
    ggplot2::theme_bw()
}


#' Plot Correlations as a Dotplot
#'
#' Draw a bubble plot of top microbe-metabolite correlations.
#'
#' @param object A `microbe_metabolite_association` object.
#' @param top_n_taxa Number of taxa displayed.
#' @param top_n_metabolites Number of metabolites displayed.
#'
#' @return A `ggplot` object.
#' @export
plot_correlation_dotplot <- function(object,
                                     top_n_taxa = 20,
                                     top_n_metabolites = 20) {
  if (!methods::is(object, "microbe_metabolite_association")) {
    stop("object must be a microbe_metabolite_association.")
  }
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
      metabolite_label = dplyr::coalesce(.data$compound_name_annotation, .data$metabolite_id)
    )
  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = .data$metabolite_label, y = .data$taxon_id, size = abs(.data$estimate), color = .data$estimate)
  ) +
    ggplot2::geom_point(alpha = 0.85) +
    ggplot2::scale_color_gradient2(low = "#2166ac", mid = "#f7f7f7", high = "#b2182b") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::labs(x = "Metabolite", y = "Taxon", size = "|correlation|", color = "Correlation")
}


#' Plot a Mechanism Summary
#'
#' Rank taxon-pathway-metabolite mechanism links by score.
#'
#' @param object A `crossomics_mechanism` object.
#' @param top_n Number of links displayed.
#'
#' @return A `ggplot` object.
#' @export
plot_mechanism_summary <- function(object,
                                   top_n = 20) {
  if (!methods::is(object, "crossomics_mechanism")) {
    stop("object must be a crossomics_mechanism.")
  }
  plot_data <- object@result %>%
    dplyr::filter(!is.na(.data$pathway_id)) %>%
    dplyr::arrange(dplyr::desc(.data$mechanism_score)) %>%
    dplyr::slice_head(n = top_n) %>%
    dplyr::mutate(
      link_label = paste(.data$taxon_id, dplyr::coalesce(.data$pathway_name, .data$pathway_id), dplyr::coalesce(.data$compound_name, .data$metabolite_id), sep = " -> "),
      link_label = stats::reorder(.data$link_label, .data$mechanism_score)
    )
  ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$mechanism_score, y = .data$link_label, fill = .data$mechanism_tier)) +
    ggplot2::geom_col() +
    ggplot2::labs(x = "Mechanism score", y = NULL, fill = "Tier") +
    ggplot2::theme_bw()
}


#' Plot a Differential Abundance MA Plot
#'
#' Draw an MA-style plot of abundance versus effect size.
#'
#' @param object A `differential_abundance_result` object.
#' @param abundance_column Optional column in `object@result` used as abundance.
#' @param q_value_cutoff Significance cutoff.
#'
#' @return A `ggplot` object.
#' @export
plot_differential_ma <- function(object,
                                 abundance_column = NULL,
                                 q_value_cutoff = 0.1) {
  if (!methods::is(object, "differential_abundance_result")) {
    stop("object must be a differential_abundance_result.")
  }
  plot_data <- object@result
  if (is.null(abundance_column)) {
    abundance_column <- if ("mean_abundance" %in% colnames(plot_data)) "mean_abundance" else NULL
  }
  if (is.null(abundance_column)) {
    plot_data$mean_abundance <- seq_len(nrow(plot_data))
    abundance_column <- "mean_abundance"
  }
  plot_data$significant <- ifelse(is.na(plot_data$q_value), "no", ifelse(plot_data$q_value <= q_value_cutoff, "yes", "no"))
  ggplot2::ggplot(plot_data, ggplot2::aes(x = log10(.data[[abundance_column]] + 1e-8), y = .data$estimate, color = .data$significant)) +
    ggplot2::geom_point(alpha = 0.8) +
    ggplot2::geom_hline(yintercept = 0, linetype = 2) +
    ggplot2::theme_bw() +
    ggplot2::labs(x = paste0("log10(", abundance_column, ")"), y = "Effect size", color = "Significant")
}


#' Plot a Differential Abundance Heatmap
#'
#' Visualize top differential taxa across samples.
#'
#' @param object A `differential_abundance_result` object.
#' @param microbiome_data A `microbiome_dataset` object.
#' @param top_n Number of taxa displayed.
#' @param sample_annotation Optional sample metadata columns.
#' @param relative Whether to use relative abundance.
#'
#' @return A `ComplexHeatmap::Heatmap` object.
#' @export
plot_differential_heatmap <- function(object,
                                      microbiome_data,
                                      top_n = 20,
                                      sample_annotation = NULL,
                                      relative = TRUE) {
  if (!methods::is(object, "differential_abundance_result")) {
    stop("object must be a differential_abundance_result.")
  }
  if (!methods::is(microbiome_data, "microbiome_dataset")) {
    stop("microbiome_data must be a microbiome_dataset.")
  }
  plot_data <- collect_differential_abundance_long(
    object = object,
    microbiome_data = microbiome_data,
    top_n = top_n,
    relative = relative
  )
  heatmap_matrix <- plot_data %>%
    dplyr::select(.data$variable_id, .data$sample_id, .data$abundance) %>%
    tidyr::pivot_wider(names_from = .data$sample_id, values_from = .data$abundance, values_fill = 0) %>%
    as.data.frame(check.names = FALSE)
  rownames(heatmap_matrix) <- heatmap_matrix$variable_id
  heatmap_matrix$variable_id <- NULL
  heatmap_matrix <- as.matrix(heatmap_matrix)
  top_ann <- NULL
  if (!is.null(sample_annotation)) {
    sample_info <- extract_sample_info(microbiome_data)
    ann_df <- sample_info[match(colnames(heatmap_matrix), sample_info$sample_id), sample_annotation, drop = FALSE]
    rownames(ann_df) <- colnames(heatmap_matrix)
    top_ann <- ComplexHeatmap::HeatmapAnnotation(df = ann_df)
  }
  ComplexHeatmap::Heatmap(
    heatmap_matrix,
    name = if (relative) "relative_abundance" else "abundance",
    top_annotation = top_ann,
    cluster_rows = TRUE,
    cluster_columns = TRUE
  )
}


#' Plot Differential Taxa as Boxplots
#'
#' Draw grouped boxplots for top differential taxa.
#'
#' @param object A `differential_abundance_result` object.
#' @param microbiome_data A `microbiome_dataset` object.
#' @param x Sample metadata column shown on the x axis.
#' @param top_n Number of taxa displayed.
#' @param relative Whether to use relative abundance.
#'
#' @return A `ggplot` object.
#' @export
plot_differential_boxplot <- function(object,
                                      microbiome_data,
                                      x = "sample_id",
                                      top_n = 12,
                                      relative = TRUE) {
  if (!methods::is(object, "differential_abundance_result")) {
    stop("object must be a differential_abundance_result.")
  }
  if (!methods::is(microbiome_data, "microbiome_dataset")) {
    stop("microbiome_data must be a microbiome_dataset.")
  }
  plot_data <- collect_differential_abundance_long(
    object = object,
    microbiome_data = microbiome_data,
    top_n = top_n,
    relative = relative
  )
  ggplot2::ggplot(plot_data, ggplot2::aes(x = .data[[x]], y = .data$abundance, fill = .data$direction)) +
    ggplot2::geom_boxplot(outlier.shape = NA, alpha = 0.8) +
    ggplot2::geom_jitter(width = 0.15, size = 0.8, alpha = 0.6) +
    ggplot2::facet_wrap(~variable_id, scales = "free_y") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::labs(x = x, y = if (relative) "Relative abundance (%)" else "Abundance", fill = "Direction")
}


#' @keywords internal
collect_differential_abundance_long <- function(object,
                                                microbiome_data,
                                                top_n = 20,
                                                relative = TRUE) {
  plot_result <- object@result %>%
    dplyr::arrange(dplyr::desc(abs(.data$estimate))) %>%
    dplyr::slice_head(n = top_n)
  if (nzchar(object@taxonomic_rank) && object@taxonomic_rank %in% microbiome_taxonomy_ranks()) {
    long_data <- psmelt_taxa(
      microbiome_data,
      taxonomic_rank = object@taxonomic_rank,
      relative = relative
    ) %>%
      dplyr::rename(variable_id = !!rlang::sym(object@taxonomic_rank))
  } else {
    long_data <- psmelt_microbiome_dataset(microbiome_data, relative = relative)
  }
  long_data %>%
    dplyr::filter(.data$variable_id %in% plot_result$variable_id) %>%
    dplyr::left_join(
      plot_result[, intersect(c("variable_id", "estimate", "direction"), colnames(plot_result)), drop = FALSE],
      by = "variable_id"
    )
}
