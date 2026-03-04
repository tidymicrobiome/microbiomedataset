#' Calculate Pathway Enrichment from Mechanism Links
#'
#' Perform a simple over-representation analysis on pathway-linked metabolites
#' using high-scoring mechanism links as the observed set.
#'
#' @param object A `crossomics_mechanism` object.
#' @param score_cutoff Minimum mechanism score retained in the observed set.
#' @param top_n Optional number of top links retained before enrichment.
#'
#' @return A data.frame of pathway enrichment statistics.
#' @export
#' @examples
#' data("demo_crossomics", package = "microbiomedataset")
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
#' calculate_pathway_enrichment(mechanism)
calculate_pathway_enrichment <- function(object,
                                         score_cutoff = NULL,
                                         top_n = NULL) {
  if (!methods::is(object, "crossomics_mechanism")) {
    stop("object must be a crossomics_mechanism.")
  }
  result <- object@result %>%
    dplyr::filter(!is.na(.data$pathway_id), !is.na(.data$metabolite_id))
  if (!is.null(top_n)) {
    result <- result %>%
      dplyr::arrange(dplyr::desc(.data$mechanism_score)) %>%
      dplyr::slice_head(n = top_n)
  }
  if (is.null(score_cutoff)) {
    score_cutoff <- stats::median(result$mechanism_score, na.rm = TRUE)
  }
  background_tbl <- result %>%
    dplyr::distinct(.data$pathway_id, .data$pathway_name, .data$metabolite_id)
  observed_tbl <- background_tbl %>%
    dplyr::inner_join(
      result %>%
        dplyr::filter(.data$mechanism_score >= score_cutoff) %>%
        dplyr::distinct(.data$pathway_id, .data$metabolite_id),
      by = c("pathway_id", "metabolite_id")
    )
  background_metabolites <- unique(background_tbl$metabolite_id)
  observed_metabolites <- unique(observed_tbl$metabolite_id)
  n_background_total <- length(background_metabolites)
  n_observed_total <- length(observed_metabolites)
  enrichment_tbl <- background_tbl %>%
    dplyr::group_by(.data$pathway_id, .data$pathway_name) %>%
    dplyr::summarise(
      n_background = dplyr::n_distinct(.data$metabolite_id),
      .groups = "drop"
    ) %>%
    dplyr::left_join(
      observed_tbl %>%
        dplyr::group_by(.data$pathway_id) %>%
        dplyr::summarise(n_observed = dplyr::n_distinct(.data$metabolite_id), .groups = "drop"),
      by = "pathway_id"
    ) %>%
    dplyr::mutate(
      n_observed = dplyr::coalesce(.data$n_observed, 0L),
      p_value = stats::phyper(
        q = pmax(.data$n_observed - 1, 0),
        m = .data$n_background,
        n = pmax(n_background_total - .data$n_background, 0),
        k = n_observed_total,
        lower.tail = FALSE
      ),
      q_value = stats::p.adjust(.data$p_value, method = "BH"),
      enrichment_ratio = ifelse(
        .data$n_background == 0 || n_observed_total == 0 || n_background_total == 0,
        NA_real_,
        (.data$n_observed / n_observed_total) / (.data$n_background / n_background_total)
      )
    ) %>%
    dplyr::arrange(.data$q_value, dplyr::desc(.data$enrichment_ratio))
  enrichment_tbl
}


#' Plot Pathway Enrichment
#'
#' Draw a bubble plot from pathway enrichment results.
#'
#' @param object A `crossomics_mechanism` object or pathway enrichment table.
#' @param score_cutoff Minimum mechanism score retained when `object` is a
#'   mechanism result.
#' @param top_n Maximum number of pathways displayed.
#'
#' @return A `ggplot` object.
#' @export
plot_pathway_enrichment <- function(object,
                                    score_cutoff = NULL,
                                    top_n = 20) {
  enrichment_tbl <- if (methods::is(object, "crossomics_mechanism")) {
    calculate_pathway_enrichment(object, score_cutoff = score_cutoff)
  } else {
    as.data.frame(object, check.names = FALSE)
  }
  if (!all(c("pathway_id", "n_observed", "q_value", "enrichment_ratio") %in% colnames(enrichment_tbl))) {
    stop("object must be a crossomics_mechanism or a pathway enrichment table.")
  }
  plot_data <- enrichment_tbl %>%
    dplyr::mutate(
      pathway_label = dplyr::coalesce(.data$pathway_name, .data$pathway_id),
      minus_log10_q = -log10(pmax(.data$q_value, 1e-300))
    ) %>%
    dplyr::arrange(.data$q_value, dplyr::desc(.data$enrichment_ratio)) %>%
    dplyr::slice_head(n = top_n) %>%
    dplyr::mutate(pathway_label = stats::reorder(.data$pathway_label, .data$minus_log10_q))
  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = .data$enrichment_ratio, y = .data$pathway_label, size = .data$n_observed, color = .data$minus_log10_q)
  ) +
    ggplot2::geom_point(alpha = 0.85) +
    ggplot2::scale_color_gradient(low = "#f7fbff", high = "#cb181d") +
    ggplot2::labs(x = "Enrichment ratio", y = NULL, size = "Observed metabolites", color = expression(-log[10](q))) +
    ggplot2::theme_bw()
}


#' Plot an Interactive Tree
#'
#' Convert [plot_tree()] output to an interactive `plotly` widget.
#'
#' @inheritParams plot_tree
#' @return A `plotly` htmlwidget.
#' @export
#' @examples
#' data("global_patterns", package = "microbiomedataset")
#' x <- prune_taxa(global_patterns, variable_id = global_patterns@variable_info$variable_id[1:120])
#' x <- align_tree(x, tree = "taxa_tree")
#' plot_tree_interactive(x, tree = "taxa_tree", taxonomic_rank = "Phylum")
plot_tree_interactive <- function(object,
                                  tree = c("otu_tree", "taxa_tree"),
                                  layout = "dendrogram",
                                  use_link = TRUE,
                                  color_by = NULL,
                                  size_by = NULL,
                                  abundance_method = c("sum", "mean", "median"),
                                  taxonomic_rank = c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species"),
                                  show_tip_label = TRUE,
                                  tip_label_size = 2.5) {
  suppressWarnings(
    plotly::ggplotly(
      plot_tree(
        object = object,
        tree = match.arg(tree),
        layout = layout,
        use_link = use_link,
        color_by = color_by,
        size_by = size_by,
        abundance_method = match.arg(abundance_method),
        taxonomic_rank = match.arg(taxonomic_rank),
        show_tip_label = show_tip_label,
        tip_label_size = tip_label_size
      )
    )
  )
}


#' Create and Visualize Longitudinal Mixed-Effect Results
#'
#' Wrap external mixed-effect model output in a standardized result class and
#' visualize the resulting effect estimates.
#'
#' @name mixed_effect_result_api
#' @aliases create_longitudinal_mixed_effect_result
#' @aliases plot_longitudinal_mixed_effect
#' @aliases plot_longitudinal_effect_heatmap
#'
#' @param result A data.frame containing at least `feature`, `term`, and
#'   `estimate`.
#' @param method Modeling method label.
#' @param time_variable Time variable used in the model.
#' @param group_variable Optional grouping variable.
#'
#' @return A `longitudinal_mixed_effect_result` object.
#' @export
#' @examples
#' result <- data.frame(
#'   feature = paste0("Taxon", 1:4),
#'   term = "time",
#'   estimate = c(0.4, -0.2, 0.1, 0.3),
#'   lower_ci = c(0.2, -0.4, -0.1, 0.1),
#'   upper_ci = c(0.6, 0.0, 0.3, 0.5)
#' )
#' create_longitudinal_mixed_effect_result(
#'   result,
#'   method = "external",
#'   time_variable = "time",
#'   group_variable = "group"
#' )
create_longitudinal_mixed_effect_result <- function(result,
                                                    method = "external",
                                                    time_variable = "",
                                                    group_variable = "") {
  result <- as.data.frame(result, check.names = FALSE)
  if (!all(c("feature", "term", "estimate") %in% colnames(result))) {
    stop("result must contain feature, term, and estimate columns.")
  }
  if (!"p_value" %in% colnames(result)) {
    result$p_value <- NA_real_
  }
  if (!"q_value" %in% colnames(result)) {
    result$q_value <- stats::p.adjust(result$p_value, method = "BH")
  }
  new(
    Class = "longitudinal_mixed_effect_result",
    result = result,
    method = as.character(method),
    time_variable = as.character(time_variable),
    group_variable = as.character(group_variable)
  )
}


#' @describeIn mixed_effect_result_api Draw a forest plot of feature-level
#'   mixed-effect estimates.
#'
#' @param object A `longitudinal_mixed_effect_result` object.
#' @param term Optional model term retained for plotting.
#' @param top_n Maximum number of features displayed.
#'
#' @return A `ggplot` object.
#' @export
plot_longitudinal_mixed_effect <- function(object,
                                           term = NULL,
                                           top_n = 20) {
  if (!methods::is(object, "longitudinal_mixed_effect_result")) {
    stop("object must be a longitudinal_mixed_effect_result.")
  }
  plot_data <- object@result
  if (!is.null(term)) {
    plot_data <- plot_data %>% dplyr::filter(.data$term == term)
  }
  plot_data <- plot_data %>%
    dplyr::arrange(dplyr::desc(abs(.data$estimate))) %>%
    dplyr::slice_head(n = top_n) %>%
    dplyr::mutate(feature = stats::reorder(.data$feature, .data$estimate))
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$estimate, y = .data$feature, color = .data$term))
  if (all(c("lower_ci", "upper_ci") %in% colnames(plot_data))) {
    p <- p + ggplot2::geom_errorbarh(ggplot2::aes(xmin = .data$lower_ci, xmax = .data$upper_ci), height = 0.2)
  }
  p +
    ggplot2::geom_point(size = 2.2) +
    ggplot2::geom_vline(xintercept = 0, linetype = 2) +
    ggplot2::labs(x = "Estimate", y = NULL, color = "Term") +
    ggplot2::theme_bw()
}


#' @describeIn mixed_effect_result_api Show feature-by-term effect estimates as
#'   a heatmap.
#'
#' @param object A `longitudinal_mixed_effect_result` object.
#' @param top_n Maximum number of features displayed.
#'
#' @return A `ggplot` object.
#' @export
plot_longitudinal_effect_heatmap <- function(object,
                                             top_n = 30) {
  if (!methods::is(object, "longitudinal_mixed_effect_result")) {
    stop("object must be a longitudinal_mixed_effect_result.")
  }
  selected_features <- object@result %>%
    dplyr::group_by(.data$feature) %>%
    dplyr::summarise(score = max(abs(.data$estimate), na.rm = TRUE), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(.data$score)) %>%
    dplyr::slice_head(n = top_n) %>%
    dplyr::pull(.data$feature)
  plot_data <- object@result %>%
    dplyr::filter(.data$feature %in% selected_features)
  ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$term, y = .data$feature, fill = .data$estimate)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient2(low = "#2166ac", mid = "#f7f7f7", high = "#b2182b") +
    ggplot2::labs(x = "Term", y = "Feature", fill = "Estimate") +
    ggplot2::theme_bw()
}
