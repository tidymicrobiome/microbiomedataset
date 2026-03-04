#' Plot Microbiome Composition
#'
#' Draw a stacked composition barplot at a selected taxonomic rank.
#'
#' @param object A `microbiome_dataset` object.
#' @param taxonomic_rank Taxonomic rank displayed in the stack.
#' @param top_n Number of dominant taxa shown per sample.
#' @param x Sample metadata column shown on the x axis.
#' @param relative Whether to show relative abundance.
#' @param facet_by Optional sample metadata column used for faceting.
#'
#' @return A `ggplot` object.
#' @export
#' @examples
#' data("global_patterns", package = "microbiomedataset")
#'
#' plot_composition(
#'   object = global_patterns,
#'   taxonomic_rank = "Phylum",
#'   top_n = 8,
#'   x = "SampleType"
#' )
plot_composition <- function(object,
                             taxonomic_rank = c("Kingdom",
                                                "Phylum",
                                                "Class",
                                                "Order",
                                                "Family",
                                                "Genus",
                                                "Species"),
                             top_n = 10,
                             x = "sample_id",
                             relative = TRUE,
                             facet_by = NULL) {
  taxonomic_rank <- match.arg(taxonomic_rank)
  args <- list(
    object = object,
    fill = taxonomic_rank,
    top_n = top_n,
    relative = relative,
    x = x
  )
  if (!is.null(facet_by)) {
    args$facet_grid <- facet_by
  }
  do.call(plot_barplot, args)
}


#' Plot Taxon Abundance
#'
#' Draw sample-wise abundance distributions for selected taxa.
#'
#' @param object A `microbiome_dataset` object.
#' @param taxonomic_rank Taxonomic rank used for aggregation.
#' @param taxa Optional taxa to keep. Defaults to the most abundant taxa.
#' @param top_n Number of taxa shown when `taxa` is not supplied.
#' @param x Sample metadata column shown on the x axis.
#' @param geom Geometry used to display abundance.
#' @param relative Whether to show relative abundance.
#'
#' @return A `ggplot` object.
#' @export
#' @examples
#' data("global_patterns", package = "microbiomedataset")
#'
#' plot_abundance(
#'   object = global_patterns,
#'   taxonomic_rank = "Phylum",
#'   x = "SampleType",
#'   top_n = 6
#' )
plot_abundance <- function(object,
                           taxonomic_rank = c("Kingdom",
                                              "Phylum",
                                              "Class",
                                              "Order",
                                              "Family",
                                              "Genus",
                                              "Species"),
                           taxa = NULL,
                           top_n = 10,
                           x = "sample_id",
                           geom = c("boxplot", "violin", "point"),
                           relative = TRUE) {
  taxonomic_rank <- match.arg(taxonomic_rank)
  geom <- match.arg(geom)
  abundance_data <- psmelt_taxa(
    object = object,
    taxonomic_rank = taxonomic_rank,
    relative = relative
  )
  rank_values <- unique(stats::na.omit(as.character(abundance_data[[taxonomic_rank]])))
  if (is.null(taxa)) {
    taxa <- abundance_data %>%
      dplyr::group_by(.data[[taxonomic_rank]]) %>%
      dplyr::summarise(mean_abundance = mean(.data$abundance, na.rm = TRUE), .groups = "drop") %>%
      dplyr::arrange(dplyr::desc(.data$mean_abundance)) %>%
      dplyr::slice_head(n = top_n) %>%
      dplyr::pull(.data[[taxonomic_rank]])
  }
  taxa <- intersect(as.character(taxa), rank_values)
  abundance_data <- abundance_data %>%
    dplyr::filter(.data[[taxonomic_rank]] %in% taxa)
  
  p <- ggplot2::ggplot(
    abundance_data,
    ggplot2::aes(x = .data[[x]], y = .data$abundance, fill = .data[[taxonomic_rank]])
  )
  p <- switch(
    geom,
    boxplot = p + ggplot2::geom_boxplot(outlier.shape = NA, alpha = 0.85) +
      ggplot2::geom_jitter(width = 0.15, size = 0.8, alpha = 0.6),
    violin = p + ggplot2::geom_violin(trim = FALSE, alpha = 0.8) +
      ggplot2::geom_jitter(width = 0.15, size = 0.8, alpha = 0.5),
    point = p + ggplot2::geom_point(
      ggplot2::aes(color = .data[[taxonomic_rank]]),
      position = ggplot2::position_jitter(width = 0.15, height = 0),
      size = 1.2,
      alpha = 0.75
    )
  )
  p +
    ggplot2::facet_wrap(stats::as.formula(paste("~", taxonomic_rank)), scales = "free_y") +
    ggplot2::labs(
      x = x,
      y = if (relative) "Relative abundance (%)" else "Abundance",
      fill = taxonomic_rank,
      color = taxonomic_rank
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}


#' Plot a Microbiome Heatmap
#'
#' Draw a heatmap of dominant taxa across samples.
#'
#' @param object A `microbiome_dataset` object.
#' @param taxonomic_rank Taxonomic rank used for aggregation.
#' @param top_n Number of taxa displayed.
#' @param relative Whether to use relative abundance.
#' @param cluster_samples Whether to cluster samples.
#' @param cluster_taxa Whether to cluster taxa.
#' @param sample_label Sample metadata column shown on the x axis.
#'
#' @return A `ggplot` object.
#' @export
#' @examples
#' data("global_patterns", package = "microbiomedataset")
#'
#' plot_heatmap(
#'   object = global_patterns,
#'   taxonomic_rank = "Phylum",
#'   top_n = 12,
#'   sample_label = "SampleType"
#' )
plot_heatmap <- function(object,
                         taxonomic_rank = c("Kingdom",
                                            "Phylum",
                                            "Class",
                                            "Order",
                                            "Family",
                                            "Genus",
                                            "Species"),
                         top_n = 20,
                         relative = TRUE,
                         cluster_samples = TRUE,
                         cluster_taxa = TRUE,
                         sample_label = "sample_id") {
  taxonomic_rank <- match.arg(taxonomic_rank)
  abundance_data <- psmelt_taxa(
    object = object,
    taxonomic_rank = taxonomic_rank,
    relative = relative
  )
  top_taxa <- abundance_data %>%
    dplyr::group_by(.data[[taxonomic_rank]]) %>%
    dplyr::summarise(mean_abundance = mean(.data$abundance, na.rm = TRUE), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(.data$mean_abundance)) %>%
    dplyr::slice_head(n = top_n) %>%
    dplyr::pull(.data[[taxonomic_rank]])
  abundance_data <- abundance_data %>%
    dplyr::filter(.data[[taxonomic_rank]] %in% top_taxa)
  
  heatmap_matrix <- abundance_data %>%
    dplyr::select(sample_id, !!rlang::sym(taxonomic_rank), abundance) %>%
    tidyr::pivot_wider(names_from = sample_id, values_from = abundance, values_fill = 0) %>%
    as.data.frame(check.names = FALSE)
  rownames(heatmap_matrix) <- heatmap_matrix[[taxonomic_rank]]
  heatmap_matrix[[taxonomic_rank]] <- NULL
  heatmap_matrix <- as.matrix(heatmap_matrix)
  
  sample_order <- colnames(heatmap_matrix)
  taxa_order <- rownames(heatmap_matrix)
  if (isTRUE(cluster_samples) && ncol(heatmap_matrix) > 1) {
    sample_order <- colnames(heatmap_matrix)[stats::hclust(stats::dist(t(heatmap_matrix)))$order]
  }
  if (isTRUE(cluster_taxa) && nrow(heatmap_matrix) > 1) {
    taxa_order <- rownames(heatmap_matrix)[stats::hclust(stats::dist(heatmap_matrix))$order]
  }
  
  sample_info <- extract_sample_info(object)
  label_map <- sample_info[, intersect(c("sample_id", sample_label), colnames(sample_info)), drop = FALSE]
  if (!sample_label %in% colnames(label_map)) {
    label_map[[sample_label]] <- label_map$sample_id
  }
  sample_labels <- label_map[[sample_label]][match(abundance_data$sample_id, label_map$sample_id)]
  ordered_labels <- unique(label_map[[sample_label]][match(sample_order, label_map$sample_id)])
  
  plot_data <- abundance_data %>%
    dplyr::mutate(
      sample_plot = factor(sample_labels, levels = ordered_labels),
      taxon_plot = factor(.data[[taxonomic_rank]], levels = taxa_order)
    )
  
  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = .data$sample_plot, y = .data$taxon_plot, fill = .data$abundance)
  ) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient(low = "#f7fbff", high = "#08306b") +
    ggplot2::labs(
      x = sample_label,
      y = taxonomic_rank,
      fill = if (relative) "Relative abundance (%)" else "Abundance"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}


#' Plot Taxon Prevalence
#'
#' Draw a prevalence barplot for dominant taxa.
#'
#' @param object A `microbiome_dataset` object.
#' @param taxonomic_rank Taxonomic rank used for aggregation.
#' @param detection Detection threshold used in prevalence calculation.
#' @param proportion Whether prevalence should be shown as a proportion.
#' @param top_n Number of taxa displayed.
#'
#' @return A `ggplot` object.
#' @export
#' @examples
#' data("global_patterns", package = "microbiomedataset")
#'
#' x <- prune_taxa(global_patterns, variable_id = global_patterns@variable_info$variable_id[1:200])
#' plot_prevalence(
#'   object = x,
#'   taxonomic_rank = "Phylum",
#'   top_n = 10
#' )
plot_prevalence <- function(object,
                            taxonomic_rank = c("Kingdom",
                                               "Phylum",
                                               "Class",
                                               "Order",
                                               "Family",
                                               "Genus",
                                               "Species"),
                            detection = 0,
                            proportion = TRUE,
                            top_n = 20) {
  taxonomic_rank <- match.arg(taxonomic_rank)
  taxa_object <- summarise_taxa(object, taxonomic_rank = taxonomic_rank)
  prevalence_tbl <- data.frame(
    taxon_id = taxa_object@variable_info$variable_id,
    prevalence = taxa_prevalence(taxa_object, detection = detection, proportion = proportion),
    stringsAsFactors = FALSE
  ) %>%
    dplyr::left_join(
      taxa_object@variable_info[, c("variable_id", taxonomic_rank), drop = FALSE],
      by = c("taxon_id" = "variable_id")
    ) %>%
    dplyr::arrange(dplyr::desc(.data$prevalence)) %>%
    dplyr::slice_head(n = top_n)
  
  ggplot2::ggplot(
    prevalence_tbl,
    ggplot2::aes(
      x = stats::reorder(.data[[taxonomic_rank]], .data$prevalence),
      y = .data$prevalence
    )
  ) +
    ggplot2::geom_col(fill = "#1b9e77") +
    ggplot2::coord_flip() +
    ggplot2::labs(
      x = taxonomic_rank,
      y = if (proportion) "Prevalence proportion" else "Prevalence",
      title = "Taxon prevalence"
    ) +
    ggplot2::theme_bw()
}


#' Plot Sample Depth
#'
#' Draw sequencing depth per sample.
#'
#' @param object A `microbiome_dataset` object.
#' @param x Sample metadata column shown on the x axis.
#' @param geom Geometry used to display depth.
#'
#' @return A `ggplot` object.
#' @export
#' @examples
#' data("global_patterns", package = "microbiomedataset")
#'
#' plot_sample_depth(global_patterns, x = "SampleType")
plot_sample_depth <- function(object,
                              x = "sample_id",
                              geom = c("bar", "point")) {
  geom <- match.arg(geom)
  sample_info <- extract_sample_info(object)
  depth_tbl <- data.frame(
    sample_id = sample_info$sample_id,
    sample_depth = sample_sums(object),
    stringsAsFactors = FALSE
  ) %>%
    dplyr::left_join(sample_info, by = "sample_id")
  
  p <- ggplot2::ggplot(depth_tbl, ggplot2::aes(x = .data[[x]], y = .data$sample_depth))
  p <- switch(
    geom,
    bar = p + ggplot2::geom_col(fill = "#4c78a8"),
    point = p + ggplot2::geom_point(size = 2.2, color = "#4c78a8")
  )
  p +
    ggplot2::labs(x = x, y = "Sample depth") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}


#' Plot Alpha Rarefaction Curves
#'
#' Draw expected richness curves across sequencing depths.
#'
#' @param object A `microbiome_dataset` object.
#' @param steps Number of depth points used per sample.
#' @param color_by Optional sample metadata column used for color.
#'
#' @return A `ggplot` object.
#' @export
#' @examples
#' data("global_patterns", package = "microbiomedataset")
#'
#' plot_rarefaction(global_patterns, steps = 8, color_by = "SampleType")
plot_rarefaction <- function(object,
                             steps = 10,
                             color_by = NULL) {
  expression_data <- as.matrix(extract_expression_data(object))
  sample_info <- extract_sample_info(object)
  
  rarefaction_tbl <- lapply(seq_len(ncol(expression_data)), function(i) {
    counts <- as.numeric(expression_data[, i])
    counts <- counts[!is.na(counts) & counts > 0]
    sample_id <- colnames(expression_data)[i]
    total_depth <- sum(counts)
    if (length(counts) == 0 || total_depth <= 1) {
      return(NULL)
    }
    depth_grid <- unique(round(seq(1, total_depth, length.out = steps)))
    expected_richness <- vapply(depth_grid, function(m) {
      if (m >= total_depth) {
        return(length(counts))
      }
      sum(1 - exp(lchoose(total_depth - counts, m) - lchoose(total_depth, m)))
    }, numeric(1))
    data.frame(
      sample_id = sample_id,
      depth = depth_grid,
      expected_richness = expected_richness,
      stringsAsFactors = FALSE
    )
  }) %>%
    dplyr::bind_rows() %>%
    dplyr::left_join(sample_info, by = "sample_id")
  
  p <- ggplot2::ggplot(
    rarefaction_tbl,
    ggplot2::aes(x = .data$depth, y = .data$expected_richness, group = .data$sample_id)
  )
  if (!is.null(color_by) && color_by %in% colnames(rarefaction_tbl)) {
    p <- p + ggplot2::geom_line(ggplot2::aes(color = .data[[color_by]]), alpha = 0.75)
  } else {
    p <- p + ggplot2::geom_line(color = "#4c78a8", alpha = 0.5)
  }
  p +
    ggplot2::labs(x = "Sequencing depth", y = "Expected richness", color = color_by) +
    ggplot2::theme_bw()
}
