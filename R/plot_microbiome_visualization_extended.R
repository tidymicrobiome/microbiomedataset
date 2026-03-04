#' Plot Multiple Alpha Diversity Metrics
#'
#' Draw a richness-style summary plot across multiple alpha diversity metrics.
#'
#' @param object A `microbiome_dataset` object.
#' @param metrics Alpha diversity metrics to calculate.
#' @param x Optional sample metadata column used on the x axis.
#' @param color_by Optional sample metadata column used for color.
#'
#' @return A `ggplot` object.
#' @export
#' @examples
#' data("global_patterns", package = "microbiomedataset")
#' plot_richness(global_patterns, x = "SampleType")
plot_richness <- function(object,
                          metrics = c("observed", "shannon", "simpson", "pielou"),
                          x = NULL,
                          color_by = NULL) {
  richness_data <- lapply(metrics, function(metric) {
    result <- calculate_alpha_diversity(object, metric = metric)@result
    result$metric <- metric
    result
  }) %>%
    dplyr::bind_rows()
  
  if (is.null(x)) {
    x <- "metric"
  }
  mapping <- ggplot2::aes(x = .data[[x]], y = .data$value)
  if (!is.null(color_by)) {
    mapping$colour <- rlang::sym(color_by)
  }
  ggplot2::ggplot(richness_data, mapping) +
    ggplot2::geom_boxplot(outlier.shape = NA, alpha = 0.15) +
    ggplot2::geom_jitter(width = 0.15, size = 1.7, alpha = 0.75) +
    ggplot2::facet_wrap(~metric, scales = "free_y") +
    ggplot2::theme_bw() +
    ggplot2::labs(x = x, y = "Diversity")
}


#' Plot an Ordination Biplot
#'
#' Draw an ordination plot with feature loadings.
#'
#' @param object A `microbiome_ordination` or `microbiome_dataset` object.
#' @param method Ordination method used when `object` is a dataset.
#' @param distance_method Distance method used when computing PCoA.
#' @param x,y Axis names. Defaults to the first two axes.
#' @param color_by Optional sample metadata column.
#' @param shape_by Optional sample metadata column.
#' @param top_n_loading Number of feature loadings retained.
#' @param loading_scale Multiplier applied to feature loading vectors.
#'
#' @return A `ggplot` object.
#' @export
#' @examples
#' data("global_patterns", package = "microbiomedataset")
#' plot_ordination_biplot(global_patterns, method = "PCA", color_by = "SampleType")
plot_ordination_biplot <- function(object,
                                   method = c("PCA", "PCoA"),
                                   distance_method = c("bray", "jaccard", "euclidean"),
                                   x = NULL,
                                   y = NULL,
                                   color_by = NULL,
                                   shape_by = NULL,
                                   top_n_loading = 10,
                                   loading_scale = 1) {
  ordination <- resolve_ordination_visualization_input(
    object = object,
    method = match.arg(method),
    distance_method = match.arg(distance_method)
  )
  if (nrow(ordination@feature_loading) > 0) {
    axis_columns <- colnames(ordination@sample_coord)[grepl("^(Axis\\.|PC)", colnames(ordination@sample_coord))]
    if (is.null(x)) {
      x <- axis_columns[1]
    }
    if (is.null(y)) {
      y <- axis_columns[2]
    }
    ordination@feature_loading <- ordination@feature_loading %>%
      dplyr::mutate(score = abs(.data[[x]]) + abs(.data[[y]])) %>%
      dplyr::arrange(dplyr::desc(.data$score)) %>%
      dplyr::slice_head(n = top_n_loading) %>%
      dplyr::select(-.data$score)
  }
  plot_ordination(
    ordination,
    x = x,
    y = y,
    color_by = color_by,
    shape_by = shape_by,
    show_loading = TRUE,
    loading_scale = loading_scale
  )
}


#' Plot an Ordination Scree Plot
#'
#' Show explained variance across ordination axes.
#'
#' @param object A `microbiome_ordination` or `microbiome_dataset` object.
#' @param method Ordination method used when `object` is a dataset.
#' @param distance_method Distance method used when computing PCoA.
#' @param top_n Number of axes displayed.
#'
#' @return A `ggplot` object.
#' @export
#' @examples
#' data("global_patterns", package = "microbiomedataset")
#' plot_scree(run_ordination(global_patterns, method = "PCA"))
plot_scree <- function(object,
                       method = c("PCA", "PCoA"),
                       distance_method = c("bray", "jaccard", "euclidean"),
                       top_n = 10) {
  ordination <- resolve_ordination_visualization_input(
    object = object,
    method = match.arg(method),
    distance_method = match.arg(distance_method)
  )
  eigenvalues <- ordination@eigenvalues
  explained <- if (sum(eigenvalues, na.rm = TRUE) > 0) {
    eigenvalues / sum(eigenvalues, na.rm = TRUE) * 100
  } else {
    rep(NA_real_, length(eigenvalues))
  }
  plot_data <- data.frame(
    axis = seq_along(eigenvalues),
    explained = explained,
    stringsAsFactors = FALSE
  ) %>%
    dplyr::slice_head(n = top_n)
  
  ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$axis, y = .data$explained)) +
    ggplot2::geom_col(fill = "#4c78a8") +
    ggplot2::geom_line(group = 1, color = "#1f1f1f") +
    ggplot2::geom_point(color = "#1f1f1f") +
    ggplot2::labs(x = "Axis", y = "Explained variance (%)") +
    ggplot2::theme_bw()
}


#' Plot a 3D Ordination
#'
#' Draw an interactive 3D ordination plot using `plotly`.
#'
#' @param object A `microbiome_ordination` or `microbiome_dataset` object.
#' @param method Ordination method used when `object` is a dataset.
#' @param distance_method Distance method used when computing PCoA.
#' @param color_by Optional sample metadata column.
#' @param text_by Optional sample metadata column shown on hover.
#'
#' @return A `plotly` htmlwidget.
#' @export
#' @examples
#' data("global_patterns", package = "microbiomedataset")
#' ord <- run_ordination(global_patterns, method = "PCoA", n_axes = 3)
#' plot_ordination_3d(ord, color_by = "SampleType")
plot_ordination_3d <- function(object,
                               method = c("PCoA", "PCA"),
                               distance_method = c("bray", "jaccard", "euclidean"),
                               color_by = NULL,
                               text_by = "sample_id") {
  ordination <- resolve_ordination_visualization_input(
    object = object,
    method = match.arg(method),
    distance_method = match.arg(distance_method),
    n_axes = 3
  )
  coord <- ordination@sample_coord
  axis_columns <- colnames(coord)[grepl("^(Axis\\.|PC)", colnames(coord))]
  if (length(axis_columns) < 3) {
    stop("At least three ordination axes are required for plot_ordination_3d().")
  }
  color_values <- if (!is.null(color_by) && color_by %in% colnames(coord)) coord[[color_by]] else NULL
  hover_text <- if (!is.null(text_by) && text_by %in% colnames(coord)) coord[[text_by]] else coord$sample_id
  plotly::plot_ly(
    data = coord,
    x = coord[[axis_columns[1]]],
    y = coord[[axis_columns[2]]],
    z = coord[[axis_columns[3]]],
    color = color_values,
    text = hover_text,
    type = "scatter3d",
    mode = "markers"
  ) %>%
    plotly::layout(
      scene = list(
        xaxis = list(title = axis_columns[1]),
        yaxis = list(title = axis_columns[2]),
        zaxis = list(title = axis_columns[3])
      )
    )
}


#' Plot Taxa as Boxplots
#'
#' Thin wrapper around [plot_abundance()] with `geom = "boxplot"`.
#'
#' @inheritParams plot_abundance
#' @return A `ggplot` object.
#' @export
#' @examples
#' data("global_patterns", package = "microbiomedataset")
#' plot_taxa_boxplot(global_patterns, taxonomic_rank = "Phylum", x = "SampleType", top_n = 4)
plot_taxa_boxplot <- function(object,
                              taxonomic_rank = c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species"),
                              taxa = NULL,
                              top_n = 10,
                              x = "sample_id",
                              relative = TRUE) {
  plot_abundance(
    object = object,
    taxonomic_rank = match.arg(taxonomic_rank),
    taxa = taxa,
    top_n = top_n,
    x = x,
    geom = "boxplot",
    relative = relative
  )
}


#' Plot Taxa as Violin Plots
#'
#' Thin wrapper around [plot_abundance()] with `geom = "violin"`.
#'
#' @inheritParams plot_abundance
#' @return A `ggplot` object.
#' @export
#' @examples
#' data("global_patterns", package = "microbiomedataset")
#' plot_taxa_violin(global_patterns, taxonomic_rank = "Phylum", x = "SampleType", top_n = 4)
plot_taxa_violin <- function(object,
                             taxonomic_rank = c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species"),
                             taxa = NULL,
                             top_n = 10,
                             x = "sample_id",
                             relative = TRUE) {
  plot_abundance(
    object = object,
    taxonomic_rank = match.arg(taxonomic_rank),
    taxa = taxa,
    top_n = top_n,
    x = x,
    geom = "violin",
    relative = relative
  )
}


#' Plot Abundance Density
#'
#' Plot abundance distributions for dominant taxa.
#'
#' @param object A `microbiome_dataset` object.
#' @param taxonomic_rank Taxonomic rank used for aggregation.
#' @param top_n Number of taxa displayed.
#' @param relative Whether to use relative abundance.
#' @param log10_transform Should abundance values be log10 transformed with a pseudocount?
#'
#' @return A `ggplot` object.
#' @export
#' @examples
#' data("global_patterns", package = "microbiomedataset")
#' plot_abundance_density(global_patterns, taxonomic_rank = "Phylum", top_n = 5)
plot_abundance_density <- function(object,
                                   taxonomic_rank = c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species"),
                                   top_n = 10,
                                   relative = TRUE,
                                   log10_transform = FALSE) {
  taxonomic_rank <- match.arg(taxonomic_rank)
  abundance_data <- select_top_taxa_long(
    object = object,
    taxonomic_rank = taxonomic_rank,
    top_n = top_n,
    relative = relative
  )
  if (log10_transform) {
    abundance_data$abundance <- log10(abundance_data$abundance + 1e-6)
  }
  ggplot2::ggplot(
    abundance_data,
    ggplot2::aes(x = .data$abundance, colour = .data[[taxonomic_rank]], fill = .data[[taxonomic_rank]])
  ) +
    ggplot2::geom_density(alpha = 0.18) +
    ggplot2::theme_bw() +
    ggplot2::labs(
      x = if (log10_transform) "log10 abundance" else if (relative) "Relative abundance (%)" else "Abundance",
      y = "Density",
      colour = taxonomic_rank,
      fill = taxonomic_rank
    )
}


#' Plot a Heatmap with Metadata Annotations
#'
#' Draw a `ComplexHeatmap` object with optional sample and taxa annotations.
#'
#' @param object A `microbiome_dataset` object.
#' @param taxonomic_rank Taxonomic rank used for aggregation.
#' @param top_n Number of taxa displayed.
#' @param relative Whether to use relative abundance.
#' @param sample_annotation Optional sample metadata columns.
#' @param taxa_annotation Optional taxa metadata columns.
#'
#' @return A `ComplexHeatmap::Heatmap` object.
#' @export
#' @examples
#' data("global_patterns", package = "microbiomedataset")
#' plot_heatmap_annotation(
#'   global_patterns,
#'   taxonomic_rank = "Phylum",
#'   top_n = 8,
#'   sample_annotation = "SampleType"
#' )
plot_heatmap_annotation <- function(object,
                                    taxonomic_rank = c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species"),
                                    top_n = 20,
                                    relative = TRUE,
                                    sample_annotation = NULL,
                                    taxa_annotation = NULL) {
  taxonomic_rank <- match.arg(taxonomic_rank)
  heatmap_matrix <- build_taxa_heatmap_matrix(
    object = object,
    taxonomic_rank = taxonomic_rank,
    top_n = top_n,
    relative = relative
  )
  sample_info <- extract_sample_info(object)
  top_ann <- NULL
  if (!is.null(sample_annotation)) {
    ann_df <- sample_info[match(colnames(heatmap_matrix), sample_info$sample_id), sample_annotation, drop = FALSE]
    rownames(ann_df) <- colnames(heatmap_matrix)
    top_ann <- ComplexHeatmap::HeatmapAnnotation(df = ann_df)
  }
  left_ann <- NULL
  if (!is.null(taxa_annotation)) {
    taxa_object <- summarise_taxa(object, taxonomic_rank = taxonomic_rank)
    ann_df <- taxa_object@variable_info[match(rownames(heatmap_matrix), taxa_object@variable_info$variable_id), taxa_annotation, drop = FALSE]
    rownames(ann_df) <- rownames(heatmap_matrix)
    left_ann <- ComplexHeatmap::rowAnnotation(df = ann_df)
  }
  ht <- ComplexHeatmap::Heatmap(
    heatmap_matrix,
    name = if (relative) "relative_abundance" else "abundance",
    top_annotation = top_ann,
    left_annotation = left_ann,
    cluster_rows = TRUE,
    cluster_columns = TRUE,
    column_title = "Samples",
    row_title = taxonomic_rank
  )
  ht
}


#' Plot a Distance Heatmap
#'
#' Visualize sample distances as a `ComplexHeatmap` heatmap.
#'
#' @param object A `microbiome_dataset` or beta-type `microbiome_diversity` object.
#' @param method Distance method used when `object` is a dataset.
#' @param annotate_by Optional sample metadata columns.
#' @param cluster Should samples be hierarchically clustered?
#'
#' @return A `ComplexHeatmap::Heatmap` object.
#' @export
#' @examples
#' data("global_patterns", package = "microbiomedataset")
#' plot_distance_heatmap(global_patterns, annotate_by = "SampleType")
plot_distance_heatmap <- function(object,
                                  method = c("bray", "jaccard", "euclidean"),
                                  annotate_by = NULL,
                                  cluster = TRUE) {
  diversity <- if (methods::is(object, "microbiome_dataset")) {
    calculate_beta_diversity(object, method = match.arg(method))
  } else {
    object
  }
  if (!methods::is(diversity, "microbiome_diversity") || diversity@type != "beta") {
    stop("object must be a microbiome_dataset or beta-type microbiome_diversity.")
  }
  distance_matrix <- as.matrix(diversity@result)
  if (cluster && nrow(distance_matrix) > 1) {
    order_id <- stats::hclust(diversity@result)$labels[stats::hclust(diversity@result)$order]
    distance_matrix <- distance_matrix[order_id, order_id, drop = FALSE]
  }
  top_ann <- NULL
  if (!is.null(annotate_by) && methods::is(object, "microbiome_dataset")) {
    sample_info <- extract_sample_info(object)
    ann_df <- sample_info[match(colnames(distance_matrix), sample_info$sample_id), annotate_by, drop = FALSE]
    rownames(ann_df) <- colnames(distance_matrix)
    top_ann <- ComplexHeatmap::HeatmapAnnotation(df = ann_df)
  }
  ComplexHeatmap::Heatmap(
    distance_matrix,
    name = diversity@method,
    top_annotation = top_ann,
    cluster_rows = cluster,
    cluster_columns = cluster,
    column_title = paste("Beta diversity:", diversity@method)
  )
}


#' Plot Core Taxa
#'
#' Highlight taxa that satisfy prevalence and abundance thresholds.
#'
#' @param object A `microbiome_dataset` object.
#' @param taxonomic_rank Taxonomic rank used for aggregation.
#' @param detection Detection threshold used for prevalence.
#' @param prevalence_cutoff Core prevalence cutoff.
#' @param abundance_cutoff Core mean abundance cutoff.
#' @param relative Whether to use relative abundance.
#' @param top_n Number of taxa displayed.
#'
#' @return A `ggplot` object.
#' @export
#' @examples
#' data("global_patterns", package = "microbiomedataset")
#' x <- prune_taxa(global_patterns, variable_id = global_patterns@variable_info$variable_id[1:200])
#' plot_core_taxa(x, taxonomic_rank = "Phylum", top_n = 8)
plot_core_taxa <- function(object,
                           taxonomic_rank = c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species"),
                           detection = 0,
                           prevalence_cutoff = 0.5,
                           abundance_cutoff = NULL,
                           relative = TRUE,
                           top_n = 20) {
  plot_data <- prevalence_abundance_summary(
    object = object,
    taxonomic_rank = match.arg(taxonomic_rank),
    detection = detection,
    relative = relative,
    top_n = top_n
  )
  if (is.null(abundance_cutoff)) {
    abundance_cutoff <- stats::median(plot_data$mean_abundance, na.rm = TRUE)
  }
  plot_data$core <- plot_data$prevalence >= prevalence_cutoff & plot_data$mean_abundance >= abundance_cutoff
  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = .data$prevalence, y = .data$mean_abundance, color = .data$core, label = .data$taxon)
  ) +
    ggplot2::geom_point(size = 2.3, alpha = 0.85) +
    ggplot2::geom_hline(yintercept = abundance_cutoff, linetype = 2) +
    ggplot2::geom_vline(xintercept = prevalence_cutoff, linetype = 2) +
    ggplot2::geom_text(check_overlap = TRUE, nudge_y = 0.02 * max(plot_data$mean_abundance, na.rm = TRUE)) +
    ggplot2::theme_bw() +
    ggplot2::labs(x = "Prevalence", y = "Mean abundance", color = "Core taxon")
}


#' Plot Prevalence versus Abundance
#'
#' Draw a prevalence-abundance scatter plot for dominant taxa.
#'
#' @inheritParams plot_core_taxa
#' @return A `ggplot` object.
#' @export
#' @examples
#' data("global_patterns", package = "microbiomedataset")
#' x <- prune_taxa(global_patterns, variable_id = global_patterns@variable_info$variable_id[1:200])
#' plot_prevalence_abundance(x, taxonomic_rank = "Phylum", top_n = 8)
plot_prevalence_abundance <- function(object,
                                      taxonomic_rank = c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species"),
                                      detection = 0,
                                      relative = TRUE,
                                      top_n = 20) {
  plot_data <- prevalence_abundance_summary(
    object = object,
    taxonomic_rank = match.arg(taxonomic_rank),
    detection = detection,
    relative = relative,
    top_n = top_n
  )
  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = .data$prevalence, y = .data$mean_abundance, size = .data$mean_abundance, color = .data$taxon)
  ) +
    ggplot2::geom_point(alpha = 0.8) +
    ggplot2::theme_bw() +
    ggplot2::labs(x = "Prevalence", y = "Mean abundance", color = "Taxon", size = "Mean abundance")
}


#' Plot Tree Abundance
#'
#' Thin wrapper around [plot_tree()] with abundance mapping enabled.
#'
#' @inheritParams plot_tree
#' @return A `ggplot` object.
#' @export
#' @examples
#' data("global_patterns", package = "microbiomedataset")
#' x <- prune_taxa(global_patterns, variable_id = global_patterns@variable_info$variable_id[1:120])
#' x <- align_tree(x, tree = "taxa_tree")
#' plot_tree_abundance(x, tree = "taxa_tree", taxonomic_rank = "Phylum")
plot_tree_abundance <- function(object,
                                tree = c("otu_tree", "taxa_tree"),
                                layout = "dendrogram",
                                use_link = TRUE,
                                abundance_method = c("sum", "mean", "median"),
                                taxonomic_rank = c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species"),
                                show_tip_label = TRUE,
                                tip_label_size = 2.5) {
  plot_tree(
    object = object,
    tree = match.arg(tree),
    layout = layout,
    use_link = use_link,
    color_by = "abundance",
    size_by = "abundance",
    abundance_method = match.arg(abundance_method),
    taxonomic_rank = match.arg(taxonomic_rank),
    show_tip_label = show_tip_label,
    tip_label_size = tip_label_size
  )
}


#' Plot a Tree-Ordered Heatmap
#'
#' Draw a heatmap ordered by the tip order of `taxa_tree` or `otu_tree`.
#'
#' @param object A `microbiome_dataset` object.
#' @param tree Tree slot used to derive row order.
#' @param taxonomic_rank Taxonomic rank used when `tree = "taxa_tree"`.
#' @param top_n Number of rows displayed when `tree = "taxa_tree"`.
#' @param relative Whether to use relative abundance.
#'
#' @return A `ComplexHeatmap::Heatmap` object.
#' @export
#' @examples
#' data("global_patterns", package = "microbiomedataset")
#' x <- prune_taxa(global_patterns, variable_id = global_patterns@variable_info$variable_id[1:120])
#' x <- align_tree(x, tree = "taxa_tree")
#' plot_tree_heatmap(x, tree = "taxa_tree", taxonomic_rank = "Phylum")
plot_tree_heatmap <- function(object,
                              tree = c("otu_tree", "taxa_tree"),
                              taxonomic_rank = c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species"),
                              top_n = 20,
                              relative = TRUE) {
  tree <- match.arg(tree)
  taxonomic_rank <- match.arg(taxonomic_rank)
  tree_object <- extract_tree_data(object, tree = tree, data_type = "treedata")
  if (is.null(tree_object)) {
    stop(tree, " is NULL.")
  }
  heatmap_matrix <- if (identical(tree, "otu_tree")) {
    as.matrix(extract_expression_data(object))
  } else {
    build_taxa_heatmap_matrix(object, taxonomic_rank = taxonomic_rank, top_n = top_n, relative = relative)
  }
  row_order <- if (identical(tree, "taxa_tree")) {
    prefix_map <- c(
      Kingdom = "k__",
      Phylum = "p__",
      Class = "c__",
      Order = "o__",
      Family = "f__",
      Genus = "g__",
      Species = "s__"
    )
    rownames(heatmap_matrix) <- paste0(unname(prefix_map[[taxonomic_rank]]), rownames(heatmap_matrix))
    link_table <- extract_tree_link_data(object, tree = "taxa_tree")
    if (!is.null(link_table) && "link_rank" %in% colnames(link_table)) {
      ordered_nodes <- link_table %>%
        dplyr::filter(.data$link_rank == taxonomic_rank) %>%
        dplyr::arrange(.data$node) %>%
        dplyr::pull(.data$node_label) %>%
        unique()
      intersect(ordered_nodes, rownames(heatmap_matrix))
    } else {
      character()
    }
  } else {
    phylo <- tidytree::as.phylo(tree_object)
    intersect(phylo$tip.label, rownames(heatmap_matrix))
  }
  if (length(row_order) == 0) {
    stop("No overlapping tip labels were found between the tree and heatmap matrix.")
  }
  heatmap_matrix <- heatmap_matrix[row_order, , drop = FALSE]
  ComplexHeatmap::Heatmap(
    heatmap_matrix,
    name = if (relative) "relative_abundance" else "abundance",
    cluster_rows = FALSE,
    cluster_columns = TRUE,
    row_title = paste(tree, "order"),
    column_title = "Samples"
  )
}


#' Plot Longitudinal Taxa Trajectories
#'
#' Plot selected taxa across an ordered sample metadata variable.
#'
#' @name time_series_taxa_plot
#' @aliases plot_longitudinal
#'
#' @param object A `microbiome_dataset` object.
#' @param taxonomic_rank Taxonomic rank used for aggregation.
#' @param time_by Sample metadata column used on the x axis.
#' @param id_by Optional metadata column used to connect repeated measures.
#' @param color_by Optional metadata column used for color.
#' @param taxa Optional taxa labels.
#' @param top_n Number of taxa shown when `taxa` is omitted.
#' @param relative Whether to use relative abundance.
#'
#' @return A `ggplot` object.
#' @export
#' @examples
#' data("demo_crossomics", package = "microbiomedataset")
#' plot_longitudinal(
#'   demo_crossomics$microbiome_data,
#'   taxonomic_rank = "Genus",
#'   time_by = "age",
#'   id_by = "subject_id",
#'   top_n = 4
#' )
plot_longitudinal <- function(object,
                              taxonomic_rank = c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species"),
                              time_by,
                              id_by = NULL,
                              color_by = NULL,
                              taxa = NULL,
                              top_n = 6,
                              relative = TRUE) {
  taxonomic_rank <- match.arg(taxonomic_rank)
  abundance_data <- psmelt_taxa(object, taxonomic_rank = taxonomic_rank, relative = relative)
  if (is.null(taxa)) {
    taxa <- abundance_data %>%
      dplyr::group_by(.data[[taxonomic_rank]]) %>%
      dplyr::summarise(mean_abundance = mean(.data$abundance, na.rm = TRUE), .groups = "drop") %>%
      dplyr::arrange(dplyr::desc(.data$mean_abundance)) %>%
      dplyr::slice_head(n = top_n) %>%
      dplyr::pull(.data[[taxonomic_rank]])
  }
  abundance_data <- abundance_data %>%
    dplyr::filter(.data[[taxonomic_rank]] %in% taxa)
  mapping <- ggplot2::aes(x = .data[[time_by]], y = .data$abundance, group = if (!is.null(id_by)) .data[[id_by]] else .data$sample_id)
  if (!is.null(color_by)) {
    mapping$colour <- rlang::sym(color_by)
  } else {
    mapping$colour <- rlang::sym(taxonomic_rank)
  }
  ggplot2::ggplot(abundance_data, mapping) +
    ggplot2::geom_line(alpha = 0.55) +
    ggplot2::geom_point(size = 1.6, alpha = 0.8) +
    ggplot2::facet_wrap(stats::as.formula(paste("~", taxonomic_rank)), scales = "free_y") +
    ggplot2::theme_bw() +
    ggplot2::labs(x = time_by, y = if (relative) "Relative abundance (%)" else "Abundance")
}


#' Plot Taxa Overlap Between Groups
#'
#' Visualize pairwise group overlap in prevalent taxa.
#'
#' @param object A `microbiome_dataset` object.
#' @param group_by Sample metadata column defining the groups.
#' @param taxonomic_rank Taxonomic rank used for aggregation.
#' @param prevalence_cutoff Prevalence cutoff within each group.
#' @param relative Whether to use relative abundance.
#'
#' @return A `ggplot` object.
#' @export
#' @examples
#' data("global_patterns", package = "microbiomedataset")
#' plot_taxa_overlap(global_patterns, group_by = "SampleType", taxonomic_rank = "Phylum")
plot_taxa_overlap <- function(object,
                              group_by,
                              taxonomic_rank = c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species"),
                              prevalence_cutoff = 0.3,
                              relative = TRUE) {
  taxonomic_rank <- match.arg(taxonomic_rank)
  long_data <- psmelt_taxa(object, taxonomic_rank = taxonomic_rank, relative = relative)
  overlap_tbl <- long_data %>%
    dplyr::group_by(.data[[group_by]], .data[[taxonomic_rank]]) %>%
    dplyr::summarise(prevalence = mean(.data$abundance > 0, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(present = .data$prevalence >= prevalence_cutoff) %>%
    dplyr::select(!!rlang::sym(group_by), !!rlang::sym(taxonomic_rank), .data$present) %>%
    tidyr::pivot_wider(names_from = !!rlang::sym(group_by), values_from = .data$present, values_fill = FALSE)
  overlap_matrix <- as.matrix(overlap_tbl[, -1, drop = FALSE])
  groups <- colnames(overlap_matrix)
  jaccard <- outer(groups, groups, Vectorize(function(g1, g2) {
    x <- overlap_matrix[, g1]
    y <- overlap_matrix[, g2]
    union <- sum(x | y)
    if (union == 0) 0 else sum(x & y) / union
  }))
  dimnames(jaccard) <- list(groups, groups)
  plot_data <- as.data.frame(jaccard) %>%
    tibble::rownames_to_column("group_1") %>%
    tidyr::pivot_longer(-.data$group_1, names_to = "group_2", values_to = "overlap")
  ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$group_1, y = .data$group_2, fill = .data$overlap)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient(low = "#f7fbff", high = "#08306b") +
    ggplot2::theme_bw() +
    ggplot2::labs(x = group_by, y = group_by, fill = "Jaccard overlap")
}


#' Plot Prevalence UpSet
#'
#' Draw an UpSet plot of taxa shared across groups.
#'
#' @param object A `microbiome_dataset` object.
#' @param group_by Sample metadata column defining the groups.
#' @param taxonomic_rank Taxonomic rank used for aggregation.
#' @param prevalence_cutoff Prevalence cutoff within each group.
#' @param relative Whether to use relative abundance.
#'
#' @return A `CombinationMatrix`-based UpSet plot object.
#' @export
#' @examples
#' data("global_patterns", package = "microbiomedataset")
#' plot_prevalence_upset(global_patterns, group_by = "SampleType", taxonomic_rank = "Phylum")
plot_prevalence_upset <- function(object,
                                  group_by,
                                  taxonomic_rank = c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species"),
                                  prevalence_cutoff = 0.3,
                                  relative = TRUE) {
  taxonomic_rank <- match.arg(taxonomic_rank)
  long_data <- psmelt_taxa(object, taxonomic_rank = taxonomic_rank, relative = relative)
  comb_df <- long_data %>%
    dplyr::group_by(.data[[group_by]], .data[[taxonomic_rank]]) %>%
    dplyr::summarise(prevalence = mean(.data$abundance > 0, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(present = .data$prevalence >= prevalence_cutoff) %>%
    tidyr::pivot_wider(names_from = !!rlang::sym(group_by), values_from = .data$present, values_fill = FALSE)
  comb_mat <- ComplexHeatmap::make_comb_mat(as.data.frame(comb_df[, -1, drop = FALSE]))
  ComplexHeatmap::UpSet(comb_mat)
}


#' Plot Taxonomy as a Sunburst
#'
#' Draw a sunburst chart from hierarchical taxonomy abundances.
#'
#' @param object A `microbiome_dataset` object.
#' @param ranks Taxonomy ranks used in the sunburst.
#' @param top_n Number of taxa displayed at the deepest rank.
#'
#' @return A `plotly` htmlwidget.
#' @export
#' @examples
#' data("global_patterns", package = "microbiomedataset")
#' plot_taxonomy_sunburst(global_patterns, ranks = c("Kingdom", "Phylum", "Class"))
plot_taxonomy_sunburst <- function(object,
                                   ranks = c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus"),
                                   top_n = 50) {
  variable_info <- normalize_variable_info_schema(extract_variable_info(object))
  abundance <- taxa_sums(object)
  plot_data <- variable_info %>%
    dplyr::mutate(value = abundance[match(.data$variable_id, names(abundance))]) %>%
    dplyr::arrange(dplyr::desc(.data$value)) %>%
    dplyr::slice_head(n = top_n)
  sunburst_tbl <- build_taxonomy_sunburst_table(plot_data, ranks = ranks)
  plotly::plot_ly(
    labels = sunburst_tbl$label,
    parents = sunburst_tbl$parent,
    values = sunburst_tbl$value,
    type = "sunburst",
    branchvalues = "total"
  )
}


#' @keywords internal
resolve_ordination_visualization_input <- function(object,
                                                   method = "PCA",
                                                   distance_method = "bray",
                                                   n_axes = 3) {
  if (methods::is(object, "microbiome_ordination")) {
    return(object)
  }
  if (!methods::is(object, "microbiome_dataset")) {
    stop("object must be a microbiome_dataset or microbiome_ordination.")
  }
  run_ordination(
    object = object,
    method = method,
    distance_method = distance_method,
    n_axes = n_axes
  )
}


#' @keywords internal
select_top_taxa_long <- function(object,
                                 taxonomic_rank,
                                 top_n = 10,
                                 relative = TRUE) {
  abundance_data <- psmelt_taxa(object, taxonomic_rank = taxonomic_rank, relative = relative)
  top_taxa <- abundance_data %>%
    dplyr::group_by(.data[[taxonomic_rank]]) %>%
    dplyr::summarise(mean_abundance = mean(.data$abundance, na.rm = TRUE), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(.data$mean_abundance)) %>%
    dplyr::slice_head(n = top_n) %>%
    dplyr::pull(.data[[taxonomic_rank]])
  abundance_data %>%
    dplyr::filter(.data[[taxonomic_rank]] %in% top_taxa)
}


#' @keywords internal
build_taxa_heatmap_matrix <- function(object,
                                      taxonomic_rank,
                                      top_n = 20,
                                      relative = TRUE) {
  abundance_data <- select_top_taxa_long(
    object = object,
    taxonomic_rank = taxonomic_rank,
    top_n = top_n,
    relative = relative
  )
  heatmap_matrix <- abundance_data %>%
    dplyr::select(sample_id, !!rlang::sym(taxonomic_rank), abundance) %>%
    tidyr::pivot_wider(names_from = sample_id, values_from = abundance, values_fill = 0) %>%
    as.data.frame(check.names = FALSE)
  rownames(heatmap_matrix) <- heatmap_matrix[[taxonomic_rank]]
  heatmap_matrix[[taxonomic_rank]] <- NULL
  as.matrix(heatmap_matrix)
}


#' @keywords internal
prevalence_abundance_summary <- function(object,
                                         taxonomic_rank,
                                         detection = 0,
                                         relative = TRUE,
                                         top_n = 20) {
  taxa_object <- summarise_taxa(object, taxonomic_rank = taxonomic_rank)
  if (relative) {
    taxa_object <- transform_counts(taxa_object, method = "relative")
  }
  plot_data <- data.frame(
    variable_id = taxa_object@variable_info$variable_id,
    prevalence = taxa_prevalence(taxa_object, detection = detection, proportion = TRUE),
    mean_abundance = rowMeans(extract_expression_data(taxa_object), na.rm = TRUE),
    taxon = taxa_object@variable_info[[taxonomic_rank]],
    stringsAsFactors = FALSE
  ) %>%
    dplyr::arrange(dplyr::desc(.data$mean_abundance)) %>%
    dplyr::slice_head(n = top_n)
  plot_data
}


#' @keywords internal
build_taxonomy_sunburst_table <- function(variable_info,
                                          ranks) {
  nodes <- list()
  seen <- new.env(parent = emptyenv())
  for (i in seq_len(nrow(variable_info))) {
    value <- variable_info$value[i]
    parent <- ""
    for (rank in ranks) {
      label <- as.character(variable_info[[rank]][i])
      if (is.na(label) || !nzchar(label)) {
        next
      }
      node_id <- paste(c(parent, label), collapse = "/")
      if (!exists(node_id, envir = seen, inherits = FALSE)) {
        assign(node_id, list(label = label, parent = parent, value = 0), envir = seen)
      }
      node <- get(node_id, envir = seen, inherits = FALSE)
      node$value <- node$value + value
      assign(node_id, node, envir = seen)
      parent <- node_id
    }
  }
  as.data.frame(do.call(rbind, lapply(ls(seen), function(id) {
    node <- get(id, envir = seen, inherits = FALSE)
    data.frame(label = node$label, parent = node$parent, value = node$value, stringsAsFactors = FALSE)
  })), stringsAsFactors = FALSE)
}
