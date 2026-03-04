#' Calculate Alpha Diversity
#'
#' Calculate a sample-wise alpha diversity metric from a `microbiome_dataset`.
#'
#' @param object A `microbiome_dataset` object.
#' @param metric Alpha diversity metric.
#'
#' @return A `microbiome_diversity` object.
#' @export
#' @examples
#' data("global_patterns", package = "microbiomedataset")
#'
#' x <- calculate_alpha_diversity(global_patterns, metric = "shannon")
#' class(x)
#' head(x@result)
calculate_alpha_diversity <-
  function(object,
           metric = c("observed", "shannon", "simpson", "pielou")) {
    metric <- match.arg(metric)
    expression_data <- as.matrix(extract_expression_data(object))
    sample_info <- extract_sample_info(object)
    
    result_value <-
      apply(expression_data, 2, function(x) {
        x <- x[!is.na(x)]
        if (length(x) == 0) {
          return(NA_real_)
        }
        observed <- sum(x > 0)
        if (observed == 0) {
          return(0)
        }
        p <- x / sum(x)
        p <- p[p > 0]
        shannon <- -sum(p * log(p))
        simpson <- 1 - sum(p ^ 2)
        switch(
          metric,
          observed = observed,
          shannon = shannon,
          simpson = simpson,
          pielou = if (observed > 1) shannon / log(observed) else NA_real_
        )
      })
    
    result <-
      data.frame(
        sample_id = names(result_value),
        value = as.numeric(result_value),
        stringsAsFactors = FALSE
      ) %>%
      dplyr::left_join(sample_info, by = "sample_id")
    
    new(
      Class = "microbiome_diversity",
      result = result,
      type = "alpha",
      method = metric
    )
  }


#' Calculate Beta Diversity
#'
#' Calculate a sample-wise beta diversity distance matrix from a
#' `microbiome_dataset`.
#'
#' @param object A `microbiome_dataset` object.
#' @param method Distance method.
#'
#' @return A `microbiome_diversity` object.
#' @export
#' @examples
#' data("global_patterns", package = "microbiomedataset")
#'
#' x <- calculate_beta_diversity(global_patterns, method = "bray")
#' class(x)
#' dim(as.matrix(x@result))
calculate_beta_diversity <-
  function(object,
           method = c("bray", "jaccard", "euclidean")) {
    method <- match.arg(method)
    expression_data <- t(as.matrix(extract_expression_data(object)))
    
    distance_result <-
      switch(
        method,
        euclidean = stats::dist(expression_data, method = "euclidean"),
        bray = {
          n <- nrow(expression_data)
          d <- matrix(0, nrow = n, ncol = n)
          for (i in seq_len(n - 1)) {
            for (j in seq(i + 1, n)) {
              numerator <- sum(abs(expression_data[i, ] - expression_data[j, ]), na.rm = TRUE)
              denominator <- sum(expression_data[i, ] + expression_data[j, ], na.rm = TRUE)
              d[i, j] <- numerator / denominator
              d[j, i] <- d[i, j]
            }
          }
          stats::as.dist(d)
        },
        jaccard = {
          binary_data <- (expression_data > 0) * 1
          n <- nrow(binary_data)
          d <- matrix(0, nrow = n, ncol = n)
          for (i in seq_len(n - 1)) {
            for (j in seq(i + 1, n)) {
              intersect_count <- sum(binary_data[i, ] == 1 & binary_data[j, ] == 1, na.rm = TRUE)
              union_count <- sum(binary_data[i, ] == 1 | binary_data[j, ] == 1, na.rm = TRUE)
              d[i, j] <- if (union_count == 0) 0 else 1 - intersect_count / union_count
              d[j, i] <- d[i, j]
            }
          }
          stats::as.dist(d)
        }
      )
    
    attr(distance_result, "Labels") <- rownames(expression_data)
    
    new(
      Class = "microbiome_diversity",
      result = distance_result,
      type = "beta",
      method = method
    )
  }


#' Run Ordination on a Microbiome Dataset
#'
#' Perform a basic ordination analysis and return an objectized result.
#'
#' @param object A `microbiome_dataset` object.
#' @param method Ordination method.
#' @param distance_method Distance method used for PCoA.
#' @param n_axes Number of axes to keep.
#'
#' @return A `microbiome_ordination` object.
#' @export
#' @examples
#' data("global_patterns", package = "microbiomedataset")
#'
#' x <- run_ordination(global_patterns, method = "PCoA")
#' class(x)
#' head(x@sample_coord)
run_ordination <-
  function(object,
           method = c("PCoA", "PCA"),
           distance_method = c("bray", "jaccard", "euclidean"),
           n_axes = 2) {
    method <- match.arg(method)
    distance_method <- match.arg(distance_method)
    sample_info <- extract_sample_info(object)
    
    if (identical(method, "PCA")) {
      expression_data <- t(as.matrix(extract_expression_data(object)))
      keep_columns <- apply(expression_data, 2, stats::sd, na.rm = TRUE) > 0
      expression_data <- expression_data[, keep_columns, drop = FALSE]
      fit <- stats::prcomp(expression_data, center = TRUE, scale. = TRUE)
      coord <- as.data.frame(fit$x[, seq_len(min(n_axes, ncol(fit$x))), drop = FALSE])
      coord$sample_id <- rownames(coord)
      coord <- dplyr::left_join(coord, sample_info, by = "sample_id")
      loading <- as.data.frame(fit$rotation[, seq_len(min(n_axes, ncol(fit$rotation))), drop = FALSE])
      loading$feature_id <- rownames(loading)
      return(
        new(
          Class = "microbiome_ordination",
          sample_coord = coord,
          eigenvalues = fit$sdev ^ 2,
          method = method,
          distance_method = "",
          feature_loading = loading
        )
      )
    }
    
    distance_object <- calculate_beta_diversity(object, method = distance_method)@result
    fit <- ape::pcoa(distance_object)
    vectors <- fit$vectors[, seq_len(min(n_axes, ncol(fit$vectors))), drop = FALSE]
    coord <- as.data.frame(vectors)
    coord$sample_id <- rownames(coord)
    coord <- dplyr::left_join(coord, sample_info, by = "sample_id")
    
    new(
      Class = "microbiome_ordination",
      sample_coord = coord,
      eigenvalues = fit$values$Eigenvalues,
      method = method,
      distance_method = distance_method,
      feature_loading = data.frame()
    )
  }


#' Agglomerate Taxa Using Tree Distances
#'
#' Collapse OTU-level abundance using the topology of `otu_tree`.
#'
#' @param object A `microbiome_dataset` object.
#' @param k Number of clusters used to cut the tree.
#' @param h Height used to cut the tree.
#' @param what Aggregation method.
#'
#' @return A `microbiome_dataset` object.
#' @export
#' @examples
#' data("global_patterns", package = "microbiomedataset")
#'
#' x <- prune_taxa(global_patterns, variable_id = global_patterns@variable_info$variable_id[1:80])
#' x <- microbiomedataset::convert2microbiome_dataset(
#'   microbiomedataset::convert2phyloseq(x)
#' )
#' otu_phylo <- ape::rtree(
#'   n = nrow(x@variable_info),
#'   tip.label = x@variable_info$variable_id
#' )
#' x@otu_tree <- tidytree::treedata(
#'   phylo = otu_phylo,
#'   data = tibble::tibble(node = seq_len(length(otu_phylo$tip.label) + otu_phylo$Nnode))
#' )
#' x <- agglomerate_tree(x, k = 6)
#' dim(x@expression_data)
agglomerate_tree <-
  function(object,
           k,
           h,
           what = c("sum_intensity", "mean_intensity", "median_intensity")) {
    what <- match.arg(what)
    
    if (is.null(object@otu_tree)) {
      stop("object@otu_tree is NULL.")
    }
    if (missing(k) && missing(h)) {
      stop("Either k or h must be provided.")
    }
    
    phylo <- tidytree::as.phylo(object@otu_tree)
    distance_matrix <- ape::cophenetic.phylo(phylo)
    cluster_fit <- stats::hclust(stats::as.dist(distance_matrix), method = "average")
    cluster_group <- if (!missing(k)) {
      stats::cutree(cluster_fit, k = k)
    } else {
      stats::cutree(cluster_fit, h = h)
    }
    
    variable_info <- extract_variable_info(object)
    expression_data <- as.matrix(extract_expression_data(object))
    otu_tree_link <- extract_tree_link_data(object, tree = "otu_tree")
    if (is.null(otu_tree_link)) {
      otu_tree_link <- build_otu_tree_link(variable_info, object@otu_tree)
    }
    if (is.null(otu_tree_link) || nrow(otu_tree_link) == 0) {
      stop("No valid otu_tree_link metadata is available.")
    }
    matched_cluster <- cluster_group[match(otu_tree_link$node_label, names(cluster_group))]
    cluster_ids <- paste0("tree_cluster_", matched_cluster[match(variable_info$variable_id, otu_tree_link$variable_id)])
    keep_index <- !is.na(cluster_ids)
    variable_info <- variable_info[keep_index, , drop = FALSE]
    expression_data <- expression_data[keep_index, , drop = FALSE]
    cluster_ids <- cluster_ids[keep_index]
    
    aggregated_expression <-
      split(seq_along(cluster_ids), cluster_ids) %>%
      lapply(function(idx) {
        apply(expression_data[idx, , drop = FALSE], 2, function(x) {
          calculate(x, what = what, na.rm = TRUE)
        })
      }) %>%
      dplyr::bind_rows() %>%
      as.data.frame()
    rownames(aggregated_expression) <- names(split(seq_along(cluster_ids), cluster_ids))
    colnames(aggregated_expression) <- colnames(expression_data)
    
    taxonomy_columns <- intersect(
      microbiome_taxonomy_ranks(),
      colnames(variable_info)
    )
    aggregated_variable_info <-
      split(variable_info, cluster_ids) %>%
      lapply(function(df) {
        result <- df[1, , drop = FALSE]
        result$variable_id <- unique(cluster_ids[match(df$variable_id, variable_info$variable_id)])[1]
        result$tree_cluster_size <- nrow(df)
        for (column in taxonomy_columns) {
          values <- unique(stats::na.omit(as.character(df[[column]])))
          result[[column]] <- if (length(values) == 1) values else NA_character_
        }
        result
      }) %>%
      dplyr::bind_rows() %>%
      as.data.frame()
    rownames(aggregated_variable_info) <- aggregated_variable_info$variable_id
    
    variable_info_note <- extract_variable_info_note(object)
    if (!"tree_cluster_size" %in% variable_info_note$name) {
      variable_info_note <-
        dplyr::bind_rows(
          variable_info_note,
          data.frame(
            name = "tree_cluster_size",
            meaning = "Number of original variables within each tree cluster",
            check.names = FALSE,
            stringsAsFactors = FALSE
          )
        )
    }
    
    object@expression_data <- aggregated_expression
    object@variable_info <- aggregated_variable_info
    object@variable_info_note <- variable_info_note
    object@taxa_tree <- rebuild_taxa_tree(object@variable_info)
    object@otu_tree <- NULL
    if (methods::.hasSlot(object, "taxa_tree_link")) {
      object@taxa_tree_link <- build_taxa_tree_link(object@variable_info, object@taxa_tree)
    }
    if (methods::.hasSlot(object, "otu_tree_link")) {
      object@otu_tree_link <- NULL
    }
    object@ref_seq <- NULL
    
    append_process_parameter(
      object = object,
      process_name = "agglomerate_tree",
      function_name = "agglomerate_tree()",
      parameter = list(
        k = if (missing(k)) NULL else k,
        h = if (missing(h)) NULL else h,
        what = what
      )
    )
  }


#' Extract Diversity Results
#'
#' Extract the tabular or distance result stored in a `microbiome_diversity`
#' object.
#'
#' @param object A `microbiome_diversity` object.
#'
#' @return A data.frame or `dist` object.
#' @export
#' @examples
#' data("global_patterns", package = "microbiomedataset")
#'
#' x <- calculate_alpha_diversity(global_patterns, metric = "shannon")
#' y <- extract_diversity_result(x)
#' class(y)
extract_diversity_result <-
  function(object) {
    if (!methods::is(object, "microbiome_diversity")) {
      stop("object must be a microbiome_diversity object.")
    }
    object@result
  }


#' Extract Ordination Coordinates
#'
#' Extract sample coordinates from a `microbiome_ordination` object.
#'
#' @param object A `microbiome_ordination` object.
#'
#' @return A data.frame.
#' @export
#' @examples
#' data("global_patterns", package = "microbiomedataset")
#'
#' x <- run_ordination(global_patterns, method = "PCoA")
#' y <- extract_ordination_result(x)
#' head(y)
extract_ordination_result <-
  function(object) {
    if (!methods::is(object, "microbiome_ordination")) {
      stop("object must be a microbiome_ordination object.")
    }
    object@sample_coord
  }


#' Plot Ordination Results
#'
#' Draw a lightweight scatter plot from a `microbiome_ordination` object.
#'
#' @param object A `microbiome_ordination` object.
#' @param x Axis column used for the x-axis.
#' @param y Axis column used for the y-axis.
#' @param color_by Optional column used for point color.
#' @param shape_by Optional column used for point shape.
#' @param label_samples Should sample labels be drawn? Defaults to `FALSE`.
#' @param ellipse_by Optional grouping column used to draw confidence ellipses.
#' @param centroid_by Optional grouping column used to draw group centroids.
#' @param show_loading Should feature loadings be drawn when available?
#' @param loading_scale Multiplier used to scale feature loading arrows.
#'
#' @return A `ggplot` object.
#' @export
#' @examples
#' data("global_patterns", package = "microbiomedataset")
#'
#' ord <- run_ordination(global_patterns, method = "PCoA")
#' p <- plot_ordination(ord, color_by = "SampleType")
#' class(p)
plot_ordination <-
  function(object,
           x = NULL,
           y = NULL,
           color_by = NULL,
           shape_by = NULL,
           label_samples = FALSE,
           ellipse_by = NULL,
           centroid_by = NULL,
           show_loading = FALSE,
           loading_scale = 1) {
    if (!methods::is(object, "microbiome_ordination")) {
      stop("object must be a microbiome_ordination object.")
    }
    
    coord <- object@sample_coord
    axis_columns <- colnames(coord)[grepl("^(Axis\\.|PC)", colnames(coord))]
    if (length(axis_columns) < 2) {
      stop("At least two ordination axes are required for plotting.")
    }
    if (is.null(x)) {
      x <- axis_columns[1]
    }
    if (is.null(y)) {
      y <- axis_columns[2]
    }
    eigenvalues <- object@eigenvalues
    explained <- if (length(eigenvalues) > 0 && sum(eigenvalues, na.rm = TRUE) > 0) {
      eigenvalues / sum(eigenvalues, na.rm = TRUE) * 100
    } else {
      numeric()
    }
    x_index <- match(x, axis_columns)
    y_index <- match(y, axis_columns)
    x_label <- if (!is.na(x_index) && x_index <= length(explained)) {
      paste0(x, " (", round(explained[x_index], 1), "%)")
    } else {
      x
    }
    y_label <- if (!is.na(y_index) && y_index <= length(explained)) {
      paste0(y, " (", round(explained[y_index], 1), "%)")
    } else {
      y
    }
    
    mapping <- ggplot2::aes(x = !!rlang::sym(x), y = !!rlang::sym(y))
    if (!is.null(color_by)) {
      mapping$colour <- rlang::sym(color_by)
    }
    if (!is.null(shape_by)) {
      mapping$shape <- rlang::sym(shape_by)
    }
    
    p <-
      ggplot2::ggplot(coord, mapping) +
      ggplot2::geom_point(size = 2.2, alpha = 0.85) +
      ggplot2::theme_bw() +
      ggplot2::labs(x = x_label, y = y_label)
    
    if (label_samples && "sample_id" %in% colnames(coord)) {
      p <-
        p +
        ggplot2::geom_text(
          ggplot2::aes(label = sample_id),
          vjust = -0.5,
          size = 3
        )
    }
    
    if (!is.null(ellipse_by)) {
      p <-
        p +
        ggplot2::stat_ellipse(
          ggplot2::aes(group = .data[[ellipse_by]], colour = .data[[ellipse_by]]),
          linewidth = 0.5,
          alpha = 0.6
        )
    }
    
    if (!is.null(centroid_by)) {
      centroids <-
        coord %>%
        dplyr::group_by(.data[[centroid_by]]) %>%
        dplyr::summarise(
          x_centroid = mean(.data[[x]], na.rm = TRUE),
          y_centroid = mean(.data[[y]], na.rm = TRUE),
          .groups = "drop"
        )
      colnames(centroids)[1] <- centroid_by
      p <-
        p +
        ggplot2::geom_point(
          data = centroids,
          ggplot2::aes(
            x = x_centroid,
            y = y_centroid,
            colour = .data[[centroid_by]]
          ),
          inherit.aes = FALSE,
          size = 4,
          shape = 4,
          stroke = 1.1
        )
    }
    
    if (show_loading && nrow(object@feature_loading) > 0) {
      loading <- object@feature_loading
      if (all(c(x, y) %in% colnames(loading))) {
        loading[[x]] <- loading[[x]] * loading_scale
        loading[[y]] <- loading[[y]] * loading_scale
        p <-
          p +
          ggplot2::geom_segment(
            data = loading,
            ggplot2::aes(
              x = 0,
              y = 0,
              xend = .data[[x]],
              yend = .data[[y]]
            ),
            inherit.aes = FALSE,
            arrow = ggplot2::arrow(length = grid::unit(0.12, "inches")),
            linewidth = 0.3,
            alpha = 0.7
          ) +
          ggplot2::geom_text(
            data = loading,
            ggplot2::aes(
              x = .data[[x]],
              y = .data[[y]],
              label = feature_id
            ),
            inherit.aes = FALSE,
            size = 2.5,
            vjust = -0.4
          )
      }
    }
    
    p
  }


#' Plot Alpha Diversity
#'
#' Draw a lightweight plot for alpha diversity results.
#'
#' @param object A `microbiome_diversity` object with `type = "alpha"`.
#' @param x Optional metadata column used on the x-axis.
#' @param color_by Optional metadata column used for point color.
#'
#' @return A `ggplot` object.
#' @export
#' @examples
#' data("global_patterns", package = "microbiomedataset")
#'
#' x <- calculate_alpha_diversity(global_patterns, metric = "shannon")
#' p <- plot_alpha_diversity(x, x = "SampleType")
#' class(p)
plot_alpha_diversity <-
  function(object,
           x = NULL,
           color_by = NULL) {
    if (!methods::is(object, "microbiome_diversity") || object@type != "alpha") {
      stop("object must be an alpha-type microbiome_diversity object.")
    }
    result <- as.data.frame(object@result)
    if (is.null(x)) {
      result$.alpha_metric <- object@method
      x <- ".alpha_metric"
    }
    mapping <- ggplot2::aes(x = !!rlang::sym(x), y = value)
    if (!is.null(color_by)) {
      mapping$colour <- rlang::sym(color_by)
    }
    ggplot2::ggplot(result, mapping) +
      ggplot2::geom_boxplot(outlier.shape = NA, alpha = 0.15) +
      ggplot2::geom_jitter(width = 0.15, size = 2, alpha = 0.8) +
      ggplot2::theme_bw()
  }


#' Plot Beta Diversity
#'
#' Draw a heatmap-style plot from beta diversity distance results.
#'
#' @param object A `microbiome_diversity` object with `type = "beta"`.
#' @param annotate_by Optional named character vector used to annotate samples.
#'   Names must be sample identifiers.
#' @param cluster Should samples be reordered by hierarchical clustering?
#'
#' @return A `ggplot` object.
#' @export
#' @examples
#' data("global_patterns", package = "microbiomedataset")
#'
#' x <- calculate_beta_diversity(global_patterns, method = "bray")
#' p <- plot_beta_diversity(x)
#' class(p)
plot_beta_diversity <-
  function(object,
           annotate_by = NULL,
           cluster = FALSE) {
    if (!methods::is(object, "microbiome_diversity") || object@type != "beta") {
      stop("object must be a beta-type microbiome_diversity object.")
    }
    distance_matrix <- as.matrix(object@result)
    
    if (cluster) {
      hc <- stats::hclust(object@result)
      order_id <- hc$labels[hc$order]
      distance_matrix <- distance_matrix[order_id, order_id, drop = FALSE]
    }
    
    distance_table <-
      distance_matrix %>%
      as.data.frame(check.names = FALSE) %>%
      tibble::rownames_to_column(var = "sample_id") %>%
      tidyr::pivot_longer(
        cols = -sample_id,
        names_to = "sample_id2",
        values_to = "distance"
      )
    
    if (!is.null(annotate_by)) {
      sample_annotation <- data.frame(
        sample_id = rownames(distance_matrix),
        annotation = annotate_by[match(rownames(distance_matrix), names(annotate_by))],
        stringsAsFactors = FALSE
      )
      distance_table <-
        distance_table %>%
        dplyr::left_join(sample_annotation, by = "sample_id")
    }
    
    p <- ggplot2::ggplot(
      distance_table,
      ggplot2::aes(x = sample_id, y = sample_id2, fill = distance)
    ) +
      ggplot2::geom_tile() +
      ggplot2::scale_fill_gradient(low = "#f4f1de", high = "#bc4749") +
      ggplot2::theme_bw() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5)
      )
    
    if (!is.null(annotate_by)) {
      p <- p + ggplot2::facet_grid(. ~ annotation, scales = "free_x", space = "free_x")
    }
    
    p
  }


#' Convert Diversity Results to a Tibble
#'
#' Convert a `microbiome_diversity` result to a tibble for tidy analysis.
#'
#' @param object A `microbiome_diversity` object.
#'
#' @return A tibble.
#' @export
#' @examples
#' data("global_patterns", package = "microbiomedataset")
#'
#' x <- calculate_alpha_diversity(global_patterns, metric = "shannon")
#' y <- as_tibble_diversity(x)
#' class(y)
as_tibble_diversity <-
  function(object) {
    if (!methods::is(object, "microbiome_diversity")) {
      stop("object must be a microbiome_diversity object.")
    }
    result <- object@result
    if (inherits(result, "dist")) {
      as.matrix(result) %>%
        as.data.frame(check.names = FALSE) %>%
        tibble::rownames_to_column(var = "sample_id") %>%
        tibble::as_tibble()
    } else {
      tibble::as_tibble(result)
    }
  }


#' Convert Ordination Results to a Tibble
#'
#' Convert a `microbiome_ordination` result to a tibble for tidy analysis.
#'
#' @param object A `microbiome_ordination` object.
#'
#' @return A tibble.
#' @export
#' @examples
#' data("global_patterns", package = "microbiomedataset")
#'
#' x <- run_ordination(global_patterns, method = "PCoA")
#' y <- as_tibble_ordination(x)
#' class(y)
as_tibble_ordination <-
  function(object) {
    if (!methods::is(object, "microbiome_ordination")) {
      stop("object must be a microbiome_ordination object.")
    }
    tibble::as_tibble(object@sample_coord)
  }
