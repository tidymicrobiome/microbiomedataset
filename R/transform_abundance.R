#' Transform Count Data in a Microbiome Dataset
#'
#' @param object A `microbiome_dataset` object.
#' @param method Transformation method.
#' @param pseudocount A pseudocount added before transformations that require
#'   strictly positive values.
#' @param ... Other parameters.
#'
#' @return A transformed `microbiome_dataset` object.
#' @export
#' @examples
#' data("global_patterns", package = "microbiomedataset")
#'
#' x <- transform_counts(global_patterns, method = "relative")
#' attr(x@expression_data, "counts_or_relative")
#'
#' y <- transform_counts(global_patterns, method = "log", pseudocount = 1)
#' attr(y@expression_data, "transform_method")
transform_counts <-
  function(object,
           method = c("relative",
                      "log",
                      "presence_absence",
                      "clr"),
           pseudocount = 1,
           ...) {
    if (methods::is(object, "microbiome_dataset")) {
      return(
        transform_counts.microbiome_dataset(
          object = object,
          method = method,
          pseudocount = pseudocount,
          ...
        )
      )
    }
    stop("transform_counts() currently supports microbiome_dataset objects only.")
  }

#' @method transform_counts microbiome_dataset
#' @rdname transform_counts
#' @importFrom tibble column_to_rownames rownames_to_column
#' @export
transform_counts.microbiome_dataset <-
  function(object,
           method = c("relative",
                      "log",
                      "presence_absence",
                      "clr"),
           pseudocount = 1,
           ...) {
    method <- match.arg(method)
    expression_data <-
      extract_expression_data(object) %>%
      as.matrix()
    
    transformed <-
      switch(
        method,
        relative = {
          totals <- colSums(expression_data, na.rm = TRUE)
          transformed_matrix <- sweep(expression_data, 2, totals, "/")
          zero_totals <- totals == 0
          if (any(zero_totals)) {
            transformed_matrix[, zero_totals] <- 0
          }
          transformed_matrix
        },
        log = log10(expression_data + pseudocount),
        presence_absence = (expression_data > 0) * 1,
        clr = apply(expression_data, 2, function(x) {
          y <- x + pseudocount
          log_y <- log(y)
          log_y - mean(log_y, na.rm = TRUE)
        })
      )
    
    if (is.null(dim(transformed))) {
      transformed <-
        matrix(
          transformed,
          nrow = nrow(expression_data),
          ncol = ncol(expression_data),
          dimnames = dimnames(expression_data)
        )
    } else {
      rownames(transformed) <- rownames(expression_data)
      colnames(transformed) <- colnames(expression_data)
    }
    
    transformed <- as.data.frame(transformed, check.names = FALSE)
    
    if (method == "relative") {
      attr(transformed, "counts_or_relative") <- "relative"
    } else{
      attr(transformed, "counts_or_relative") <- NULL
    }
    attr(transformed, "transform_method") <- method
    
    slot(object = object, name = "expression_data") <-
      transformed
    
    append_process_parameter(
      object = object,
      process_name = "transform_counts",
      function_name = "transform_counts()",
      parameter = list(method = method, pseudocount = pseudocount)
    )
  }


#' @title transform data to relative data
#' @param object mass_dataset, microbiome_dataset or Taxa table
#' @param ... other params
#' @return mass_dataset, microbiome_dataset or Taxa table
#' @export
#' @examples
#' data("global_patterns", package = "microbiomedataset")
#'
#' x <- transform2relative_intensity(global_patterns)
#' attr(x@expression_data, "counts_or_relative")
transform2relative_intensity <-
  function(object,
           ...) {
    UseMethod("transform2relative_intensity")
  }

#' @method transform2relative_intensity microbiome_dataset
#' @rdname transform2relative_intensity
#' @export
transform2relative_intensity.microbiome_dataset <-
  function(object,
           ...) {
    transform_counts(
      object = object,
      method = "relative",
      ...
    )
  }


#' Transform Taxa after Taxonomic Summarization
#'
#' Summarize a `microbiome_dataset` at a chosen taxonomy rank and then apply a
#' count transformation to the aggregated abundance matrix.
#'
#' @param object A `microbiome_dataset` object.
#' @param taxonomic_rank Taxonomic rank used for summarization.
#' @param what Aggregation method used before transformation.
#' @param method Transformation method.
#' @param pseudocount A pseudocount added before transformations that require
#'   strictly positive values.
#' @param na_remove Should taxa with missing rank be removed before
#'   summarization?
#' @param ... Other parameters passed to [transform_counts()].
#'
#' @return A transformed `microbiome_dataset` object.
#' @export
#' @examples
#' data("global_patterns", package = "microbiomedataset")
#'
#' x0 <- prune_taxa(global_patterns, variable_id = global_patterns@variable_info$variable_id[1:200])
#' x <- transform_taxa(
#'   object = x0,
#'   taxonomic_rank = "Genus",
#'   what = "sum_intensity",
#'   method = "relative"
#' )
#' attr(x@expression_data, "transform_method")
transform_taxa <-
  function(object,
           taxonomic_rank = c("Kingdom",
                              "Phylum",
                              "Class",
                              "Order",
                              "Family",
                              "Genus",
                              "Species"),
           what = c("sum_intensity",
                    "mean_intensity",
                    "median_intensity"),
           method = c("relative",
                      "log",
                      "presence_absence",
                      "clr"),
           pseudocount = 1,
           na_remove = TRUE,
           ...) {
    taxonomic_rank <- match.arg(taxonomic_rank)
    what <- match.arg(what)
    method <- match.arg(method)
    
    object <- summarise_taxa(
      object = object,
      taxonomic_rank = taxonomic_rank,
      what = what,
      na_remove = na_remove
    )
    
    transform_counts(
      object = object,
      method = method,
      pseudocount = pseudocount,
      ...
    )
  }


#' Transform Samples after Sample Group Summarization
#'
#' Summarize samples by a grouping column in `sample_info` and then apply a
#' count transformation to the summarized abundance matrix.
#'
#' @param object A `microbiome_dataset` object.
#' @param group_by A column in `sample_info`.
#' @param what Aggregation method used before transformation.
#' @param method Transformation method.
#' @param pseudocount A pseudocount added before transformations that require
#'   strictly positive values.
#' @param ... Other parameters passed to [transform_counts()].
#'
#' @return A transformed `microbiome_dataset` object.
#' @export
#' @examples
#' data("global_patterns", package = "microbiomedataset")
#'
#' x <- transform_samples(
#'   object = global_patterns,
#'   group_by = "SampleType",
#'   what = "sum_intensity",
#'   method = "relative"
#' )
#' attr(x@expression_data, "transform_method")
transform_samples <-
  function(object,
           group_by,
           what = c("sum_intensity",
                    "mean_intensity",
                    "median_intensity"),
           method = c("relative",
                      "log",
                      "presence_absence",
                      "clr"),
           pseudocount = 1,
           ...) {
    what <- match.arg(what)
    method <- match.arg(method)
    
    object <- summarise_samples_by_group(
      object = object,
      group_by = group_by,
      what = what
    )
    
    transform_counts(
      object = object,
      method = method,
      pseudocount = pseudocount,
      ...
    )
  }


#' Transform Abundance by Taxa or Sample Groups
#'
#' Unified front-end for transforming abundance after summarising either taxa or
#' samples.
#'
#' @param object A `microbiome_dataset` object.
#' @param by Transformation target, either `"taxa"` or `"samples"`.
#' @param group_by Taxonomy rank or sample metadata column used for grouping.
#' @param what Aggregation method used before transformation.
#' @param method Transformation method.
#' @param pseudocount A pseudocount added before transformations that require
#'   strictly positive values.
#' @param na_remove Should taxa with missing rank be removed before
#'   summarisation when `by = "taxa"`?
#' @param ... Other parameters passed to [transform_counts()].
#'
#' @return A transformed `microbiome_dataset` object.
#' @export
#' @examples
#' data("global_patterns", package = "microbiomedataset")
#'
#' x0 <- prune_taxa(global_patterns, variable_id = global_patterns@variable_info$variable_id[1:200])
#' x <- transform_abundance_by(
#'   object = x0,
#'   by = "taxa",
#'   group_by = "Genus",
#'   what = "sum_intensity",
#'   method = "relative"
#' )
#' attr(x@expression_data, "transform_method")
transform_abundance_by <-
  function(object,
           by = c("taxa", "samples"),
           group_by,
           what = c("sum_intensity",
                    "mean_intensity",
                    "median_intensity"),
           method = c("relative",
                      "log",
                      "presence_absence",
                      "clr"),
           pseudocount = 1,
           na_remove = TRUE,
           ...) {
    by <- match.arg(by)
    what <- match.arg(what)
    method <- match.arg(method)
    
    if (missing(group_by)) {
      stop("group_by must be provided.")
    }
    
    if (identical(by, "taxa")) {
      return(
        transform_taxa(
          object = object,
          taxonomic_rank = group_by,
          what = what,
          method = method,
          pseudocount = pseudocount,
          na_remove = na_remove,
          ...
        )
      )
    }
    
    transform_samples(
      object = object,
      group_by = group_by,
      what = what,
      method = method,
      pseudocount = pseudocount,
      ...
    )
  }
