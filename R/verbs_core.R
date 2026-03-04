#' Prune Samples from a Microbiome Dataset
#'
#' Remove samples from a `microbiome_dataset` while keeping
#' `expression_data` and `sample_info` synchronized.
#'
#' @param object A `microbiome_dataset` object.
#' @param sample_id Sample identifiers to keep or remove.
#' @param sample_index Sample indices to keep or remove.
#' @param keep Should the provided samples be kept? Defaults to `TRUE`.
#'
#' @return A `microbiome_dataset` object.
#' @importFrom dplyr filter
#' @examples
#' data("global_patterns", package = "microbiomedataset")
#'
#' keep_samples <- global_patterns@sample_info$sample_id[1:3]
#' x <- prune_samples(global_patterns, sample_id = keep_samples)
#' dim(x@expression_data)
#' @export
prune_samples <-
  function(object,
           sample_id,
           sample_index,
           keep = TRUE) {
    if (missing(sample_id) && missing(sample_index)) {
      return(object)
    }
    
    sample_info <- extract_sample_info(object)
    sample_ids <- as.character(sample_info$sample_id)
    
    keep_index <- rep(FALSE, length(sample_ids))
    
    if (!missing(sample_id)) {
      keep_index <- keep_index | sample_ids %in% as.character(sample_id)
    }
    
    if (!missing(sample_index)) {
      keep_index[sample_index[sample_index %in% seq_along(sample_ids)]] <- TRUE
    }
    
    if (!keep) {
      keep_index <- !keep_index
    }
    
    keep_ids <- sample_ids[keep_index]
    object@sample_info <- sample_info[match(keep_ids, sample_ids), , drop = FALSE]
    object@expression_data <-
      object@expression_data[, keep_ids, drop = FALSE]
    
    append_process_parameter(
      object = object,
      process_name = "prune_samples",
      function_name = "prune_samples()",
      parameter = list(sample_id = keep_ids, keep = keep)
    )
  }


#' Prune Taxa from a Microbiome Dataset
#'
#' Remove taxa from a `microbiome_dataset` while keeping
#' `expression_data`, `variable_info`, `otu_tree`, `taxa_tree`, and `ref_seq`
#' synchronized.
#'
#' @param object A `microbiome_dataset` object.
#' @param variable_id Variable identifiers to keep or remove.
#' @param variable_index Variable indices to keep or remove.
#' @param keep Should the provided taxa be kept? Defaults to `TRUE`.
#'
#' @return A `microbiome_dataset` object.
#' @importFrom dplyr filter
#' @importFrom methods .hasSlot
#' @examples
#' data("global_patterns", package = "microbiomedataset")
#'
#' keep_taxa <- global_patterns@variable_info$variable_id[1:5]
#' x <- prune_taxa(global_patterns, variable_id = keep_taxa)
#' dim(x@expression_data)
#' @export
prune_taxa <-
  function(object,
           variable_id,
           variable_index,
           keep = TRUE) {
    if (missing(variable_id) && missing(variable_index)) {
      return(object)
    }
    
    variable_info <- extract_variable_info(object)
    variable_ids <- as.character(variable_info$variable_id)
    
    keep_index <- rep(FALSE, length(variable_ids))
    
    if (!missing(variable_id)) {
      keep_index <- keep_index | variable_ids %in% as.character(variable_id)
    }
    
    if (!missing(variable_index)) {
      keep_index[variable_index[variable_index %in% seq_along(variable_ids)]] <- TRUE
    }
    
    if (!keep) {
      keep_index <- !keep_index
    }
    
    keep_ids <- variable_ids[keep_index]
    object@variable_info <- variable_info[match(keep_ids, variable_ids), , drop = FALSE]
    object@expression_data <-
      object@expression_data[keep_ids, , drop = FALSE]
    
    if (methods::.hasSlot(object, "annotation_table") &&
        nrow(object@annotation_table) > 0 &&
        "variable_id" %in% colnames(object@annotation_table)) {
      object@annotation_table <-
        object@annotation_table %>%
        dplyr::filter(variable_id %in% keep_ids)
    }
    
    object@otu_tree <- prune_otu_tree(object@otu_tree, keep_ids)
    object@taxa_tree <- rebuild_taxa_tree(object@variable_info)
    object@otu_tree_link <- build_otu_tree_link(object@variable_info, object@otu_tree)
    object@taxa_tree_link <- build_taxa_tree_link(object@variable_info, object@taxa_tree)
    object@ref_seq <- subset_ref_seq(object@ref_seq, keep_ids)
    
    append_process_parameter(
      object = object,
      process_name = "prune_taxa",
      function_name = "prune_taxa()",
      parameter = list(variable_id = keep_ids, keep = keep)
    )
  }


#' Agglomerate Taxa to a Taxonomic Rank
#'
#' Collapse taxa in a `microbiome_dataset` to a taxonomy rank.
#'
#' @param object A `microbiome_dataset` object.
#' @param taxonomic_rank A taxonomy rank in `variable_info`.
#' @param what Aggregation method.
#' @param na_remove Should taxa with missing rank be removed before agglomeration?
#'
#' @return A `microbiome_dataset` object.
#' @examples
#' data("global_patterns", package = "microbiomedataset")
#'
#' x <- prune_taxa(global_patterns, variable_id = global_patterns@variable_info$variable_id[1:200])
#' x <- agglomerate_taxa(
#'   object = x,
#'   taxonomic_rank = "Genus",
#'   what = "sum_intensity"
#' )
#' dim(x@expression_data)
#' @export
agglomerate_taxa <-
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
           na_remove = TRUE) {
    taxonomic_rank <- match.arg(taxonomic_rank)
    what <- match.arg(what)
    
    if (!taxonomic_rank %in% colnames(object@variable_info)) {
      stop(taxonomic_rank, " must be in variable_info.")
    }
    
    if (na_remove) {
      object <- prune_taxa(
        object = object,
        variable_id = object@variable_info$variable_id[!is.na(object@variable_info[[taxonomic_rank]])]
      )
    }
    
    object <- summarise_variables(
      object = object,
      what = what,
      group_by = taxonomic_rank
    )
    
    object@variable_info$variable_id <- as.character(object@variable_info[[taxonomic_rank]])
    rownames(object@expression_data) <- object@variable_info$variable_id
    
    object@taxa_tree <- rebuild_taxa_tree(object@variable_info)
    object@otu_tree <- NULL
    object@otu_tree_link <- NULL
    object@taxa_tree_link <- build_taxa_tree_link(object@variable_info, object@taxa_tree)
    object@ref_seq <- NULL
    
    append_process_parameter(
      object = object,
      process_name = "agglomerate_taxa",
      function_name = "agglomerate_taxa()",
      parameter = list(
        taxonomic_rank = taxonomic_rank,
        what = what,
        na_remove = na_remove
      )
    )
  }


#' Summarise Taxa at a Taxonomic Rank
#'
#' Microbiome-oriented wrapper around [agglomerate_taxa()] that summarizes
#' abundance at a chosen taxonomy rank.
#'
#' @inheritParams agglomerate_taxa
#'
#' @return A `microbiome_dataset` object.
#' @export
#' @examples
#' data("global_patterns", package = "microbiomedataset")
#'
#' x0 <- prune_taxa(global_patterns, variable_id = global_patterns@variable_info$variable_id[1:200])
#' x <- summarise_taxa(
#'   object = x0,
#'   taxonomic_rank = "Family",
#'   what = "sum_intensity"
#' )
#' dim(x@expression_data)
summarise_taxa <-
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
           na_remove = TRUE) {
    agglomerate_taxa(
      object = object,
      taxonomic_rank = taxonomic_rank,
      what = what,
      na_remove = na_remove
    )
  }


#' Summarise Abundance by Taxa or Sample Groups
#'
#' Unified front-end for summarising abundance either across taxonomy ranks or
#' sample metadata groups.
#'
#' @param object A `microbiome_dataset` object.
#' @param by Summarisation target, either `"taxa"` or `"samples"`.
#' @param group_by Taxonomy rank or sample metadata column used for grouping.
#' @param what Aggregation method.
#' @param na_remove Should taxa with missing rank be removed before
#'   summarisation when `by = "taxa"`?
#'
#' @return A `microbiome_dataset` object.
#' @export
#' @examples
#' data("global_patterns", package = "microbiomedataset")
#'
#' x0 <- prune_taxa(global_patterns, variable_id = global_patterns@variable_info$variable_id[1:200])
#' x <- summarise_abundance(
#'   object = x0,
#'   by = "taxa",
#'   group_by = "Genus",
#'   what = "sum_intensity"
#' )
#' dim(x@expression_data)
#'
#' y <- summarise_abundance(
#'   object = x0,
#'   by = "samples",
#'   group_by = "SampleType",
#'   what = "sum_intensity"
#' )
#' dim(y@expression_data)
summarise_abundance <-
  function(object,
           by = c("taxa", "samples"),
           group_by,
           what = c("sum_intensity",
                    "mean_intensity",
                    "median_intensity"),
           na_remove = TRUE) {
    by <- match.arg(by)
    what <- match.arg(what)
    
    if (missing(group_by)) {
      stop("group_by must be provided.")
    }
    
    if (identical(by, "taxa")) {
      return(
        summarise_taxa(
          object = object,
          taxonomic_rank = group_by,
          what = what,
          na_remove = na_remove
        )
      )
    }
    
    summarise_samples_by_group(
      object = object,
      group_by = group_by,
      what = what
    )
  }


#' Merge Samples by a Sample Metadata Column
#'
#' Merge samples in a `microbiome_dataset` using a column in `sample_info`.
#'
#' @param object A `microbiome_dataset` object.
#' @param group_by A column in `sample_info`.
#' @param what Aggregation method.
#'
#' @return A `microbiome_dataset` object.
#' @examples
#' data("global_patterns", package = "microbiomedataset")
#'
#' x <- merge_samples(
#'   object = global_patterns,
#'   group_by = "SampleType",
#'   what = "sum_intensity"
#' )
#' dim(x@expression_data)
#' @export
merge_samples <-
  function(object,
           group_by,
           what = c("sum_intensity",
                    "mean_intensity",
                    "median_intensity")) {
    what <- match.arg(what)
    
    object <- summarise_samples(
      object = object,
      what = what,
      group_by = group_by
    )
    
    append_process_parameter(
      object = object,
      process_name = "merge_samples",
      function_name = "merge_samples()",
      parameter = list(group_by = group_by, what = what)
    )
  }


#' Prune a Tree Slot in a Microbiome Dataset
#'
#' Keep or remove selected tips from `otu_tree` or `taxa_tree` without changing
#' the abundance matrix.
#'
#' @param object A `microbiome_dataset` object.
#' @param tree Tree slot to prune, either `"otu_tree"` or `"taxa_tree"`.
#' @param tip_label Tip labels to keep or remove.
#' @param keep Should the provided tips be kept? Defaults to `TRUE`.
#'
#' @return A `microbiome_dataset` object.
#' @export
#' @examples
#' data("global_patterns", package = "microbiomedataset")
#'
#' x <- prune_taxa(global_patterns, variable_id = global_patterns@variable_info$variable_id[1:120])
#' x <- microbiomedataset::convert2microbiome_dataset(
#'   microbiomedataset::convert2phyloseq(x)
#' )
#' keep_tips <- x@variable_info$variable_id[1:5]
#' x <- prune_tree(x, tree = "otu_tree", tip_label = keep_tips)
#' class(x@otu_tree)
prune_tree <-
  function(object,
           tree = c("otu_tree", "taxa_tree"),
           tip_label,
           keep = TRUE) {
    tree <- match.arg(tree)
    
    if (missing(tip_label)) {
      stop("tip_label must be provided.")
    }
    
    slot(object, tree) <- prune_treedata_tips(
      tree = slot(object, tree),
      tip_label = tip_label,
      keep = keep
    )
    
    if (identical(tree, "otu_tree")) {
      object@otu_tree_link <- subset_tree_link(
        build_otu_tree_link(object@variable_info, object@otu_tree),
        variable_id = object@variable_info$variable_id
      )
    } else {
      object@taxa_tree_link <- subset_tree_link(
        build_taxa_tree_link(object@variable_info, object@taxa_tree),
        variable_id = object@variable_info$variable_id
      )
    }
    
    append_process_parameter(
      object = object,
      process_name = "prune_tree",
      function_name = "prune_tree()",
      parameter = list(
        tree = tree,
        tip_label = as.character(tip_label),
        keep = keep
      )
    )
  }


#' Align a Tree Slot with the Current Dataset
#'
#' Synchronize `otu_tree` or `taxa_tree` with the current `variable_info`.
#'
#' @param object A `microbiome_dataset` object.
#' @param tree Tree slot to align, either `"otu_tree"` or `"taxa_tree"`.
#' @param drop_unmatched Should unmatched OTU tips be dropped when aligning the
#'   OTU tree?
#'
#' @return A `microbiome_dataset` object.
#' @export
#' @examples
#' data("global_patterns", package = "microbiomedataset")
#'
#' x <- prune_taxa(global_patterns, variable_id = global_patterns@variable_info$variable_id[1:120])
#' x <- microbiomedataset::convert2microbiome_dataset(
#'   microbiomedataset::convert2phyloseq(x)
#' )
#' x <- align_tree(x, tree = "taxa_tree")
#' class(x@taxa_tree)
align_tree <-
  function(object,
           tree = c("otu_tree", "taxa_tree"),
           drop_unmatched = TRUE) {
    tree <- match.arg(tree)
    
    if (identical(tree, "otu_tree")) {
      if (drop_unmatched) {
        object@otu_tree <- prune_otu_tree(
          tree = object@otu_tree,
          variable_id = object@variable_info$variable_id
        )
      }
      object@otu_tree_link <- build_otu_tree_link(object@variable_info, object@otu_tree)
    } else {
      object@taxa_tree <- rebuild_taxa_tree(object@variable_info)
      object@taxa_tree_link <- build_taxa_tree_link(object@variable_info, object@taxa_tree)
    }
    
    append_process_parameter(
      object = object,
      process_name = "align_tree",
      function_name = "align_tree()",
      parameter = list(
        tree = tree,
        drop_unmatched = drop_unmatched
      )
    )
  }


#' Filter the Taxa Tree by a Taxonomic Rank
#'
#' Rebuild `taxa_tree` using only taxa that match selected labels at a chosen
#' taxonomy rank, without changing the abundance matrix.
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
#' x0 <- prune_taxa(global_patterns, variable_id = global_patterns@variable_info$variable_id[1:120])
#' x <- microbiomedataset::convert2microbiome_dataset(
#'   microbiomedataset::convert2phyloseq(x0)
#' )
#' x <- filter_tree_by_rank(
#'   x,
#'   taxonomic_rank = "Phylum",
#'   taxa = c("Bacteroidetes", "Firmicutes")
#' )
#' class(x@taxa_tree)
filter_tree_by_rank <-
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
    keep_index <- rank_values %in% as.character(taxa)
    
    if (na_keep) {
      keep_index[is.na(rank_values)] <- TRUE
    }
    
    if (!keep) {
      keep_index <- !keep_index
    }
    
    object@taxa_tree <- rebuild_taxa_tree(variable_info[keep_index, , drop = FALSE])
    object@taxa_tree_link <- build_taxa_tree_link(object@variable_info, object@taxa_tree)
    
    append_process_parameter(
      object = object,
      process_name = "filter_tree_by_rank",
      function_name = "filter_tree_by_rank()",
      parameter = list(
        taxonomic_rank = taxonomic_rank,
        taxa = as.character(taxa),
        keep = keep,
        na_keep = na_keep
      )
    )
  }


#' Replace a Tree Slot in a Microbiome Dataset
#'
#' Set `otu_tree` or `taxa_tree` on a `microbiome_dataset` and optionally align
#' it with the current dataset.
#'
#' @param object A `microbiome_dataset` object.
#' @param tree Tree slot to replace, either `"otu_tree"` or `"taxa_tree"`.
#' @param value A `treedata` object or `NULL`.
#' @param align Should the tree be aligned after replacement? Defaults to
#'   `TRUE`.
#'
#' @return A `microbiome_dataset` object.
#' @export
#' @examples
#' data("global_patterns", package = "microbiomedataset")
#'
#' x0 <- prune_taxa(global_patterns, variable_id = global_patterns@variable_info$variable_id[1:80])
#' x <- microbiomedataset::convert2microbiome_dataset(
#'   microbiomedataset::convert2phyloseq(x0)
#' )
#' otu_phylo <- ape::rtree(
#'   n = nrow(x@variable_info),
#'   tip.label = x@variable_info$variable_id
#' )
#' x <- replace_tree(
#'   x,
#'   tree = "otu_tree",
#'   value = tidytree::treedata(
#'     phylo = otu_phylo,
#'     data = tibble::tibble(node = seq_len(length(otu_phylo$tip.label) + otu_phylo$Nnode))
#'   )
#' )
#' class(x@otu_tree)
replace_tree <-
  function(object,
           tree = c("otu_tree", "taxa_tree"),
           value = NULL,
           align = TRUE) {
    tree <- match.arg(tree)
    
    if (!is.null(value) && !methods::is(value, "treedata")) {
      stop("value must be a tidytree::treedata object or NULL.")
    }
    
    slot(object, tree) <- value
    if (identical(tree, "otu_tree")) {
      object@otu_tree_link <- build_otu_tree_link(object@variable_info, object@otu_tree)
    } else {
      object@taxa_tree_link <- build_taxa_tree_link(object@variable_info, object@taxa_tree)
    }
    
    if (align) {
      if (identical(tree, "otu_tree")) {
        object <- align_tree(object, tree = "otu_tree")
      } else if (is.null(value)) {
        object@taxa_tree <- rebuild_taxa_tree(object@variable_info)
        object@taxa_tree_link <- build_taxa_tree_link(object@variable_info, object@taxa_tree)
      }
    }
    
    append_process_parameter(
      object = object,
      process_name = "replace_tree",
      function_name = "replace_tree()",
      parameter = list(
        tree = tree,
        align = align,
        is_null = is.null(value)
      )
    )
  }
