


#' @title extract_otu_tree
#' @description Extract OTU tree.
#' @rdname extract-microbiome_dataset
#' @docType methods
#' @author Xiaotao Shen
#' \email{xiaotao.shen@@outlook.com}
#' @param object (required) microbiome_dataset class object.
#' @return A treedata
#' @export
#' @examples
#' data("global_patterns", package = "microbiomedataset")
#'
#' otu_tree <- extract_otu_tree(global_patterns)
#' class(otu_tree)

extract_otu_tree <-
  function(object) {
    otu_tree <- object@otu_tree
    otu_tree
  }


#' @title extract_taxa_tree
#' @description Extract Taxa tree.
#' @rdname extract-microbiome_dataset
#' @docType methods
#' @author Xiaotao Shen
#' \email{xiaotao.shen@@outlook.com}
#' @param object (required) microbiome_dataset class object.
#' @return A treedata
#' @export
#' @examples
#' data("global_patterns", package = "microbiomedataset")
#'
#' taxa_tree <- extract_taxa_tree(global_patterns)
#' class(taxa_tree)

extract_taxa_tree <-
  function(object) {
    taxa_tree <- object@taxa_tree
    taxa_tree
  }



#' @title extract_ref_seq
#' @description Extract ref sequence.
#' @rdname extract-microbiome_dataset
#' @docType methods
#' @author Xiaotao Shen
#' \email{xiaotao.shen@@outlook.com}
#' @param object (required) microbiome_dataset class object.
#' @return A xstringset
#' @export
#' @examples
#' data("global_patterns", package = "microbiomedataset")
#'
#' ref_seq <- extract_ref_seq(global_patterns)
#' class(ref_seq)

extract_ref_seq <-
  function(object) {
    ref_seq <- object@ref_seq
    ref_seq
  }


#' Extract Tree Link Metadata
#'
#' Return the explicit mapping table between `variable_id` and nodes in
#' `otu_tree` or `taxa_tree`.
#'
#' @param object A `microbiome_dataset` object.
#' @param tree Tree slot, either `"otu_tree"` or `"taxa_tree"`.
#'
#' @return A data frame or `NULL`.
#' @export
#' @examples
#' data("global_patterns", package = "microbiomedataset")
#'
#' x <- prune_taxa(global_patterns, variable_id = global_patterns@variable_info$variable_id[1:120])
#' x <- microbiomedataset::convert2microbiome_dataset(
#'   microbiomedataset::convert2phyloseq(x)
#' )
#' extract_tree_link_data(x, tree = "taxa_tree")[1:6, ]
extract_tree_link_data <-
  function(object,
           tree = c("otu_tree", "taxa_tree")) {
    tree <- match.arg(tree)
    if (identical(tree, "otu_tree")) {
      if (methods::.hasSlot(object, "otu_tree_link")) object@otu_tree_link else NULL
    } else {
      if (methods::.hasSlot(object, "taxa_tree_link")) object@taxa_tree_link else NULL
    }
  }


#' Extract Tree Data from a Microbiome Dataset
#'
#' Export `otu_tree` or `taxa_tree` from a `microbiome_dataset` in the desired
#' representation.
#'
#' @param object A `microbiome_dataset` object.
#' @param tree Tree slot to extract, either `"otu_tree"` or `"taxa_tree"`.
#' @param data_type Output type, one of `"treedata"`, `"phylo"`, or `"table"`.
#'
#' @return A tree object or data frame depending on `data_type`.
#' @export
#' @examples
#' data("global_patterns", package = "microbiomedataset")
#'
#' x <- prune_taxa(global_patterns, variable_id = global_patterns@variable_info$variable_id[1:120])
#' x <- microbiomedataset::convert2microbiome_dataset(
#'   microbiomedataset::convert2phyloseq(x)
#' )
#' tree_tbl <- extract_tree_data(x, tree = "taxa_tree", data_type = "table")
#' head(tree_tbl)
extract_tree_data <-
  function(object,
           tree = c("otu_tree", "taxa_tree"),
           data_type = c("treedata", "phylo", "table")) {
    tree <- match.arg(tree)
    data_type <- match.arg(data_type)
    tree_object <- slot(object, tree)
    
    if (is.null(tree_object)) {
      return(NULL)
    }
    
    switch(
      data_type,
      treedata = tree_object,
      phylo = tidytree::as.phylo(tree_object),
      table = as.data.frame(tidytree::as_tibble(tree_object))
    )
  }


#' Melt a Tree Slot into a Tidy Table
#'
#' Convert `otu_tree` or `taxa_tree` into a tidy table for inspection,
#' visualisation, or downstream joins.
#'
#' @param object A `microbiome_dataset` object.
#' @param tree Tree slot to melt, either `"otu_tree"` or `"taxa_tree"`.
#' @param include_tree Should the output include a `tree` column? Defaults to
#'   `TRUE`.
#'
#' @return A data frame.
#' @export
#' @examples
#' data("global_patterns", package = "microbiomedataset")
#'
#' x <- prune_taxa(global_patterns, variable_id = global_patterns@variable_info$variable_id[1:120])
#' x <- microbiomedataset::convert2microbiome_dataset(
#'   microbiomedataset::convert2phyloseq(x)
#' )
#' tree_tbl <- melt_tree(x, tree = "taxa_tree")
#' head(tree_tbl)
melt_tree <-
  function(object,
           tree = c("otu_tree", "taxa_tree"),
           include_tree = TRUE) {
    tree <- match.arg(tree)
    result <- extract_tree_data(
      object = object,
      tree = tree,
      data_type = "table"
    )
    
    if (is.null(result)) {
      return(NULL)
    }
    
    if (include_tree) {
      result$tree <- tree
      result <- result %>%
        dplyr::select(tree, dplyr::everything())
    }
    
    as.data.frame(result)
  }


#' Plot a Tree Slot from a Microbiome Dataset
#'
#' Draw a lightweight default tree plot for `otu_tree` or `taxa_tree`.
#'
#' @param object A `microbiome_dataset` object.
#' @param tree Tree slot to plot, either `"otu_tree"` or `"taxa_tree"`.
#' @param layout Graph layout passed to [ggraph::ggraph()].
#' @param use_link Should explicit `*_tree_link` metadata be used before
#'   falling back to label-based matching? Defaults to `TRUE`.
#' @param color_by Optional variable mapped to node color.
#' @param size_by Optional variable mapped to node size.
#' @param abundance_method Summary used when `abundance` is mapped onto nodes.
#' @param taxonomic_rank Taxonomic rank used when mapping abundance onto
#'   `taxa_tree`. Ignored for `otu_tree`.
#' @param show_tip_label Should tip labels be drawn? Defaults to `TRUE`.
#' @param tip_label_size Text size for tip labels.
#'
#' @return A `ggplot` object.
#' @export
#' @examples
#' data("global_patterns", package = "microbiomedataset")
#'
#' x <- prune_taxa(global_patterns, variable_id = global_patterns@variable_info$variable_id[1:120])
#' x <- microbiomedataset::convert2microbiome_dataset(
#'   microbiomedataset::convert2phyloseq(x)
#' )
#' p <- plot_tree(x, tree = "taxa_tree")
#' class(p)
plot_tree <-
  function(object,
           tree = c("otu_tree", "taxa_tree"),
           layout = "dendrogram",
           use_link = TRUE,
           color_by = NULL,
           size_by = NULL,
           abundance_method = c("sum", "mean", "median"),
           taxonomic_rank = c("Kingdom",
                              "Phylum",
                              "Class",
                              "Order",
                              "Family",
                              "Genus",
                              "Species"),
           show_tip_label = TRUE,
           tip_label_size = 2.5) {
    tree <- match.arg(tree)
    abundance_method <- match.arg(abundance_method)
    taxonomic_rank <- match.arg(taxonomic_rank)
    tree_object <- extract_tree_data(
      object = object,
      tree = tree,
      data_type = "treedata"
    )
    
    if (is.null(tree_object)) {
      stop(tree, " is NULL.")
    }
    
    phylo <- tidytree::as.phylo(tree_object)
    graph_data <- ggraph::create_layout(graph = phylo, layout = layout)
    tree_table <- extract_tree_data(object, tree = tree, data_type = "table")
    
    graph_data <-
      dplyr::left_join(
        graph_data,
        tree_table,
        by = c("name" = "label")
      )
    link_table <- if (use_link) extract_tree_link_data(object, tree = tree) else NULL
    
    if (!"label" %in% colnames(graph_data)) {
      graph_data$label <- graph_data$name
    } else {
      graph_data$label[is.na(graph_data$label)] <- graph_data$name[is.na(graph_data$label)]
    }
    
    if (identical(color_by, "abundance") || identical(size_by, "abundance")) {
      abundance_fun <- switch(
        abundance_method,
        sum = sum,
        mean = mean,
        median = stats::median
      )
      abundance_table <- NULL
      if (!is.null(link_table) && nrow(link_table) > 0) {
        expression_data <- as.matrix(extract_expression_data(object))
        feature_abundance <- apply(expression_data, 1, abundance_fun, na.rm = TRUE)
        node_abundance <- lapply(split(link_table$variable_id, link_table$node_label), function(ids) {
          abundance_fun(feature_abundance[ids], na.rm = TRUE)
        })
        abundance_table <- data.frame(
          label = names(node_abundance),
          abundance = as.numeric(unlist(node_abundance)),
          stringsAsFactors = FALSE
        )
      } else if (identical(tree, "otu_tree")) {
        abundance_values <- apply(extract_expression_data(object), 1, abundance_fun, na.rm = TRUE)
        abundance_table <- data.frame(
          label = names(abundance_values),
          abundance = as.numeric(abundance_values),
          stringsAsFactors = FALSE
        )
      } else {
        rank_matrix <-
          extract_intensity(
            object = object,
            taxonomic_rank = taxonomic_rank,
            data_type = "wider",
            relative = FALSE,
            what = "sum_intensity"
          )
        abundance_values <- apply(rank_matrix, 1, abundance_fun, na.rm = TRUE)
        prefix_map <- c(
          Kingdom = "k__",
          Phylum = "p__",
          Class = "c__",
          Order = "o__",
          Family = "f__",
          Genus = "g__",
          Species = "s__"
        )
        abundance_table <- data.frame(
          label = paste0(unname(prefix_map[[taxonomic_rank]]), names(abundance_values)),
          abundance = as.numeric(abundance_values),
          stringsAsFactors = FALSE
        )
      }
      graph_data <- dplyr::left_join(graph_data, abundance_table, by = "label")
    }
    
    if (identical(tree, "otu_tree")) {
      variable_info <- extract_variable_info(object)
      graph_data <- dplyr::left_join(graph_data, variable_info, by = c("label" = "variable_id"))
    }
    
    p <-
      ggraph::ggraph(graph_data) +
      ggraph::geom_edge_diagonal(linewidth = 0.3, alpha = 0.8) +
      ggplot2::theme_void()
    
    point_mapping <- ggplot2::aes()
    if (!is.null(color_by)) {
      if (!color_by %in% colnames(graph_data)) {
        stop(color_by, " is not available for plotting.")
      }
      point_mapping$colour <- rlang::sym(color_by)
    }
    if (!is.null(size_by)) {
      if (!size_by %in% colnames(graph_data)) {
        stop(size_by, " is not available for plotting.")
      }
      point_mapping$size <- rlang::sym(size_by)
    }
    
    p <-
      p +
      ggraph::geom_node_point(
        data = graph_data[!graph_data$leaf, , drop = FALSE],
        size = if (is.null(size_by)) 1.2 else NULL,
        alpha = 0.8,
        mapping = point_mapping
      )
    
    tip_mapping <- ggplot2::aes(label = label)
    if (!is.null(color_by)) {
      tip_mapping$colour <- rlang::sym(color_by)
    }
    
    if (show_tip_label) {
      p <-
        p +
        ggraph::geom_node_text(
          data = graph_data[graph_data$leaf, , drop = FALSE],
          mapping = tip_mapping,
          hjust = -0.05,
          size = tip_label_size
        ) +
        ggplot2::coord_cartesian(clip = "off")
    }
    
    p
  }


#' Check Explicit Tree Link Metadata
#'
#' Summarize whether `otu_tree_link` or `taxa_tree_link` is structurally
#' consistent with the current object.
#'
#' @param object A `microbiome_dataset` object.
#' @param tree Tree slot, either `"otu_tree"` or `"taxa_tree"`.
#'
#' @return A one-row data frame summarizing coverage and potential issues.
#' @export
#' @examples
#' data("global_patterns", package = "microbiomedataset")
#'
#' x <- prune_taxa(global_patterns, variable_id = global_patterns@variable_info$variable_id[1:120])
#' x <- microbiomedataset::convert2microbiome_dataset(
#'   microbiomedataset::convert2phyloseq(x)
#' )
#' check_tree_link(x, tree = "taxa_tree")
check_tree_link <-
  function(object,
           tree = c("otu_tree", "taxa_tree")) {
    tree <- match.arg(tree)
    link_table <- extract_tree_link_data(object, tree = tree)
    tree_object <- extract_tree_data(object, tree = tree, data_type = "treedata")
    variable_ids <- extract_variable_info(object)$variable_id
    tree_nodes <- if (identical(tree, "otu_tree")) {
      get_tree_tip_labels(tree_object)
    } else {
      get_tree_node_labels(tree_object)
    }
    
    if (is.null(link_table)) {
      return(data.frame(
        tree = tree,
        has_tree = !is.null(tree_object),
        has_link = FALSE,
        n_links = 0L,
        linked_variables = 0L,
        uncovered_variables = length(variable_ids),
        invalid_nodes = 0L,
        duplicated_variables = 0L,
        stringsAsFactors = FALSE
      ))
    }
    
    data.frame(
      tree = tree,
      has_tree = !is.null(tree_object),
      has_link = TRUE,
      n_links = nrow(link_table),
      linked_variables = length(unique(link_table$variable_id)),
      uncovered_variables = sum(!variable_ids %in% link_table$variable_id),
      invalid_nodes = sum(!link_table$node_label %in% tree_nodes),
      duplicated_variables = sum(duplicated(link_table$variable_id)),
      stringsAsFactors = FALSE
    )
  }


#' Plot Tree Link Coverage
#'
#' Visualize how many variables are linked to each tree rank or tree type.
#'
#' @param object A `microbiome_dataset` object.
#' @param tree Tree slot, either `"otu_tree"` or `"taxa_tree"`.
#' @param x Variable used on the x-axis. Defaults to `"link_rank"`.
#'
#' @return A `ggplot` object.
#' @export
#' @examples
#' data("global_patterns", package = "microbiomedataset")
#'
#' x <- prune_taxa(global_patterns, variable_id = global_patterns@variable_info$variable_id[1:120])
#' x <- microbiomedataset::convert2microbiome_dataset(
#'   microbiomedataset::convert2phyloseq(x)
#' )
#' p <- plot_tree_link(x, tree = "taxa_tree")
#' class(p)
plot_tree_link <-
  function(object,
           tree = c("otu_tree", "taxa_tree"),
           x = c("link_rank", "tree")) {
    tree <- match.arg(tree)
    x <- match.arg(x)
    link_table <- extract_tree_link_data(object, tree = tree)
    
    if (is.null(link_table) || nrow(link_table) == 0) {
      stop("No tree link metadata available for ", tree, ".")
    }
    
    plot_data <- dplyr::count(link_table, !!rlang::sym(x), name = "n")
    
    ggplot2::ggplot(plot_data, ggplot2::aes(x = !!rlang::sym(x), y = .data$n, fill = !!rlang::sym(x))) +
      ggplot2::geom_col(show.legend = FALSE) +
      ggplot2::labs(x = x, y = "Linked variables", title = paste("Tree link coverage:", tree)) +
      ggplot2::theme_bw()
  }


#' Align Reference Sequences with the Current Dataset
#'
#' Synchronize `ref_seq` with the current `variable_info`.
#'
#' @param object A `microbiome_dataset` object.
#' @param drop_unmatched Should unmatched sequences be dropped? Defaults to
#'   `TRUE`.
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
#' x@ref_seq <- Biostrings::DNAStringSet(rep("ACGT", nrow(x@variable_info)))
#' x <- align_ref_seq(x)
#' names(x@ref_seq)[1]
align_ref_seq <-
  function(object,
           drop_unmatched = TRUE) {
    ref_seq <- object@ref_seq
    
    if (is.null(ref_seq)) {
      return(object)
    }
    
    variable_id <- as.character(object@variable_info$variable_id)
    ref_names <- names(ref_seq)
    
    if (is.null(ref_names)) {
      if (length(ref_seq) != length(variable_id)) {
        if (drop_unmatched) {
          object@ref_seq <- NULL
          return(
            append_process_parameter(
              object = object,
              process_name = "align_ref_seq",
              function_name = "align_ref_seq()",
              parameter = list(drop_unmatched = drop_unmatched, action = "drop_all_unnamed")
            )
          )
        }
        stop("Unnamed ref_seq must have the same length as variable_info.")
      }
      names(ref_seq) <- variable_id
      object@ref_seq <- ref_seq
    } else if (drop_unmatched) {
      object@ref_seq <- subset_ref_seq(ref_seq, variable_id)
    }
    
    append_process_parameter(
      object = object,
      process_name = "align_ref_seq",
      function_name = "align_ref_seq()",
      parameter = list(drop_unmatched = drop_unmatched)
    )
  }


#' Prune Reference Sequences in a Microbiome Dataset
#'
#' Keep or remove selected sequences from `ref_seq` without changing the
#' abundance matrix.
#'
#' @param object A `microbiome_dataset` object.
#' @param variable_id Variable identifiers to keep or remove.
#' @param variable_index Variable indices to keep or remove.
#' @param keep Should the provided sequences be kept? Defaults to `TRUE`.
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
#' x@ref_seq <- Biostrings::DNAStringSet(rep("ACGT", nrow(x@variable_info)))
#' names(x@ref_seq) <- x@variable_info$variable_id
#' x <- prune_ref_seq(x, variable_id = x@variable_info$variable_id[1:5])
#' length(x@ref_seq)
prune_ref_seq <-
  function(object,
           variable_id,
           variable_index,
           keep = TRUE) {
    if (missing(variable_id) && missing(variable_index)) {
      return(object)
    }
    
    variable_ids <- as.character(object@variable_info$variable_id)
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
    
    object@ref_seq <- subset_ref_seq(object@ref_seq, variable_ids[keep_index])
    
    append_process_parameter(
      object = object,
      process_name = "prune_ref_seq",
      function_name = "prune_ref_seq()",
      parameter = list(variable_id = variable_ids[keep_index], keep = keep)
    )
  }


#' Replace Reference Sequences in a Microbiome Dataset
#'
#' Set `ref_seq` on a `microbiome_dataset` and optionally align it with the
#' current dataset.
#'
#' @param object A `microbiome_dataset` object.
#' @param value A `Biostrings::XStringSet` object or `NULL`.
#' @param align Should the sequences be aligned after replacement? Defaults to
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
#' ref_seq <- Biostrings::DNAStringSet(rep("ACGT", nrow(x@variable_info)))
#' x <- replace_ref_seq(x, value = ref_seq)
#' length(x@ref_seq)
replace_ref_seq <-
  function(object,
           value = NULL,
           align = TRUE) {
    if (!is.null(value) && !methods::is(value, "XStringSet")) {
      stop("value must be a Biostrings::XStringSet object or NULL.")
    }
    
    object@ref_seq <- value
    
    if (align) {
      object <- align_ref_seq(object, drop_unmatched = TRUE)
    }
    
    append_process_parameter(
      object = object,
      process_name = "replace_ref_seq",
      function_name = "replace_ref_seq()",
      parameter = list(
        align = align,
        is_null = is.null(value)
      )
    )
  }


#' Export Reference Sequences to FASTA
#'
#' Write `ref_seq` from a `microbiome_dataset` to a FASTA file for use in
#' external tools.
#'
#' @param object A `microbiome_dataset` object.
#' @param file Output FASTA path.
#' @param width Number of letters per line in the FASTA file.
#'
#' @return Invisibly returns `file`.
#' @export
#' @examples
#' data("global_patterns", package = "microbiomedataset")
#'
#' x0 <- prune_taxa(global_patterns, variable_id = global_patterns@variable_info$variable_id[1:80])
#' x <- microbiomedataset::convert2microbiome_dataset(
#'   microbiomedataset::convert2phyloseq(x0)
#' )
#' ref_seq <- Biostrings::DNAStringSet(rep("ACGT", nrow(x@variable_info)))
#' x <- replace_ref_seq(x, value = ref_seq)
#' fasta_file <- tempfile(fileext = ".fasta")
#' export_ref_seq(x, fasta_file)
#' file.exists(fasta_file)
export_ref_seq <-
  function(object,
           file,
           width = 80) {
    if (missing(file)) {
      stop("file must be provided.")
    }
    
    ref_seq <- extract_ref_seq(object)
    
    if (is.null(ref_seq)) {
      stop("object@ref_seq is NULL.")
    }
    
    if (is.null(names(ref_seq))) {
      names(ref_seq) <- as.character(object@variable_info$variable_id)[seq_along(ref_seq)]
    }
    
    Biostrings::writeXStringSet(
      x = ref_seq,
      filepath = file,
      width = width,
      format = "fasta"
    )
    
    invisible(file)
  }


#' Import Reference Sequences from FASTA
#'
#' Read a FASTA file into `ref_seq` and optionally align sequence names to the
#' current dataset.
#'
#' @param object A `microbiome_dataset` object.
#' @param file Input FASTA path.
#' @param format Sequence format passed to [Biostrings::readDNAStringSet()].
#' @param align Should imported sequences be aligned to `variable_info`?
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
#' fasta_file <- tempfile(fileext = ".fasta")
#' ref_seq <- Biostrings::DNAStringSet(rep("ACGT", nrow(x@variable_info)))
#' names(ref_seq) <- x@variable_info$variable_id
#' Biostrings::writeXStringSet(ref_seq, fasta_file, format = "fasta")
#' x <- import_ref_seq(x, fasta_file)
#' length(x@ref_seq)
import_ref_seq <-
  function(object,
           file,
           format = "fasta",
           align = TRUE) {
    if (missing(file)) {
      stop("file must be provided.")
    }
    
    ref_seq <- switch(
      format,
      fasta = Biostrings::readDNAStringSet(filepath = file, format = "fasta"),
      stop("Unsupported format: ", format)
    )
    
    object <- replace_ref_seq(
      object = object,
      value = ref_seq,
      align = align
    )
    
    append_process_parameter(
      object = object,
      process_name = "import_ref_seq",
      function_name = "import_ref_seq()",
      parameter = list(
        file = file,
        format = format,
        align = align
      )
    )
  }


#' Import a Tree from Newick into a Microbiome Dataset
#'
#' Read a tree file and store it in `otu_tree` or `taxa_tree`, optionally
#' aligning the imported tree to the current dataset.
#'
#' @param object A `microbiome_dataset` object.
#' @param file Input tree path.
#' @param tree Tree slot to import into, either `"otu_tree"` or `"taxa_tree"`.
#' @param format Tree format. Currently only `"newick"` is supported.
#' @param align Should the imported tree be aligned to the current dataset?
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
#' tree_file <- tempfile(fileext = ".nwk")
#' otu_phylo <- ape::rtree(
#'   n = nrow(x@variable_info),
#'   tip.label = x@variable_info$variable_id
#' )
#' ape::write.tree(otu_phylo, file = tree_file)
#' x <- import_tree(x, file = tree_file, tree = "otu_tree")
#' class(x@otu_tree)
import_tree <-
  function(object,
           file,
           tree = c("otu_tree", "taxa_tree"),
           format = c("newick"),
           align = TRUE) {
    tree <- match.arg(tree)
    format <- match.arg(format)
    
    if (missing(file)) {
      stop("file must be provided.")
    }
    
    phylo <- switch(
      format,
      newick = ape::read.tree(file = file)
    )
    
    tree_object <- tidytree::treedata(
      phylo = phylo,
      data = tibble::tibble(node = seq_len(length(phylo$tip.label) + phylo$Nnode))
    )
    
    object <- replace_tree(
      object = object,
      tree = tree,
      value = tree_object,
      align = align
    )
    
    append_process_parameter(
      object = object,
      process_name = "import_tree",
      function_name = "import_tree()",
      parameter = list(
        file = file,
        tree = tree,
        format = format,
        align = align
      )
    )
  }


#' Export a Tree Slot to Newick or Nexus
#'
#' Write `otu_tree` or `taxa_tree` from a `microbiome_dataset` to a tree file
#' for use in external tools.
#'
#' @param object A `microbiome_dataset` object.
#' @param file Output tree path.
#' @param tree Tree slot to export, either `"otu_tree"` or `"taxa_tree"`.
#' @param format Tree format, one of `"newick"` or `"nexus"`.
#'
#' @return Invisibly returns `file`.
#' @export
#' @examples
#' data("global_patterns", package = "microbiomedataset")
#'
#' x0 <- prune_taxa(global_patterns, variable_id = global_patterns@variable_info$variable_id[1:80])
#' x <- microbiomedataset::convert2microbiome_dataset(
#'   microbiomedataset::convert2phyloseq(x0)
#' )
#' tree_file <- tempfile(fileext = ".nwk")
#' export_tree(x, file = tree_file, tree = "taxa_tree", format = "newick")
#' file.exists(tree_file)
export_tree <-
  function(object,
           file,
           tree = c("otu_tree", "taxa_tree"),
           format = c("newick", "nexus")) {
    tree <- match.arg(tree)
    format <- match.arg(format)
    
    if (missing(file)) {
      stop("file must be provided.")
    }
    
    phylo <- extract_tree_data(
      object = object,
      tree = tree,
      data_type = "phylo"
    )
    
    if (is.null(phylo)) {
      stop(tree, " is NULL.")
    }
    
    switch(
      format,
      newick = ape::write.tree(phy = phylo, file = file),
      nexus = ape::write.nexus(phy = phylo, file = file)
    )
    
    invisible(file)
  }
