####This code is from MicrobiotaProcess, credit should go its developer

#' @importFrom magrittr %>%
#' @importFrom tibble rownames_to_column column_to_rownames
#' @keywords internal
duplicated_taxa_check <- function(taxdf) {
  if (ncol(taxdf) == 1) {
    return(taxdf)
  }
  taxdf <- taxdf %>%
    tibble::rownames_to_column()
  for (i in ncol(taxdf):3) {
    tmp <- split(taxdf, taxdf[, i])
    for (j in seq_len(length(tmp))) {
      flag <- length(unique(as.vector(tmp[[j]][, i - 1])))
      if (flag > 1) {
        tmp[[j]][, i] <- paste(tmp[[j]][, i], tmp[[j]][, i - 1], sep = "_")
      }
    }
    taxdf <- do.call("rbind", c(tmp, make.row.names = FALSE))
  }
  return(taxdf)
}

####This code is from MicrobiotaProcess, credit should go its developer

# #' @keywords internal
# #taxlevelchar <- c("k", "p", "c", "o", "f", "g", "s", "st")

new_taxa_name <- function(x, y, taxlevelchar) {
  y <- as.vector(y)
  x[y] <- paste(taxlevelchar[y], x[y], sep = "__un_")
  x
}

####This code is from MicrobiotaProcess, credit should go its developer

#' @keywords internal
repduplicated_tax_check <- function(taxdf) {
  for (i in seq_len(7)) {
    taxdf <- duplicated_taxa_check(taxdf) %>%
      tibble::column_to_rownames(var = "rowname")
  }
  return(taxdf)
}

####This code is from MicrobiotaProcess, credit should go its developer
#' @importFrom zoo na.locf
#' @keywords internal
fill_taxa_table_name <-
  function(taxdf, type = "species", taxlevelchar = NULL) {
    tmprownames <- rownames(taxdf)
    if (is.null(taxlevelchar)) {
      taxlevelchar <-
        if (type == "species") {
          c("k", "p", "c", "o", "f", "g", "s", "st")
        } else {
          paste0("d", seq_len(ncol(taxdf)))
        }
    }
    indexmark <-
      apply(taxdf, 2, function(x) {
        nchar(x, keepNA = TRUE)
      }) <= 4
    taxdf[indexmark] <- NA
    if (any(is.na(taxdf[, 1]))) {
      if (type == "species") {
        prefix <- "k__"
      } else{
        prefix <- "d1__"
      }
      taxdf[is.na(taxdf[, 1]), 1] <- paste0(prefix, "Unknown")
    }
    indextmp <- apply(is.na(taxdf), 1, which)
    if (length(indextmp) == 0) {
      taxdf <- data.frame(taxdf, check.names = FALSE)
      return(taxdf)
    }
    taxdf <- apply(taxdf, 1, zoo::na.locf)
    taxdf <- lapply(seq_len(ncol(taxdf)), function(i)
      taxdf[, i])
    taxdf <- data.frame(t(mapply(
      new_taxa_name,
      taxdf,
      indextmp,
      MoreArgs = list(taxlevelchar = taxlevelchar)
    )),
                        stringsAsFactors = FALSE)
    rownames(taxdf) <- tmprownames
    return(taxdf)
  }

####This code is from MicrobiotaProcess, credit should go its developer
#' @keywords internal
addtaxlevel <- function(taxdf, taxlevelchar) {
  #, type="species"){
  #if (type != "species"){
  #    taxlevelchar <- paste0("d", seq_len(ncol(taxdf)))
  #}else{
  #    taxlevelchar <- taxlevelchar[seq_len(ncol(taxdf))]
  #}
  current_levels <- taxlevelchar[seq_len(length(taxdf))]
  paste(current_levels, taxdf, sep = "__")
}


####This code is from MicrobiotaProcess, credit should go its developer
fill_taxa_table_na <-
  function(taxdf, type = "species") {
    taxdf <- remove_taxonomy_rank_na(taxdf)
    taxlevelchar <-
      if (type != "species") {
        paste0("d", seq_len(ncol(taxdf)))
      } else{
        c("k", "p", "c", "o", "f", "g", "s", "st")
      }
    if (!(grepl("^k__", taxdf[1, 1]) ||
          grepl("^d1__", taxdf[1, 1]))) {
      tmprownames <- rownames(taxdf)
      tmpcolnames <- colnames(taxdf)
      taxdf <- t(apply(taxdf, 1, as.character))
      taxdf[is.na(taxdf)] <- ""
      taxdf <- data.frame(t(apply(taxdf, 1, addtaxlevel, taxlevelchar = taxlevelchar)),
                          stringsAsFactors = FALSE)
      rownames(taxdf) <- tmprownames
      colnames(taxdf) <- tmpcolnames
    }
    taxdf <-
      fill_taxa_table_name(taxdf, type = type, taxlevelchar = taxlevelchar)
    taxdf <-
      repduplicated_tax_check(taxdf) #%>% column_to_rownames(var="rowname")
    attr(taxdf, "fill_taxa_table_na") <- TRUE
    return(taxdf)
  }


####This code is from MicrobiotaProcess, credit should go its developer
remove_taxonomy_rank_na <-
  function(x) {
    x <- as.data.frame(x, check.names = FALSE)
    x <- remove_taxa_table_unclassfied(x)
    indx <- vapply(x,
                   function(i)
                     all(is.na(i)),
                   FUN.VALUE = logical(1)) %>%
      setNames(NULL)
    x <- x[, !indx, drop = FALSE]
    return(x)
  }


# ####This code is from MicrobiotaProcess, credit should go its developer
# build_refseq <- function(x) {
#   flag <- guess_rownames(x)
#   refseq <- switch(
#     flag,
#     DNA = Biostrings::DNAStringSet(x),
#     AA = Biostrings::AAStringSet(x),
#     RNA = Biostrings::RNAStringSet(x),
#     Other = NULL
#   )
#   return(refseq)
# }

####This code is from MicrobiotaProcess, credit should go its developer
remove_taxa_table_unclassfied <-
  function(taxdf) {
    taxdf[grepl_data.frame(
      "Unclassified|uncultured|Ambiguous|Unknown|unknown|metagenome|Unassig",
      taxdf,
      ignore.case = TRUE
    )] <- NA
    return(taxdf)
  }

####This code is from MicrobiotaProcess, credit should go its developer
grepl_data.frame <- function(pattern, x, ...) {
  y <- if (length(x)) {
    do.call("cbind", lapply(x, "grepl", pattern = pattern, ...))
  } else{
    matrix(FALSE, length(row.names(x)), 0)
  }
  if (.row_names_info(x) > 0L)
    rownames(y) <- row.names(x)
  y
}


msg <-
  function(..., startup = FALSE) {
    if (startup) {
      if (!isTRUE(getOption("microbiomedataset.quiet"))) {
        packageStartupMessage(text_col(...))
      }
    } else {
      message(text_col(...))
    }
  }

text_col <- function(x) {
  # If RStudio not available, messages already printed in black
  if (!rstudioapi::isAvailable()) {
    return(x)
  }
  
  if (!rstudioapi::hasFun("getThemeInfo")) {
    return(x)
  }
  
  theme <- rstudioapi::getThemeInfo()
  
  if (isTRUE(theme$dark))
    crayon::white(x)
  else
    crayon::black(x)
  
}

#' List all packages in the microbiomedataset
#'
#' @param include_self Include microbiomedataset in the list?
#' @export
#' @return microbiomedataset packages
#' @examples
#' microbiomedataset_packages()
microbiomedataset_packages <-
  function(include_self = TRUE) {
    raw <- utils::packageDescription("microbiomedataset")$Imports
    imports <- strsplit(raw, ",")[[1]]
    parsed <- gsub("^\\s+|\\s+$", "", imports)
    names <-
      vapply(strsplit(parsed, "\\s+"), "[[", 1, FUN.VALUE = character(1))
    
    if (include_self) {
      names <- c(names, "microbiomedataset")
    }
    
    names
  }

invert <- function(x) {
  if (length(x) == 0)
    return()
  stacked <- utils::stack(x)
  tapply(as.character(stacked$ind), stacked$values, list)
}


style_grey <- function(level, ...) {
  crayon::style(paste0(...),
                crayon::make_style(grDevices::grey(level), grey = TRUE))
}


###re-assign NA to downstream level data
remove_na_from_taxa_table <-
  function(x) {
    ###Kingdom
    idx <- which(is.na(x$Kingdom))
    if (length(idx) > 0) {
      x[idx, c("Phylum", "Class", "Order", "Family", "Genus", "Species")] <-
        NA
    }
    
    ###Phylum
    idx <- which(is.na(x$Phylum))
    if (length(idx) > 0) {
      x[idx, c("Class", "Order", "Family", "Genus", "Species")] <- NA
    }
    
    ###Class
    idx <- which(is.na(x$Class))
    if (length(idx) > 0) {
      x[idx, c("Order", "Family", "Genus", "Species")] <- NA
    }
    
    ###Order
    idx <- which(is.na(x$Order))
    if (length(idx) > 0) {
      x[idx, c("Family", "Genus", "Species")] <- NA
    }
    
    ###Family
    idx <- which(is.na(x$Family))
    if (length(idx) > 0) {
      x[idx, c("Genus", "Species")] <- NA
    }
    
    ###Genus
    idx <- which(is.na(x$Genus))
    if (length(idx) > 0) {
      x[idx, ]$Species <- NA
    }
    
    return(x)
  }



calculate <-
  function(value,
           what = c("mean_intensity",
                    "median_intensity",
                    "sum_intensity",
                    "na_number",
                    "na_freq"),
           ...) {
    switch(
      EXPR = what,
      mean_intensity = mean(value, ...),
      median_intensity = median(value, ...),
      sum_intensity = sum(value, ...)
    )
  }


microbiome_taxonomy_ranks <- function() {
  c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")
}


parse_borenstein_taxonomy <- function(taxonomy_strings) {
  taxonomy_strings <- as.character(taxonomy_strings)
  ranks <- microbiome_taxonomy_ranks()
  prefix_map <- c(
    d = "Kingdom",
    k = "Kingdom",
    p = "Phylum",
    c = "Class",
    o = "Order",
    f = "Family",
    g = "Genus",
    s = "Species"
  )
  
  parsed <- lapply(taxonomy_strings, function(one_taxonomy) {
    values <- stats::setNames(rep(NA_character_, length(ranks)), ranks)
    if (is.na(one_taxonomy) || !nzchar(one_taxonomy) || identical(one_taxonomy, "Unclassified")) {
      return(values)
    }
    
    one_taxonomy <- strsplit(one_taxonomy, ";", fixed = TRUE)[[1]]
    for (part in one_taxonomy) {
      prefix <- sub("__.*$", "", part)
      prefix <- sub("_$", "", prefix)
      prefix <- substr(prefix, 1, 1)
      rank_name <- unname(prefix_map[prefix])
      if (is.na(rank_name) || !nzchar(rank_name)) {
        next
      }
      value <- sub("^[a-z]__", "", part)
      value <- trimws(value)
      if (!nzchar(value) || value %in% c("NA", "unclassified", "uncultured")) {
        value <- NA_character_
      }
      values[[rank_name]] <- value
    }
    values
  })
  
  parsed <- do.call(rbind, parsed)
  parsed <- as.data.frame(parsed, stringsAsFactors = FALSE, check.names = FALSE)
  parsed <- parsed[, ranks, drop = FALSE]
  rownames(parsed) <- taxonomy_strings
  parsed
}


taxonomy_rank_aliases <- function() {
  c(Domain = "Kingdom")
}


standardize_taxonomy_colnames <- function(x) {
  colnames_x <- colnames(x)
  aliases <- taxonomy_rank_aliases()
  renamed <- unname(aliases[colnames_x])
  colnames_x[!is.na(renamed)] <- renamed[!is.na(renamed)]
  colnames(x) <- colnames_x
  x
}


normalize_sample_info_schema <- function(sample_info) {
  sample_info <- as.data.frame(sample_info, check.names = FALSE)
  if ("sample_id" %in% colnames(sample_info)) {
    sample_info$sample_id <- as.character(sample_info$sample_id)
  }
  if ("class" %in% colnames(sample_info)) {
    sample_info$class <- as.character(sample_info$class)
  }
  sample_info
}


get_taxonomy_table <- function(variable_info,
                               ranks = microbiome_taxonomy_ranks()) {
  variable_info <- standardize_taxonomy_colnames(as.data.frame(variable_info))
  available_ranks <- intersect(ranks, colnames(variable_info))
  if (length(available_ranks) == 0) {
    return(data.frame(row.names = rownames(variable_info)))
  }
  variable_info[, available_ranks, drop = FALSE]
}


complete_taxonomy_table <- function(variable_info,
                                    ranks = microbiome_taxonomy_ranks()) {
  variable_info <- normalize_variable_info_schema(variable_info)
  taxonomy_table <- get_taxonomy_table(variable_info, ranks = ranks)
  missing_ranks <- setdiff(ranks, colnames(taxonomy_table))
  if (length(missing_ranks) > 0) {
    for (rank in missing_ranks) {
      taxonomy_table[[rank]] <- NA_character_
    }
  }
  taxonomy_table[, ranks, drop = FALSE]
}


suffix_metadata_columns <- function(x,
                                    id_column,
                                    suffix) {
  x <- as.data.frame(x, check.names = FALSE)
  rename_columns <- setdiff(colnames(x), id_column)
  if (length(rename_columns) == 0) {
    return(x)
  }
  colnames(x)[match(rename_columns, colnames(x))] <- paste0(rename_columns, suffix)
  x
}


normalize_variable_info_schema <- function(variable_info) {
  variable_info <- standardize_taxonomy_colnames(as.data.frame(variable_info))
  if ("variable_id" %in% colnames(variable_info)) {
    variable_info$variable_id <- as.character(variable_info$variable_id)
  }
  for (rank in intersect(microbiome_taxonomy_ranks(), colnames(variable_info))) {
    variable_info[[rank]] <- as.character(variable_info[[rank]])
  }
  if ("variable_id" %in% colnames(variable_info)) {
    variable_info <- variable_info %>%
      dplyr::select(variable_id, dplyr::everything())
  }
  variable_info
}


normalize_variable_info_note_schema <- function(variable_info_note) {
  variable_info_note <- as.data.frame(variable_info_note, check.names = FALSE)
  if ("name" %in% colnames(variable_info_note)) {
    variable_info_note$name <-
      dplyr::recode(variable_info_note$name, !!!taxonomy_rank_aliases())
    variable_info_note <- variable_info_note %>%
      dplyr::distinct(name, .keep_all = TRUE)
  }
  variable_info_note
}


get_tree_tip_labels <- function(tree) {
  if (is.null(tree)) {
    return(NULL)
  }
  phylo <- tryCatch(
    tidytree::as.phylo(tree),
    error = function(e)
      NULL
  )
  if (is.null(phylo)) {
    return(NULL)
  }
  phylo$tip.label
}


get_tree_node_labels <- function(tree) {
  if (is.null(tree)) {
    return(character())
  }
  tree_table <- tryCatch(
    as.data.frame(tidytree::as_tibble(tree)),
    error = function(e) NULL
  )
  if (is.null(tree_table) || !"label" %in% colnames(tree_table)) {
    return(character())
  }
  unique(stats::na.omit(as.character(tree_table$label)))
}


normalize_tree_link_table <- function(link_table,
                                      tree = c("otu_tree", "taxa_tree")) {
  tree <- match.arg(tree)
  
  if (is.null(link_table)) {
    return(NULL)
  }
  
  link_table <- as.data.frame(link_table, check.names = FALSE)
  required_columns <- c("variable_id", "node", "node_label", "tree")
  missing_columns <- setdiff(required_columns, colnames(link_table))
  if (length(missing_columns) > 0) {
    stop(
      "tree link table is missing required columns: ",
      paste(missing_columns, collapse = ", "),
      "."
    )
  }
  
  link_table$variable_id <- as.character(link_table$variable_id)
  link_table$node <- as.integer(link_table$node)
  link_table$node_label <- as.character(link_table$node_label)
  link_table$tree <- as.character(link_table$tree)
  link_table <- link_table[link_table$tree == tree, , drop = FALSE]
  link_table <- dplyr::distinct(link_table)
  rownames(link_table) <- NULL
  link_table
}


build_otu_tree_link <- function(variable_info, tree = NULL) {
  variable_info <- normalize_variable_info_schema(variable_info)
  if (is.null(tree)) {
    return(NULL)
  }
  
  tree_labels <- get_tree_tip_labels(tree)
  if (length(tree_labels) == 0) {
    return(NULL)
  }
  
  tree_table <- tryCatch(
    as.data.frame(tidytree::as_tibble(tree)),
    error = function(e) NULL
  )
  if (is.null(tree_table) || !all(c("node", "label") %in% colnames(tree_table))) {
    return(NULL)
  }
  
  tree_table$label <- as.character(tree_table$label)
  tree_table <- tree_table[tree_table$label %in% variable_info$variable_id, c("node", "label"), drop = FALSE]
  if (nrow(tree_table) == 0) {
    return(NULL)
  }
  
  link_table <- data.frame(
    variable_id = tree_table$label,
    node = tree_table$node,
    node_label = tree_table$label,
    tree = "otu_tree",
    link_rank = "OTU",
    stringsAsFactors = FALSE
  )
  link_table[order(link_table$variable_id), , drop = FALSE]
}


build_taxa_tree_link <- function(variable_info, tree = NULL) {
  variable_info <- normalize_variable_info_schema(variable_info)
  if (is.null(tree)) {
    return(NULL)
  }
  taxonomy_ranks <- intersect(
    microbiome_taxonomy_ranks(),
    colnames(variable_info)
  )
  if (length(taxonomy_ranks) == 0) {
    return(NULL)
  }
  
  tree_table <- tryCatch(
    as.data.frame(tidytree::as_tibble(tree)),
    error = function(e) NULL
  )
  if (is.null(tree_table) || !all(c("node", "label", "nodeClass") %in% colnames(tree_table))) {
    return(NULL)
  }
  tree_table$label <- as.character(tree_table$label)
  tree_table$nodeClass <- as.character(tree_table$nodeClass)
  tree_table$clean_label <- sub("^[a-z]__", "", tree_table$label)
  
  rank_values <- lapply(taxonomy_ranks, function(rank) {
    values <- as.character(variable_info[[rank]])
    values[values == ""] <- NA_character_
    values
  })
  names(rank_values) <- taxonomy_ranks
  
  link_rows <- lapply(seq_len(nrow(variable_info)), function(i) {
    available_ranks <- taxonomy_ranks[!is.na(vapply(rank_values, `[`, FUN.VALUE = character(1), i))]
    if (length(available_ranks) == 0) {
      return(NULL)
    }
    deepest_rank <- NULL
    matched_node <- NULL
    for (rank in rev(available_ranks)) {
      matched_rows <- tree_table[
        tree_table$nodeClass == rank &
          tree_table$clean_label == rank_values[[rank]][i],
        ,
        drop = FALSE
      ]
      if (nrow(matched_rows) > 0) {
        deepest_rank <- rank
        matched_node <- matched_rows[1, , drop = FALSE]
        break
      }
    }
    if (is.null(deepest_rank) || is.null(matched_node)) {
      return(NULL)
    }
    data.frame(
      variable_id = variable_info$variable_id[i],
      node = matched_node$node,
      node_label = matched_node$label,
      tree = "taxa_tree",
      link_rank = deepest_rank,
      stringsAsFactors = FALSE
    )
  })
  
  link_table <- dplyr::bind_rows(link_rows)
  if (nrow(link_table) == 0) {
    return(NULL)
  }
  rownames(link_table) <- NULL
  link_table
}


subset_tree_link <- function(link_table, variable_id = NULL, node_label = NULL) {
  if (is.null(link_table)) {
    return(NULL)
  }
  link_table <- as.data.frame(link_table, check.names = FALSE)
  if (!is.null(variable_id)) {
    link_table <- link_table[link_table$variable_id %in% as.character(variable_id), , drop = FALSE]
  }
  if (!is.null(node_label)) {
    link_table <- link_table[link_table$node_label %in% as.character(node_label), , drop = FALSE]
  }
  if (nrow(link_table) == 0) {
    return(NULL)
  }
  rownames(link_table) <- NULL
  link_table
}


prune_otu_tree <- function(tree, variable_id) {
  if (is.null(tree)) {
    return(NULL)
  }
  phylo <- tryCatch(
    tidytree::as.phylo(tree),
    error = function(e)
      NULL
  )
  if (is.null(phylo)) {
    return(NULL)
  }
  keep_tips <- intersect(as.character(variable_id), phylo$tip.label)
  if (length(keep_tips) == 0) {
    return(NULL)
  }
  phylo <- ape::keep.tip(phy = phylo, tip = keep_tips)
  tidytree::treedata(
    phylo = phylo,
    data = tibble::tibble(node = seq_len(length(phylo$tip.label) + phylo$Nnode))
  )
}


prune_treedata_tips <- function(tree, tip_label, keep = TRUE) {
  if (is.null(tree)) {
    return(NULL)
  }
  phylo <- tryCatch(
    tidytree::as.phylo(tree),
    error = function(e)
      NULL
  )
  if (is.null(phylo)) {
    return(NULL)
  }
  selected_tips <- intersect(as.character(tip_label), phylo$tip.label)
  if (length(selected_tips) == 0) {
    return(if (keep) NULL else tree)
  }
  phylo <- if (keep) {
    ape::keep.tip(phy = phylo, tip = selected_tips)
  } else {
    drop_tips <- setdiff(phylo$tip.label, selected_tips)
    if (length(drop_tips) == 0) {
      phylo
    } else {
      ape::drop.tip(phy = phylo, tip = drop_tips)
    }
  }
  tidytree::treedata(
    phylo = phylo,
    data = tibble::tibble(node = seq_len(length(phylo$tip.label) + phylo$Nnode))
  )
}


rebuild_taxa_tree <- function(variable_info) {
  taxonomy_table <- get_taxonomy_table(variable_info)
  if (ncol(taxonomy_table) == 0 || nrow(taxonomy_table) == 0) {
    return(NULL)
  }
  tryCatch(
    convert2treedata(taxonomy_table),
    error = function(e)
      NULL
  )
}


subset_ref_seq <- function(ref_seq, variable_id) {
  if (is.null(ref_seq)) {
    return(NULL)
  }
  ref_names <- names(ref_seq)
  if (is.null(ref_names)) {
    return(NULL)
  }
  keep_names <- intersect(as.character(variable_id), ref_names)
  if (length(keep_names) == 0) {
    return(NULL)
  }
  ref_seq[match(keep_names, ref_names)]
}


append_process_parameter <- function(object,
                                     process_name,
                                     function_name,
                                     parameter = list()) {
  process_info <- slot(object, name = "process_info")
  new_parameter <- new(
    Class = "tidymass_parameter",
    pacakge_name = "microbiomedataset",
    function_name = function_name,
    parameter = parameter,
    time = Sys.time()
  )
  if (all(names(process_info) != process_name)) {
    process_info[[process_name]] <- new_parameter
  } else{
    process_info[[process_name]] <- c(process_info[[process_name]], new_parameter)
  }
  slot(object, "process_info") <- process_info
  object
}


normalize_crossomics_sample_link <- function(sample_link) {
  sample_link <- as.data.frame(sample_link, check.names = FALSE)
  required_columns <- c("microbiome_sample_id", "metabolome_sample_id")
  missing_columns <- setdiff(required_columns, colnames(sample_link))
  if (length(missing_columns) > 0) {
    stop(
      "sample_link is missing required columns: ",
      paste(missing_columns, collapse = ", "),
      "."
    )
  }
  
  sample_link$microbiome_sample_id <- as.character(sample_link$microbiome_sample_id)
  sample_link$metabolome_sample_id <- as.character(sample_link$metabolome_sample_id)
  if (!"pair_id" %in% colnames(sample_link)) {
    sample_link$pair_id <- paste0("pair_", seq_len(nrow(sample_link)))
  }
  sample_link$pair_id <- as.character(sample_link$pair_id)
  sample_link <- dplyr::distinct(sample_link)
  rownames(sample_link) <- NULL
  sample_link
}


normalize_crossomics_feature_link <- function(feature_link) {
  if (is.null(feature_link)) {
    return(NULL)
  }
  feature_link <- as.data.frame(feature_link, check.names = FALSE)
  required_columns <- c("taxon_id", "metabolite_id")
  missing_columns <- setdiff(required_columns, colnames(feature_link))
  if (length(missing_columns) > 0) {
    stop(
      "feature_link is missing required columns: ",
      paste(missing_columns, collapse = ", "),
      "."
    )
  }
  feature_link$taxon_id <- as.character(feature_link$taxon_id)
  feature_link$metabolite_id <- as.character(feature_link$metabolite_id)
  rownames(feature_link) <- NULL
  feature_link
}


metabolite_annotation_aliases <- function() {
  c(
    Compound = "compound_name",
    compound = "compound_name",
    CompoundName = "compound_name",
    compoundname = "compound_name",
    Compound_Name = "compound_name",
    pathway = "pathway_id",
    Pathway = "pathway_id",
    pathway_name = "pathway_name",
    PathwayName = "pathway_name",
    annotation = "annotation_level",
    Annotation = "annotation_level",
    level = "annotation_level",
    Level = "annotation_level"
  )
}


#' Standardize Metabolite Annotation
#'
#' Normalize metabolite annotation column names so downstream cross-omics
#' functions can rely on a stable schema.
#'
#' @param annotation_table A metabolite annotation table.
#' @param variable_info Optional metabolite `variable_info` used to keep output
#'   rows aligned with feature ids.
#'
#' @return A standardized annotation table.
#' @export
#' @examples
#' annotation_table <- data.frame(
#'   variable_id = c("M1", "M2"),
#'   Compound = c("acetate", "lactate"),
#'   Pathway = c("organic_acid", "organic_acid")
#' )
#' standardize_metabolite_annotation(annotation_table)
standardize_metabolite_annotation <- function(annotation_table,
                                              variable_info = NULL) {
  if (is.null(annotation_table)) {
    return(NULL)
  }
  annotation_table <- as.data.frame(annotation_table, check.names = FALSE)
  variable_ids <- NULL
  if (!is.null(variable_info)) {
    variable_info <- as.data.frame(variable_info, check.names = FALSE)
    if ("variable_id" %in% colnames(variable_info)) {
      variable_ids <- as.character(variable_info$variable_id)
    }
  }
  if (nrow(annotation_table) == 0 && !is.null(variable_ids)) {
    annotation_table <- data.frame(
      variable_id = variable_ids,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }
  aliases <- metabolite_annotation_aliases()
  renamed <- unname(aliases[colnames(annotation_table)])
  colnames(annotation_table)[!is.na(renamed)] <- renamed[!is.na(renamed)]
  
  if (!"variable_id" %in% colnames(annotation_table)) {
    if (!is.null(rownames(annotation_table)) &&
        any(nzchar(rownames(annotation_table)))) {
      annotation_table$variable_id <- rownames(annotation_table)
    } else if (!is.null(variable_ids) && length(variable_ids) == nrow(annotation_table)) {
      annotation_table$variable_id <- variable_ids
    } else if (!is.null(variable_ids)) {
      annotation_table <- data.frame(
        variable_id = variable_ids,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    } else {
      stop("annotation_table must contain variable_id.")
    }
  }
  
  annotation_table$variable_id <- as.character(annotation_table$variable_id)
  default_columns <- c("compound_name", "pathway_id", "pathway_name", "annotation_level")
  for (column in default_columns) {
    if (!column %in% colnames(annotation_table)) {
      annotation_table[[column]] <- NA_character_
    }
    annotation_table[[column]] <- as.character(annotation_table[[column]])
  }
  
  if (!is.null(variable_ids)) {
    variable_info <- data.frame(variable_id = variable_ids, stringsAsFactors = FALSE)
    annotation_table <- dplyr::right_join(annotation_table, variable_info, by = "variable_id")
  }
  
  annotation_table <- annotation_table %>%
    dplyr::distinct(.data$variable_id, .keep_all = TRUE)
  rownames(annotation_table) <- NULL
  annotation_table
}


normalize_crossomics_pathway_link <- function(pathway_link) {
  if (is.null(pathway_link)) {
    return(NULL)
  }
  pathway_link <- as.data.frame(pathway_link, check.names = FALSE)
  required_columns <- c("taxon_id", "metabolite_id", "pathway_id")
  missing_columns <- setdiff(required_columns, colnames(pathway_link))
  if (length(missing_columns) > 0) {
    stop(
      "pathway_link is missing required columns: ",
      paste(missing_columns, collapse = ", "),
      "."
    )
  }
  pathway_link$taxon_id <- as.character(pathway_link$taxon_id)
  pathway_link$metabolite_id <- as.character(pathway_link$metabolite_id)
  pathway_link$pathway_id <- as.character(pathway_link$pathway_id)
  optional_columns <- c(
    "pathway_name",
    "reaction_id",
    "evidence_type",
    "compound_name",
    "annotation_level"
  )
  for (column in optional_columns) {
    if (!column %in% colnames(pathway_link)) {
      pathway_link[[column]] <- NA_character_
    }
    pathway_link[[column]] <- as.character(pathway_link[[column]])
  }
  if (all(is.na(pathway_link$evidence_type))) {
    pathway_link$evidence_type <- "user_supplied"
  }
  rownames(pathway_link) <- NULL
  pathway_link
}


#' Standardize Pathway Link Metadata
#'
#' Normalize a cross-omics pathway link table to the schema used by
#' [infer_metabolic_link()].
#'
#' @param pathway_link A pathway link table.
#'
#' @return A standardized pathway link table.
#' @export
#' @examples
#' pathway_link <- data.frame(
#'   taxon_id = "Genus_A",
#'   metabolite_id = "M1",
#'   pathway_id = "pathway_1"
#' )
#' standardize_pathway_link(pathway_link)
standardize_pathway_link <- function(pathway_link) {
  normalize_crossomics_pathway_link(pathway_link)
}


#' Standardize Taxon-to-Pathway Link Metadata
#'
#' Normalize a taxon-to-pathway mapping table for pathway-level mechanism
#' summarization.
#'
#' @param taxon_pathway_link A taxon-to-pathway link table.
#'
#' @return A standardized taxon-to-pathway link table.
#' @export
#' @examples
#' taxon_pathway_link <- data.frame(
#'   taxon_id = "Genus_A",
#'   pathway_id = "pathway_1"
#' )
#' standardize_taxon_pathway_link(taxon_pathway_link)
standardize_taxon_pathway_link <- function(taxon_pathway_link) {
  if (is.null(taxon_pathway_link)) {
    return(NULL)
  }
  taxon_pathway_link <- as.data.frame(taxon_pathway_link, check.names = FALSE)
  required_columns <- c("taxon_id", "pathway_id")
  missing_columns <- setdiff(required_columns, colnames(taxon_pathway_link))
  if (length(missing_columns) > 0) {
    stop(
      "taxon_pathway_link is missing required columns: ",
      paste(missing_columns, collapse = ", "),
      "."
    )
  }
  optional_columns <- c("pathway_name", "reaction_id", "evidence_type")
  taxon_pathway_link$taxon_id <- as.character(taxon_pathway_link$taxon_id)
  taxon_pathway_link$pathway_id <- as.character(taxon_pathway_link$pathway_id)
  for (column in optional_columns) {
    if (!column %in% colnames(taxon_pathway_link)) {
      taxon_pathway_link[[column]] <- NA_character_
    }
    taxon_pathway_link[[column]] <- as.character(taxon_pathway_link[[column]])
  }
  if (all(is.na(taxon_pathway_link$evidence_type))) {
    taxon_pathway_link$evidence_type <- "user_supplied"
  }
  taxon_pathway_link <- dplyr::distinct(taxon_pathway_link)
  rownames(taxon_pathway_link) <- NULL
  taxon_pathway_link
}


build_crossomics_sample_link <- function(microbiome_data, metabolome_data) {
  microbiome_samples <- as.character(microbiome_data@sample_info$sample_id)
  metabolome_samples <- as.character(metabolome_data@sample_info$sample_id)
  shared_samples <- intersect(microbiome_samples, metabolome_samples)
  normalize_crossomics_sample_link(
    data.frame(
      microbiome_sample_id = shared_samples,
      metabolome_sample_id = shared_samples,
      pair_id = shared_samples,
      stringsAsFactors = FALSE
    )
  )
}


prune_massdataset_samples <- function(object, sample_ids) {
  sample_ids <- as.character(sample_ids)
  sample_info <- object@sample_info
  variable_info <- object@variable_info
  sample_info_note <- object@sample_info_note
  variable_info_note <- object@variable_info_note
  expression_data <- object@expression_data
  annotation_table <- object@annotation_table
  ms2_data <- object@ms2_data
  
  sample_info <- sample_info[match(sample_ids, sample_info$sample_id), , drop = FALSE]
  expression_data <- expression_data[, sample_ids, drop = FALSE]
  
  new_object <- massdataset::create_mass_dataset(
    expression_data = expression_data,
    sample_info = sample_info,
    variable_info = variable_info,
    sample_info_note = sample_info_note,
    variable_info_note = variable_info_note
  )
  
  if (!is.null(annotation_table)) {
    new_object@annotation_table <- annotation_table
  }
  if (!is.null(ms2_data)) {
    new_object@ms2_data <- ms2_data
  }
  if (methods::.hasSlot(object, "process_info")) {
    new_object@process_info <- object@process_info
  }
  new_object
}


align_crossomics_samples <- function(microbiome_data,
                                     metabolome_data,
                                     sample_link) {
  sample_link <- normalize_crossomics_sample_link(sample_link)
  microbiome_data <- prune_samples(microbiome_data, sample_id = sample_link$microbiome_sample_id)
  metabolome_data <- prune_massdataset_samples(metabolome_data, sample_ids = sample_link$metabolome_sample_id)
  
  sample_link <- sample_link %>%
    dplyr::filter(
      .data$microbiome_sample_id %in% microbiome_data@sample_info$sample_id,
      .data$metabolome_sample_id %in% metabolome_data@sample_info$sample_id
    )
  
  list(
    microbiome_data = microbiome_data,
    metabolome_data = metabolome_data,
    sample_link = sample_link
  )
}


append_crossomics_process_parameter <- function(object,
                                                process_name,
                                                function_name,
                                                parameter = list()) {
  process_info <- slot(object, name = "process_info")
  new_parameter <- new(
    Class = "tidymass_parameter",
    pacakge_name = "microbiomedataset",
    function_name = function_name,
    parameter = parameter,
    time = Sys.time()
  )
  if (all(names(process_info) != process_name)) {
    process_info[[process_name]] <- new_parameter
  } else {
    process_info[[process_name]] <- c(process_info[[process_name]], new_parameter)
  }
  slot(object, "process_info") <- process_info
  object
}
