#' @title convert to mass_dataset class object.
#' @param object microbiomedataset
#' @param ... other params
#' @return mass_dataset class object.
#' @export
convert2mass_dataset <- function(object, ...) {
  UseMethod("convert2mass_dataset")
}

#' @rdname convert2mass_dataset
#' @export
as.mass_dataset <- convert2mass_dataset

#' @method convert2mass_dataset microbiome_dataset
#' @rdname convert2mass_dataset
#' @importFrom tibble column_to_rownames
#' @importFrom massdataset create_mass_dataset
#' @export
convert2mass_dataset.microbiome_dataset <-
  function(object,
           ...) {
    new_object <-
      massdataset::create_mass_dataset(
        expression_data = object@expression_data,
        sample_info = object@sample_info,
        variable_info = object@variable_info,
        sample_info_note = object@sample_info_note,
        variable_info_note = object@variable_info_note
      )
    
    return(new_object)
  }


#' @title convert to TreeSummarizedExperiment class object.
#' @param object microbiomedataset
#' @param assay_name assay name used in the output object.
#' @param ... other params
#' @return TreeSummarizedExperiment class object.
#' @export
#' @examples
#' if (requireNamespace("TreeSummarizedExperiment", quietly = TRUE)) {
#'   data("global_patterns", package = "microbiomedataset")
#'   x0 <- prune_taxa(global_patterns, variable_id = global_patterns@variable_info$variable_id[1:80])
#'   x <- convert2TreeSummarizedExperiment(x0)
#'   class(x)
#' }
convert2TreeSummarizedExperiment <- function(object,
                                             ...) {
  if (methods::is(object, "microbiome_dataset")) {
    return(convert2TreeSummarizedExperiment.microbiome_dataset(object, ...))
  }
  UseMethod("convert2TreeSummarizedExperiment")
}

#' @rdname convert2TreeSummarizedExperiment
#' @export
as.TreeSummarizedExperiment <- convert2TreeSummarizedExperiment

#' @method convert2TreeSummarizedExperiment microbiome_dataset
#' @rdname convert2TreeSummarizedExperiment
#' @export
convert2TreeSummarizedExperiment.microbiome_dataset <-
  function(object,
           assay_name = "counts",
           ...) {
    if (!requireNamespace("TreeSummarizedExperiment", quietly = TRUE)) {
      stop("Package 'TreeSummarizedExperiment' is required.")
    }
    
    assay_data <- as.matrix(extract_expression_data(object))
    sample_info <- as.data.frame(extract_sample_info(object))
    variable_info <- as.data.frame(extract_variable_info(object))
    
    rownames(sample_info) <- sample_info$sample_id
    rownames(variable_info) <- variable_info$variable_id
    
    row_tree <- NULL
    row_node_lab <- NULL
    otu_tree_link <- NULL
    if (methods::.hasSlot(object, "otu_tree_link")) {
      otu_tree_link <- object@otu_tree_link
    } else if (!is.null(object@otu_tree)) {
      otu_tree_link <- build_otu_tree_link(variable_info, object@otu_tree)
    }
    taxa_tree_link <- NULL
    if (methods::.hasSlot(object, "taxa_tree_link")) {
      taxa_tree_link <- object@taxa_tree_link
    } else if (!is.null(object@taxa_tree)) {
      taxa_tree_link <- build_taxa_tree_link(variable_info, object@taxa_tree)
    }
    if (!is.null(object@otu_tree)) {
      row_tree <- tidytree::as.phylo(object@otu_tree)
      if (!is.null(otu_tree_link)) {
        row_node_lab <- otu_tree_link$node_label[match(variable_info$variable_id, otu_tree_link$variable_id)]
      } else {
        row_node_lab <- variable_info$variable_id
      }
    }
    
    reference_seq <- object@ref_seq
    if (!is.null(reference_seq) && is.null(names(reference_seq))) {
      names(reference_seq) <- variable_info$variable_id
    }
    
    metadata_list <- list(
      taxa_tree = object@taxa_tree,
      otu_tree_link = otu_tree_link,
      taxa_tree_link = taxa_tree_link,
      process_info = object@process_info
    )
    
    tse <- suppressWarnings(
      TreeSummarizedExperiment::TreeSummarizedExperiment(
        assays = setNames(list(assay_data), assay_name),
        rowData = S4Vectors::DataFrame(variable_info),
        colData = S4Vectors::DataFrame(sample_info),
        rowTree = row_tree,
        rowNodeLab = row_node_lab,
        referenceSeq = reference_seq,
        metadata = metadata_list
      )
    )
    
    tse
  }


#' @title convert to MPSE class object.
#' @param object microbiomedataset
#' @param ... other params
#' @return MPSE class object.
#' @export
#' @examples
#' if (requireNamespace("MicrobiotaProcess", quietly = TRUE)) {
#'   data("global_patterns", package = "microbiomedataset")
#'   x <- convert2MPSE(global_patterns)
#'   class(x)
#' }
convert2MPSE <- function(object,
                         ...) {
  if (methods::is(object, "microbiome_dataset")) {
    return(convert2MPSE.microbiome_dataset(object, ...))
  }
  UseMethod("convert2MPSE")
}

#' @rdname convert2MPSE
#' @export
as.MPSE <- convert2MPSE

#' @method convert2MPSE microbiome_dataset
#' @rdname convert2MPSE
#' @export
convert2MPSE.microbiome_dataset <-
  function(object,
           ...) {
    if (!requireNamespace("MicrobiotaProcess", quietly = TRUE)) {
      stop("Package 'MicrobiotaProcess' is required.")
    }
    
    phyloseq_object <- convert2phyloseq(object)
    
    if (exists("as.MPSE", where = asNamespace("MicrobiotaProcess"), mode = "function")) {
      getExportedValue("MicrobiotaProcess", "as.MPSE")(phyloseq_object, ...)
    } else {
      stop("MicrobiotaProcess::as.MPSE() is not available.")
    }
  }


#' @title convert to microbiome_dataset class object.
#' @param object phylosep or other class
#' @param assay_name Assay name used when `object` is a
#'   `TreeSummarizedExperiment`.
#' @param ... other params
#' @return microbiome_dataset class object.
#' @export
#' @examples
#' data("global_patterns", package = "microbiomedataset")
#'
#' x0 <- prune_taxa(global_patterns, variable_id = global_patterns@variable_info$variable_id[1:80])
#' phyloseq_object <- convert2phyloseq(x0)
#' x <- convert2microbiome_dataset(phyloseq_object)
#' class(x)
convert2microbiome_dataset <- function(object, ...) {
  if (methods::is(object, "TreeSummarizedExperiment")) {
    return(convert2microbiome_dataset.TreeSummarizedExperiment(object, ...))
  }
  if (methods::is(object, "MPSE")) {
    return(convert2microbiome_dataset.MPSE(object, ...))
  }
  UseMethod("convert2microbiome_dataset")
}

#' @rdname convert2microbiome_dataset
#' @export
as.microbiome_dataset <- convert2microbiome_dataset

#' @method convert2microbiome_dataset phyloseq
#' @rdname convert2microbiome_dataset
#' @importFrom phyloseq sample_data tax_table phyloseq otu_table
#' @importFrom tibble column_to_rownames
#' @export
convert2microbiome_dataset.phyloseq <-
  function(object,
           ...) {
    otu_table <- phyloseq::otu_table(object = object)
    expression_data <- as(otu_table, "matrix")
    if (!phyloseq::taxa_are_rows(otu_table)) {
      expression_data <- t(expression_data)
    }
    expression_data <- as.data.frame(expression_data)
    
    sample_info <-
      tryCatch(
        phyloseq::sample_data(object = object),
        error = function(e)
          NULL
      )
    
    if (is.null(sample_info)) {
      sample_info <-
        data.frame(sample_id = colnames(expression_data),
                   class = "Subject")
    } else{
      sample_info <- data.frame(sample_info, check.names = FALSE)
      sample_info$sample_id <- rownames(sample_info)
      sample_info <- sample_info %>%
        dplyr::select(sample_id, dplyr::everything())
      if (!"class" %in% colnames(sample_info)) {
        sample_info$class <- "Subject"
      } else{
        sample_info$class[is.na(sample_info$class)] <- "Subject"
      }
      rownames(sample_info) <- NULL
    }
    
    variable_info <-
      tryCatch(
        phyloseq::tax_table(object = object),
        error = function(e)
          NULL
      )
    
    if (is.null(variable_info)) {
      variable_info <-
        data.frame(variable_id = rownames(expression_data))
    } else{
      new_variable_info <-
        as(variable_info, "matrix") %>%
        as.data.frame() %>%
        tibble::rownames_to_column(var = "variable_id") %>%
        dplyr::select(variable_id, dplyr::everything())
      
      variable_info <-
        normalize_variable_info_schema(new_variable_info)
    }
    
    taxa_tree <- NULL
    if (!is.null(phyloseq::tax_table(object = object)) &&
        ncol(as(phyloseq::tax_table(object = object), "matrix")) != 0) {
      taxa_tree <- convert2treedata(get_taxonomy_table(variable_info))
    }
    
    otu_tree <- NULL
    if (!is.null(object@phy_tree)) {
      otu_tree <-
        tryCatch(
          object@phy_tree %>%
            tidytree::as.treedata(),
          error = function(e)
            NULL
        )
    }
    
    ref_seq <- NULL
    if (methods::.hasSlot(object, "refseq") && !is.null(object@refseq)) {
      ref_seq <- object@refseq
      if (is.null(names(ref_seq))) {
        names(ref_seq) <- variable_info$variable_id
      }
    }
    
    if (is.null(rownames(expression_data))) {
      rownames(expression_data) <- variable_info$variable_id
    }
    if (is.null(colnames(expression_data))) {
      colnames(expression_data) <- sample_info$sample_id
    }
    
    new_object <-
      create_microbiome_dataset(
        expression_data = expression_data,
        sample_info = sample_info,
        variable_info = variable_info,
        otu_tree = otu_tree,
        taxa_tree = taxa_tree,
        ref_seq = ref_seq
      )
    return(new_object)
  }

#' @method convert2microbiome_dataset mass_dataset
#' @rdname convert2microbiome_dataset
#' @importFrom tibble column_to_rownames
#' @export
convert2microbiome_dataset.mass_dataset <-
  function(object,
           ...) {
    new_object <-
      create_microbiome_dataset(
        expression_data = object@expression_data,
        sample_info = object@sample_info,
        variable_info = object@variable_info
      )
    return(new_object)
  }


#' @method convert2microbiome_dataset TreeSummarizedExperiment
#' @rdname convert2microbiome_dataset
#' @export
convert2microbiome_dataset.TreeSummarizedExperiment <-
  function(object,
           assay_name = NULL,
           ...) {
    if (!requireNamespace("SummarizedExperiment", quietly = TRUE)) {
      stop("Package 'SummarizedExperiment' is required.")
    }
    
    if (is.null(assay_name)) {
      assay_names <- SummarizedExperiment::assayNames(object)
      assay_name <- if ("counts" %in% assay_names) "counts" else assay_names[[1]]
    }
    
    expression_data <- as.data.frame(
      as.matrix(SummarizedExperiment::assay(object, assay_name))
    )
    
    sample_info <- as.data.frame(SummarizedExperiment::colData(object))
    sample_info$sample_id <- rownames(sample_info)
    sample_info <- sample_info %>%
      dplyr::select(sample_id, dplyr::everything())
    if (!"class" %in% colnames(sample_info)) {
      sample_info$class <- "Subject"
    }
    
    variable_info <- as.data.frame(SummarizedExperiment::rowData(object))
    variable_info$variable_id <- rownames(variable_info)
    variable_info <- variable_info %>%
      dplyr::select(variable_id, dplyr::everything())
    variable_info <- normalize_variable_info_schema(variable_info)
    
    rownames(expression_data) <- variable_info$variable_id
    colnames(expression_data) <- sample_info$sample_id
    
    otu_tree <- NULL
    if (requireNamespace("TreeSummarizedExperiment", quietly = TRUE)) {
      row_tree <- tryCatch(
        TreeSummarizedExperiment::rowTree(object),
        error = function(e) NULL
      )
      if (!is.null(row_tree)) {
        otu_tree <- tryCatch(
          tidytree::as.treedata(row_tree),
          error = function(e) NULL
        )
      }
    }
    
    metadata_list <- S4Vectors::metadata(object)
    taxa_tree <- metadata_list$taxa_tree
    if (!is.null(taxa_tree) && inherits(taxa_tree, "phylo")) {
      taxa_tree <- tidytree::as.treedata(taxa_tree)
    }
    otu_tree_link <- metadata_list$otu_tree_link
    taxa_tree_link <- metadata_list$taxa_tree_link
    
    ref_seq <- NULL
    if (requireNamespace("TreeSummarizedExperiment", quietly = TRUE)) {
      ref_seq <- tryCatch(
        TreeSummarizedExperiment::referenceSeq(object),
        error = function(e) NULL
      )
    }
    
    create_microbiome_dataset(
      expression_data = expression_data,
      sample_info = sample_info,
      variable_info = variable_info,
      otu_tree = otu_tree,
      taxa_tree = taxa_tree,
      otu_tree_link = otu_tree_link,
      taxa_tree_link = taxa_tree_link,
      ref_seq = ref_seq
    )
  }


#' @method convert2microbiome_dataset MPSE
#' @rdname convert2microbiome_dataset
#' @export
convert2microbiome_dataset.MPSE <-
  function(object,
           ...) {
    if (!requireNamespace("MicrobiotaProcess", quietly = TRUE)) {
      stop("Package 'MicrobiotaProcess' is required.")
    }
    
    if (exists("as.phyloseq", where = asNamespace("MicrobiotaProcess"), mode = "function")) {
      phyloseq_object <- getExportedValue("MicrobiotaProcess", "as.phyloseq")(object)
      return(convert2microbiome_dataset(phyloseq_object))
    }
    
    stop("MicrobiotaProcess::as.phyloseq() is not available.")
  }


#' @title convert to phyloseq class object.
#' @param object microbiomedataset
#' @param ... other params
#' @return phyloseq class object.
#' @export
#' @examples
#' data("global_patterns", package = "microbiomedataset")
#'
#' x <- convert2phyloseq(global_patterns)
#' class(x)
convert2phyloseq <- function(object, ...) {
  UseMethod("convert2phyloseq")
}

#' @rdname convert2phyloseq
#' @export
as.phyloseq <- convert2phyloseq

#' @method convert2phyloseq microbiome_dataset
#' @rdname convert2phyloseq
#' @importFrom phyloseq sample_data tax_table phyloseq otu_table
#' @importFrom tibble column_to_rownames
#' @export
convert2phyloseq.microbiome_dataset <-
  function(object,
           ...) {
    taxonomy_ranks <- microbiome_taxonomy_ranks()
    
    otu_data <-
      extract_expression_data(object) %>%
      as.matrix() %>%
      phyloseq::otu_table(taxa_are_rows = TRUE)
    
    sample_info <- extract_sample_info(object)
    rownames(sample_info) <- NULL
    sample_data <-
      sample_info %>%
      tibble::column_to_rownames(var = "sample_id") %>%
      phyloseq::sample_data()
    
    variable_info <- extract_variable_info(object)
    taxonomy_table <- complete_taxonomy_table(variable_info, ranks = taxonomy_ranks)
    rownames(taxonomy_table) <- variable_info$variable_id
    taxa_data <-
      taxonomy_table %>%
      as.matrix() %>%
      phyloseq::tax_table()
    
    if (!is.null(object@otu_tree)) {
      otu_tree <-
        object@otu_tree
      phy_tree <-
        tidytree::as.phylo(x = otu_tree)
    } else{
      phy_tree <- NULL
    }
    
    phyloseq_components <- list(otu_data, sample_data, taxa_data)
    
    if (!is.null(phy_tree)) {
      phyloseq_components <- c(phyloseq_components, list(phy_tree))
    }
    
    if (!is.null(object@ref_seq)) {
      ref_seq <- object@ref_seq
      if (is.null(names(ref_seq))) {
        names(ref_seq) <- variable_info$variable_id
      }
      phyloseq_components <- c(phyloseq_components, list(ref_seq))
    }
    
    new_object <-
      do.call(phyloseq::phyloseq, phyloseq_components)
    
    return(new_object)
  }


#' Import a BIOM Table as a microbiome_dataset
#'
#' @param file A BIOM file path or a `biomformat::biom` object.
#' @param sample_info Optional sample metadata table.
#' @param variable_info Optional feature metadata table.
#' @param ... Passed to `create_microbiome_dataset()`.
#'
#' @return A `microbiome_dataset` object.
#' @export
#' @examples
#' if (requireNamespace("biomformat", quietly = TRUE)) {
#'   counts <- matrix(
#'     c(10, 0, 3, 8),
#'     nrow = 2,
#'     dimnames = list(c("OTU1", "OTU2"), c("Sample1", "Sample2"))
#'   )
#'   biom <- biomformat::make_biom(data = counts)
#'   x <- import_biom(biom)
#'   dim(x@expression_data)
#' }
import_biom <-
  function(file,
           sample_info = NULL,
           variable_info = NULL,
           ...) {
    if (!requireNamespace("biomformat", quietly = TRUE)) {
      stop("Package 'biomformat' is required.")
    }
    
    biom_object <- if (inherits(file, "biom")) {
      file
    } else {
      biomformat::read_biom(file)
    }
    
    expression_data <- as.data.frame(as.matrix(biomformat::biom_data(biom_object)))
    
    if (is.null(sample_info)) {
      sample_info <- biomformat::sample_metadata(biom_object)
      sample_info <- as.data.frame(sample_info, check.names = FALSE)
      sample_info$sample_id <- rownames(sample_info)
      sample_info <- sample_info %>%
        dplyr::select(sample_id, dplyr::everything())
    }
    if (!"class" %in% colnames(sample_info)) {
      sample_info$class <- "Subject"
    }
    
    if (is.null(variable_info)) {
      variable_info <- biomformat::observation_metadata(biom_object)
      variable_info <- as.data.frame(variable_info, check.names = FALSE)
      variable_info$variable_id <- rownames(variable_info)
      variable_info <- variable_info %>%
        dplyr::select(variable_id, dplyr::everything())
      variable_info <- normalize_variable_info_schema(variable_info)
    }
    
    rownames(expression_data) <- variable_info$variable_id
    colnames(expression_data) <- sample_info$sample_id
    
    create_microbiome_dataset(
      expression_data = expression_data,
      sample_info = sample_info,
      variable_info = variable_info,
      ...
    )
  }


#' Import a QIIME2 Dataset as a microbiome_dataset
#'
#' @param feature_table Feature table `.qza` path or loaded matrix-like object.
#' @param taxonomy Optional taxonomy `.qza` path or data frame.
#' @param metadata Optional sample metadata path or data frame.
#' @param tree Optional tree `.qza` path, `phylo`, or `treedata`.
#' @param ref_seq Optional sequence `.qza` path or `DNAStringSet`.
#' @param ... Passed to `create_microbiome_dataset()`.
#'
#' @return A `microbiome_dataset` object.
#' @export
import_qiime2 <-
  function(feature_table,
           taxonomy = NULL,
           metadata = NULL,
           tree = NULL,
           ref_seq = NULL,
           ...) {
    if (!requireNamespace("qiime2R", quietly = TRUE)) {
      stop("Package 'qiime2R' is required.")
    }
    
    read_qza_data <- function(x) {
      if (is.character(x) && length(x) == 1L && file.exists(x)) {
        qiime2R::read_qza(x)$data
      } else {
        x
      }
    }
    
    expression_data <- read_qza_data(feature_table)
    expression_data <- as.data.frame(as.matrix(expression_data))
    
    if (is.null(metadata)) {
      sample_info <- data.frame(
        sample_id = colnames(expression_data),
        class = "Subject"
      )
    } else {
      sample_info <- if (is.character(metadata) && length(metadata) == 1L && file.exists(metadata)) {
        qiime2R::read_q2metadata(metadata)
      } else {
        as.data.frame(metadata, check.names = FALSE)
      }
      sample_info$sample_id <- rownames(sample_info)
      sample_info <- sample_info %>%
        dplyr::select(sample_id, dplyr::everything())
      if (!"class" %in% colnames(sample_info)) {
        sample_info$class <- "Subject"
      }
    }
    
    if (is.null(taxonomy)) {
      variable_info <- data.frame(variable_id = rownames(expression_data))
    } else {
      taxonomy_data <- read_qza_data(taxonomy)
      variable_info <- as.data.frame(taxonomy_data, check.names = FALSE)
      if (!"variable_id" %in% colnames(variable_info)) {
        variable_info$variable_id <- rownames(variable_info)
      }
      variable_info <- variable_info %>%
        dplyr::select(variable_id, dplyr::everything())
      variable_info <- normalize_variable_info_schema(variable_info)
    }
    
    otu_tree <- read_qza_data(tree)
    if (inherits(otu_tree, "phylo")) {
      otu_tree <- tidytree::as.treedata(otu_tree)
    }
    
    ref_seq <- read_qza_data(ref_seq)
    
    create_microbiome_dataset(
      expression_data = expression_data,
      sample_info = sample_info,
      variable_info = variable_info,
      otu_tree = otu_tree,
      ref_seq = ref_seq,
      ...
    )
  }


#' Import DADA2-style Outputs as a microbiome_dataset
#'
#' @param seqtab A DADA2 sequence table with features in rows or columns.
#' @param taxonomy_table Optional taxonomy table.
#' @param sample_info Optional sample metadata table.
#' @param taxa_are_rows Whether `seqtab` already has taxa in rows.
#' @param otu_tree Optional OTU tree as `phylo` or `treedata`.
#' @param taxa_tree Optional taxonomy tree as `phylo` or `treedata`.
#' @param ref_seq Optional `DNAStringSet`.
#' @param ... Passed to `create_microbiome_dataset()`.
#'
#' @return A `microbiome_dataset` object.
#' @export
#' @examples
#' seqtab <- matrix(
#'   c(10, 0, 3, 8),
#'   nrow = 2,
#'   dimnames = list(c("ASV1", "ASV2"), c("Sample1", "Sample2"))
#' )
#' x <- import_dada2(seqtab = seqtab, taxa_are_rows = TRUE)
#' dim(x@expression_data)
import_dada2 <-
  function(seqtab,
           taxonomy_table = NULL,
           sample_info = NULL,
           taxa_are_rows = TRUE,
           otu_tree = NULL,
           taxa_tree = NULL,
           ref_seq = NULL,
           ...) {
    expression_data <- as.matrix(seqtab)
    if (!taxa_are_rows) {
      expression_data <- t(expression_data)
    }
    expression_data <- as.data.frame(expression_data)
    
    if (is.null(sample_info)) {
      sample_info <- data.frame(
        sample_id = colnames(expression_data),
        class = "Subject"
      )
    } else {
      sample_info <- as.data.frame(sample_info, check.names = FALSE)
      if (!"sample_id" %in% colnames(sample_info)) {
        sample_info$sample_id <- rownames(sample_info)
      }
      sample_info <- sample_info %>%
        dplyr::select(sample_id, dplyr::everything())
      if (!"class" %in% colnames(sample_info)) {
        sample_info$class <- "Subject"
      }
    }
    
    if (is.null(taxonomy_table)) {
      variable_info <- data.frame(variable_id = rownames(expression_data))
    } else {
      variable_info <- as.data.frame(taxonomy_table, check.names = FALSE)
      if (!"variable_id" %in% colnames(variable_info)) {
        variable_info$variable_id <- rownames(variable_info)
      }
      variable_info <- variable_info %>%
        dplyr::select(variable_id, dplyr::everything())
      variable_info <- normalize_variable_info_schema(variable_info)
    }
    
    if (inherits(otu_tree, "phylo")) {
      otu_tree <- tidytree::as.treedata(otu_tree)
    }
    if (inherits(taxa_tree, "phylo")) {
      taxa_tree <- tidytree::as.treedata(taxa_tree)
    }
    
    create_microbiome_dataset(
      expression_data = expression_data,
      sample_info = sample_info,
      variable_info = variable_info,
      otu_tree = otu_tree,
      taxa_tree = taxa_tree,
      ref_seq = ref_seq,
      ...
    )
  }


#' @title convert to tbl_graph class object in tidygraph.
#' @param object microbiomedataset or Taxa table
#' @param intensity_method for each node, the how to calculate the intensity for
#' it accross all the samples
#' @param ... other params
#' @return tbl_graph class object.
#' @export
convert2tbl_graph <-
  function(object,
           intensity_method = c("sum", "mean", "median"),
           ...) {
    UseMethod("convert2tbl_graph")
  }

#' @rdname convert2tbl_graph
#' @export
as.tbl_graph <- convert2tbl_graph

#' @method convert2tbl_graph microbiome_dataset
#' @rdname convert2tbl_graph
#' @importFrom tidygraph tbl_graph as_tbl_graph
#' @importFrom tidygraph tbl_graph
#' @importFrom tibble column_to_rownames rownames_to_column
#' @export
convert2tbl_graph.microbiome_dataset <-
  function(object,
           intensity_method = c("sum", "mean", "median"),
           ...) {
    intensity_method <- match.arg(intensity_method)
    variable_info <-
      extract_variable_info(object)
    
    variable_info <-
      remove_na_from_taxa_table(variable_info)
    
    kingdowm_intensity <-
      extract_intensity(
        object = object,
        sample_wise = "sample_id",
        taxonomic_rank = "Kingdom",
        data_type = "wider",
        na.rm = TRUE,
        relative = FALSE
      ) %>%
      apply(1, switch(
        intensity_method,
        "sum" = sum,
        "mean" = mean,
        "median" = median,
      ))
    
    names(kingdowm_intensity) <-
      paste0("k__", names(kingdowm_intensity))
    
    phylum_intensity <-
      extract_intensity(
        object = object,
        sample_wise = "sample_id",
        taxonomic_rank = "Phylum",
        data_type = "wider",
        na.rm = TRUE,
        relative = FALSE
      ) %>%
      apply(1, switch(
        intensity_method,
        "sum" = sum,
        "mean" = mean,
        "median" = median,
      ))
    
    names(phylum_intensity) <-
      paste0("p__", names(phylum_intensity))
    
    class_intensity <-
      extract_intensity(
        object = object,
        sample_wise = "sample_id",
        taxonomic_rank = "Class",
        data_type = "wider",
        na.rm = TRUE,
        relative = FALSE
      ) %>%
      apply(1, switch(
        intensity_method,
        "sum" = sum,
        "mean" = mean,
        "median" = median,
      ))
    
    names(class_intensity) <-
      paste0("c__", names(class_intensity))
    
    order_intensity <-
      extract_intensity(
        object = object,
        sample_wise = "sample_id",
        taxonomic_rank = "Order",
        data_type = "wider",
        na.rm = TRUE,
        relative = FALSE
      ) %>%
      apply(1, switch(
        intensity_method,
        "sum" = sum,
        "mean" = mean,
        "median" = median,
      ))
    
    names(order_intensity) <-
      paste0("o__", names(order_intensity))
    
    family_intensity <-
      extract_intensity(
        object = object,
        sample_wise = "sample_id",
        taxonomic_rank = "Family",
        data_type = "wider",
        na.rm = TRUE,
        relative = FALSE
      ) %>%
      apply(1, switch(
        intensity_method,
        "sum" = sum,
        "mean" = mean,
        "median" = median,
      ))
    
    names(family_intensity) <-
      paste0("f__", names(family_intensity))
    
    genus_intensity <-
      extract_intensity(
        object = object,
        sample_wise = "sample_id",
        taxonomic_rank = "Genus",
        data_type = "wider",
        na.rm = TRUE,
        relative = FALSE
      ) %>%
      apply(1, switch(
        intensity_method,
        "sum" = sum,
        "mean" = mean,
        "median" = median,
      ))
    
    names(genus_intensity) <-
      paste0("g__", names(genus_intensity))
    
    species_intensity <-
      extract_intensity(
        object = object,
        sample_wise = "sample_id",
        taxonomic_rank = "Species",
        data_type = "wider",
        na.rm = TRUE,
        relative = FALSE
      ) %>%
      apply(1, switch(
        intensity_method,
        "sum" = sum,
        "mean" = mean,
        "median" = median,
      ))
    
    names(species_intensity) <-
      paste0("s__", names(species_intensity))
    
    root_intensity <-
      c("Root" = sum(kingdowm_intensity))
    
    node_data <-
      data.frame(
        intensity = c(
          root_intensity,
          kingdowm_intensity,
          phylum_intensity,
          class_intensity,
          order_intensity,
          family_intensity,
          genus_intensity,
          species_intensity
        )
      ) %>%
      tibble::rownames_to_column(var = "node")
    
    temp_data <-
      variable_info %>%
      dplyr::select(Kingdom, Phylum, Class, Order, Family, Genus, Species) %>%
      dplyr::mutate(Root = "Root") %>%
      dplyr::select(Root, dplyr::everything())
    
    temp_data$Kingdom <- paste0("k__",  temp_data$Kingdom)
    temp_data$Phylum <- paste0("p__",  temp_data$Phylum)
    temp_data$Class <- paste0("c__",  temp_data$Class)
    temp_data$Order <- paste0("o__",  temp_data$Order)
    temp_data$Family <- paste0("f__",  temp_data$Family)
    temp_data$Genus <- paste0("g__",  temp_data$Genus)
    temp_data$Species <- paste0("s__",  temp_data$Species)
    
    edge_data <-
      seq_len(ncol(temp_data) - 1) %>%
      purrr::map(function(i) {
        temp <-
          temp_data[, c(i, i + 1)] %>%
          dplyr::distinct()
        colnames(temp) <- c("from", "to")
        temp
      }) %>%
      dplyr::bind_rows() %>%
      dplyr::filter(!stringr::str_detect(from, "__NA") &
                      !stringr::str_detect(to, "__NA"))
    
    node_data <-
      node_data %>%
      dplyr::filter(node %in% c(edge_data$from, edge_data$to)) %>%
      dplyr::mutate(index = dplyr::row_number())
    
    edge_data <-
      edge_data %>%
      dplyr::filter(from %in% node_data$node &
                      to %in% node_data$node)
    
    result <-
      tidygraph::tbl_graph(nodes = node_data, edges = edge_data)
    
    return(result)
  }


####some code from Guangchuang Yu, credites should go there
#' @title convert data.frame contained hierarchical relationship or other classes to treedata class
#' @param object taxonomyTable class (phyloseq package) or other class (data.frame)
#' @param type type
#' @param ... other params
#' @return treedata class object (ggtree package).
#' @export
convert2treedata <-
  function(object,
           type = "species",
           ...) {
    UseMethod("convert2treedata")
  }

#' @rdname convert2treedata
#' @export
as.treedata <- convert2treedata

####some code is from MicrobiotaProcess, credit should go its developer
#' @method convert2treedata data.frame
#' @rdname convert2treedata
#' @importFrom phyloseq sample_data tax_table phyloseq otu_table
#' @importFrom phyloseq sample_data tax_table phyloseq otu_table
#' @importClassesFrom phyloseq taxonomyTable
#' @importFrom tibble column_to_rownames
#' @importFrom tidytree treedata
#' @export
convert2treedata.data.frame <-
  function(object,
           type = "species",
           ...) {
    data <- as.data.frame(object)
    
    if (!"fill_taxa_table_na" %in% names(attributes(data))) {
      data <- fill_taxa_table_na(data, type = type)
    }
    
    data$variable_id <- rownames(data)
    
    data <-
      data %>%
      dplyr::mutate(Root = "r__root") %>%
      dplyr::select(Root, dplyr::everything())
    
    if (!"fill_taxa_table_na" %in% names(attributes(data))) {
      data <- fill_taxa_table_na(data, type = type)
    }
    
    new_data <-
      seq_len(ncol(data) - 1) %>%
      purrr::map(function(i) {
        temp_data <-
          data[, c(i, i + 1)] %>%
          dplyr::mutate(nodeClass = colnames(data)[i + 1],
                        nodeDepth = i) %>%
          dplyr::distinct()
        colnames(temp_data)[1:2] <- c("parent", "child")
        temp_data
      }) %>%
      dplyr::bind_rows() %>%
      dplyr::distinct()
    
    isTip <-
      !as.vector(new_data$child) %in% as.vector(new_data$parent)
    index <- rep(NA, length(isTip))
    index[isTip] <- seq(1, sum(isTip))
    index[!isTip] <- seq(sum(isTip) + 2, length(isTip) + 1)
    mapping <-
      data.frame(node = index,
                 labelnames = as.vector(new_data$child),
                 isTip)
    indxx <- match(mapping$labelnames, new_data$child)
    mapping$nodeClass <- new_data[indxx, "nodeClass"]
    mapping$nodeDepth <- new_data[indxx, "nodeDepth"]
    parentnode <-
      mapping[match(as.vector(new_data$parent), as.vector(mapping$labelnames)),]$node
    childnode <-
      mapping[match(as.vector(new_data$child), as.vector(mapping$labelnames)),]$node
    edges <- cbind(parentnode, childnode)
    colnames(edges) <- NULL
    edges[is.na(edges)] <- sum(isTip) + 1
    root <- data.frame(
      node = sum(isTip) + 1,
      labelnames = "r__root",
      isTip = FALSE,
      nodeClass = "Root",
      nodeDepth = 0
    )
    mapping <- rbind(root, mapping)
    mapping <- mapping[order(mapping$node),]
    node.label <- as.vector(mapping$labelnames)[!mapping$isTip]
    tip.label <- as.vector(mapping$labelnames)[mapping$isTip]
    mapping <-
      mapping[, colnames(mapping) %in% c("node", "nodeClass", "nodeDepth")]
    taxphylo <-
      structure(
        list(
          edge = edges,
          node.label = node.label,
          tip.label = tip.label,
          Nnode = length(node.label)
        ),
        class = "phylo"
      )
    
    result <-
      tidytree::treedata(phylo = taxphylo,
                         data = as_tibble(mapping))
    return(result)
  }

#' @method convert2treedata taxonomyTable
#' @rdname convert2treedata
#' @importFrom phyloseq sample_data tax_table phyloseq otu_table
#' @importFrom phyloseq sample_data tax_table phyloseq otu_table
#' @importClassesFrom phyloseq taxonomyTable
#' @importFrom tibble column_to_rownames
#' @importFrom tidytree treedata
#' @export
convert2treedata.taxonomyTable <-
  function(object,
           type = "species",
           ...) {
    convert2treedata.data.frame(object = object,
                                type = type,
                                ...)
  }
