#' @importClassesFrom tidytree treedata
setClassUnion("treedata_or_null", c("treedata", "NULL"))

#' @importClassesFrom Biostrings XStringSet
setClassUnion("xstringset_or_null", c("XStringSet", "NULL"))

setClassUnion("data.frame_or_null", c("data.frame", "NULL"))

#' @title microbiome_dataset class
#' @docType class
#' @slot otu_tree A treedata object of tidytree package or NULL.
#' @slot taxa_tree A treedata object of tidytree package or NULL.
#' @slot otu_tree_link A data.frame linking `variable_id` to `otu_tree` nodes.
#' @slot taxa_tree_link A data.frame linking `variable_id` to `taxa_tree` nodes.
#' @slot ref_seq A XStringSet object of Biostrings package or NULL.
#' @slot ... Other slots from mass_dataset class object in massdataset pacakge
#' @importClassesFrom massdataset mass_dataset
#' @exportClass microbiome_dataset
setClass(
  "microbiome_dataset",
  contains = "mass_dataset",
  slots    = c(
    otu_tree  = "treedata_or_null",
    taxa_tree = "treedata_or_null",
    otu_tree_link = "data.frame_or_null",
    taxa_tree_link = "data.frame_or_null",
    ref_seq = "xstringset_or_null"
  ),
  prototype = list(
    otu_tree  = NULL,
    taxa_tree = NULL,
    otu_tree_link = NULL,
    taxa_tree_link = NULL,
    ref_seq   = NULL
  )
)


#' @title microbiome_diversity class
#' @docType class
#' @slot result A data.frame or matrix containing diversity results.
#' @slot type Diversity type, such as alpha or beta.
#' @slot method Method used for diversity calculation.
#' @exportClass microbiome_diversity
setClass(
  "microbiome_diversity",
  slots = c(
    result = "ANY",
    type = "character",
    method = "character"
  )
)


#' @title microbiome_ordination class
#' @docType class
#' @slot sample_coord A data.frame of sample coordinates.
#' @slot eigenvalues Numeric vector of eigenvalues or explained variance.
#' @slot method Ordination method.
#' @slot distance_method Distance method used, if applicable.
#' @slot feature_loading A data.frame of feature loadings, if available.
#' @exportClass microbiome_ordination
setClass(
  "microbiome_ordination",
  slots = c(
    sample_coord = "data.frame",
    eigenvalues = "numeric",
    method = "character",
    distance_method = "character",
    feature_loading = "data.frame"
  )
)


setMethod(
  f = "show",
  signature = "microbiome_diversity",
  definition = function(object) {
    cat(crayon::green("microbiome_diversity\n"))
    cat("Type: ", object@type, "\n", sep = "")
    cat("Method: ", object@method, "\n", sep = "")
    if (inherits(object@result, "dist")) {
      labels <- attr(object@result, "Labels")
      cat("Samples: ", length(labels), "\n", sep = "")
    } else {
      cat("Rows: ", nrow(object@result), "\n", sep = "")
    }
  }
)


setMethod(
  f = "show",
  signature = "microbiome_ordination",
  definition = function(object) {
    cat(crayon::green("microbiome_ordination\n"))
    cat("Method: ", object@method, "\n", sep = "")
    if (nzchar(object@distance_method)) {
      cat("Distance: ", object@distance_method, "\n", sep = "")
    }
    cat("Samples: ", nrow(object@sample_coord), "\n", sep = "")
    cat("Axes: ", sum(grepl("^(Axis\\.|PC)", colnames(object@sample_coord))), "\n", sep = "")
    if (nrow(object@feature_loading) > 0) {
      cat("Feature loadings: ", nrow(object@feature_loading), "\n", sep = "")
    }
  }
)

#' @title create_microbiome_dataset
#' @description Create the microbiome_dataset object.
#' @docType methods
#' @author Xiaotao Shen
#' \email{xiaotao.shen@@outlook.com}
#' @param expression_data MS1 peak table name.
#' @param sample_info Sample information name.
#' @param variable_info MS1 peak table name.
#' Columns are samples and rows are variables.
#' @param otu_tree otu_tree
#' @param taxa_tree taxa_tree
#' @param otu_tree_link explicit mapping between `variable_id` and `otu_tree`.
#' @param taxa_tree_link explicit mapping between `variable_id` and `taxa_tree`.
#' @param ref_seq ref_seq
#' @param sample_info_note Sample information name.
#' @param variable_info_note Sample information name.
#' @return A microbiome_dataset-class object.
#' @importClassesFrom massdataset tidymass_parameter
#' @export
#' @examples
#'
#' expression_data <-
#'   as.data.frame(matrix(
#'     sample(1:100, 100, replace = TRUE),
#'     nrow = 10,
#'     ncol = 10
#'   ))
#'
#' rownames(expression_data) <- paste0("OTU", 1:nrow(expression_data))
#' colnames(expression_data) <-
#'   paste0("Sample", 1:ncol(expression_data))
#' expression_data
#'
#' variable_info <-
#'   as.data.frame(matrix(
#'     sample(letters, 70, replace = TRUE),
#'     nrow = nrow(expression_data),
#'     ncol = 7
#'   ))
#'
#' rownames(variable_info) <- rownames(expression_data)
#' colnames(variable_info) <-
#'   c("Domain",
#'     "Phylum",
#'     "Class",
#'     "Order",
#'     "Family",
#'     "Genus",
#'     "Species")
#'
#' variable_info$variable_id <-
#'   rownames(expression_data)
#'
#' sample_info <-
#'   data.frame(sample_id = colnames(expression_data),
#'              class = "Subject")
#'
#' object <-
#'   create_microbiome_dataset(
#'     expression_data = expression_data,
#'     sample_info = sample_info,
#'     variable_info = variable_info
#'   )
#'
#' object
create_microbiome_dataset <-
  function(expression_data,
           sample_info,
           variable_info,
           otu_tree,
           taxa_tree,
           otu_tree_link,
           taxa_tree_link,
           ref_seq,
           sample_info_note,
           variable_info_note) {
    if (!missing(expression_data)) {
      expression_data <-
        as.data.frame(expression_data)
    }
    
    if (!missing(sample_info)) {
      sample_info <-
        normalize_sample_info_schema(sample_info)
    }
    
    if (!missing(variable_info)) {
      variable_info <-
        normalize_variable_info_schema(variable_info)
    }
    
    if (!missing(sample_info_note)) {
      sample_info_note <-
        as.data.frame(sample_info_note)
    } else{
      sample_info_note <-
        data.frame(
          name = colnames(sample_info),
          meaning = colnames(sample_info),
          check.names = FALSE
        )
    }
    
    if (!missing(variable_info_note)) {
      variable_info_note <-
        normalize_variable_info_note_schema(variable_info_note)
    } else{
      variable_info_note <-
        data.frame(
          name = colnames(variable_info),
          meaning = colnames(variable_info),
          check.names = FALSE
        )
    }
    
    if (missing(otu_tree)) {
      otu_tree <- NULL
    }
    
    if (missing(taxa_tree)) {
      taxa_tree <- NULL
    }
    
    if (missing(ref_seq)) {
      ref_seq <- NULL
    }
    
    if (missing(otu_tree_link)) {
      otu_tree_link <- build_otu_tree_link(variable_info, otu_tree)
    }
    
    if (missing(taxa_tree_link)) {
      taxa_tree_link <- build_taxa_tree_link(variable_info, taxa_tree)
    }
    
    check_result <-
      check_microbiome_dataset(
        expression_data = expression_data,
        sample_info = sample_info,
        variable_info = variable_info,
        otu_tree = otu_tree,
        taxa_tree = taxa_tree,
        otu_tree_link = otu_tree_link,
        taxa_tree_link = taxa_tree_link,
        ref_seq = ref_seq,
        sample_info_note = sample_info_note,
        variable_info_note = variable_info_note
      )
    
    if (stringr::str_detect(check_result, "error")) {
      stop(check_result)
    }
    
    process_info <- list()
    
    parameter <- new(
      Class = "tidymass_parameter",
      pacakge_name = "microbiomedataset",
      function_name = "create_microbiome_dataset()",
      parameter = list("no" = "no"),
      time = Sys.time()
    )
    
    process_info$create_microbiome_dataset = parameter
    
    expression_data <- as.data.frame(expression_data)
    sample_info <- normalize_sample_info_schema(sample_info)
    variable_info <- normalize_variable_info_schema(variable_info)
    sample_info_note <- as.data.frame(sample_info_note)
    variable_info_note <- normalize_variable_info_note_schema(variable_info_note)
    
    object <- new(
      Class = "microbiome_dataset",
      expression_data = expression_data,
      sample_info = sample_info,
      variable_info = variable_info,
      sample_info_note = sample_info_note,
      variable_info_note = variable_info_note,
      otu_tree  = otu_tree,
      taxa_tree = taxa_tree,
      otu_tree_link = otu_tree_link,
      taxa_tree_link = taxa_tree_link,
      ref_seq = ref_seq,
      process_info = process_info,
      version = as.character(utils::packageVersion(pkg = "microbiomedataset"))
    )
    invisible(object)
  }


#' Activate a Specific Component of a Microbiome Dataset
#'
#' This function sets a specific component of a microbiome dataset object as active. 
#' It allows the user to specify which part of the dataset to work with.
#'
#' @param .data The microbiome dataset object.
#' @param what A character string specifying the component to activate. 
#'             Options are "sample_info", "variable_info", or "expression_data". 
#'             Default is "sample_info".
#'
#' @details The function uses the `slot` function to assign the active component to 
#' the 'activated' slot of the microbiome dataset object. This makes it easier to 
#' work with specific parts of the dataset in subsequent analyses.
#'
#' @return The modified microbiome dataset object with the specified component activated.
#'
#' @export
#' @examples
#' data("global_patterns", package = "microbiomedataset")
#'
#' x <- activate_microbiome_dataset(global_patterns, what = "sample_info")
#' x@activated
#' @note It's essential to ensure that the microbiome dataset object has the 
#' necessary structure and slots to use this function effectively.
#'
#' @author Xiaotao Shen \email{xiaotao.shen@@outlook.com}
activate_microbiome_dataset <-
  function(.data,
           what = c("sample_info",
                    "variable_info",
                    "expression_data")) {
    what <- match.arg(what)
    slot(object = .data, name = "activated") <- what
    return(.data)
  }


#' Upgrade a microbiome_dataset Object to the Current Schema
#'
#' Rebuild a `microbiome_dataset` using the current class definition and derive
#' missing explicit tree link metadata.
#'
#' @param object A `microbiome_dataset` object, including objects saved with an
#' older class definition.
#'
#' @return A refreshed `microbiome_dataset` object.
#' @export
#' @examples
#' data("global_patterns", package = "microbiomedataset")
#'
#' x <- update_microbiome_dataset(global_patterns)
#' x@version
update_microbiome_dataset <-
  function(object) {
    if (!methods::is(object, "microbiome_dataset")) {
      stop("object must be a microbiome_dataset.")
    }
    
    otu_tree_link <- if (methods::.hasSlot(object, "otu_tree_link")) object@otu_tree_link else NULL
    taxa_tree_link <- if (methods::.hasSlot(object, "taxa_tree_link")) object@taxa_tree_link else NULL
    
    refreshed <- create_microbiome_dataset(
      expression_data = object@expression_data,
      sample_info = object@sample_info,
      variable_info = object@variable_info,
      otu_tree = object@otu_tree,
      taxa_tree = object@taxa_tree,
      otu_tree_link = otu_tree_link,
      taxa_tree_link = taxa_tree_link,
      ref_seq = object@ref_seq,
      sample_info_note = object@sample_info_note,
      variable_info_note = object@variable_info_note
    )
    
    slot(refreshed, "process_info") <- slot(object, "process_info")
    append_process_parameter(
      object = refreshed,
      process_name = "update_microbiome_dataset",
      function_name = "update_microbiome_dataset()",
      parameter = list(
        upgraded_tree_links = TRUE
      )
    )
  }


setMethod(
  f = "show",
  signature = "microbiome_dataset",
  definition = function(object) {
    check_result = check_microbiome_dataset(
      expression_data = object@expression_data,
      sample_info = object@sample_info,
      variable_info = object@variable_info
    )
    if (check_result != "all good.") {
      cat(crayon::red(check_result, "\n"))
      cat(crayon::red(
        "You may changed the slots, try to use update_mass_dataset().\n"
      ))
    }
    cat(crayon::yellow(paste(rep("-", 20), collapse = ""), "\n"))
    cat(crayon::green("microbiomedataset version:", object@version, "\n"))
    cat(crayon::yellow(paste(rep("-", 20), collapse = ""), "\n"))
    cat(crayon::green("1.expression_data:"))
    cat(
      "[",
      nrow(object@expression_data),
      "x",
      ncol(object@expression_data),
      "data.frame]"
    )
    counts_or_relative <-
      attr(slot(object, "expression_data"), "counts_or_relative")
    if (!is.null(counts_or_relative)) {
      cat("(Relative)\n")
    } else{
      cat("\n")
    }
    cat(crayon::green("2.sample_info:"))
    cat("[",
        nrow(object@sample_info),
        "x",
        ncol(object@sample_info),
        "data.frame]\n")
    cat(crayon::green("3.variable_info:"))
    cat(
      "[",
      nrow(object@variable_info),
      "x",
      ncol(object@variable_info),
      "data.frame]\n"
    )
    cat(crayon::green("4.sample_info_note:"))
    cat(
      "[",
      nrow(object@sample_info_note),
      "x",
      ncol(object@sample_info_note),
      "data.frame]\n"
    )
    cat(crayon::green("5.variable_info_note:"))
    cat(
      "[",
      nrow(object@variable_info_note),
      "x",
      ncol(object@variable_info_note),
      "data.frame]\n"
    )
    cat(crayon::yellow(paste(rep("-", 20), collapse = ""), "\n"))
    cat(crayon::green("Processing information (extract_process_info())\n"))
    if (.hasSlot(object = object, name = "process_info") &
        length(object@process_info) != 0) {
      process_info <- object@process_info
      
      for (idx in seq_along(process_info)) {
        cat(crayon::green(names(process_info)[idx], paste(rep("-", 10), collapse = ""), "\n"))
        if (length(process_info[[idx]]) == 1) {
          data.frame(
            "Package" = process_info[[idx]]@pacakge_name,
            "Function used" = process_info[[idx]]@function_name,
            "Time" = process_info[[idx]]@time
          ) %>%
            print()
        } else{
          data.frame(
            "Package" = process_info[[idx]] %>% lapply(function(x)
              x@pacakge_name) %>% unlist(),
            "Function used" = process_info[[idx]] %>% lapply(function(x)
              x@function_name) %>% unlist(),
            "Time" = process_info[[idx]] %>% lapply(function(x)
              as.character(x@time)) %>% unlist()
          ) %>%
            print()
        }
      }
    } else{
      cat(crayon::red("There are no processing for your data.\n"))
    }
  }
)


#' @title microbiome_metabolome_dataset class
#' @docType class
#' @slot microbiome_data A `microbiome_dataset` object.
#' @slot metabolome_data A `mass_dataset` object.
#' @slot sample_link A data.frame linking microbiome and metabolome samples.
#' @slot feature_link Optional taxon-metabolite link table.
#' @slot pathway_link Optional pathway/reaction link table.
#' @slot process_info Process tracking metadata.
#' @slot version Package version used to build the object.
#' @importClassesFrom massdataset mass_dataset tidymass_parameter
#' @exportClass microbiome_metabolome_dataset
setClass(
  "microbiome_metabolome_dataset",
  slots = c(
    microbiome_data = "microbiome_dataset",
    metabolome_data = "mass_dataset",
    sample_link = "data.frame",
    feature_link = "data.frame_or_null",
    pathway_link = "data.frame_or_null",
    process_info = "list",
    version = "character"
  )
)


#' @title microbe_metabolite_association class
#' @docType class
#' @slot result A data.frame of association results.
#' @slot method Association method.
#' @slot microbiome_rank Taxonomic level used for microbiome features.
#' @slot metabolome_transform Transformation applied to metabolome data.
#' @exportClass microbe_metabolite_association
setClass(
  "microbe_metabolite_association",
  slots = c(
    result = "data.frame",
    method = "character",
    microbiome_rank = "character",
    metabolome_transform = "character"
  )
)


#' @title microbe_metabolite_network class
#' @docType class
#' @slot node_data A data.frame of network nodes.
#' @slot edge_data A data.frame of network edges.
#' @slot method Correlation method used to derive the network.
#' @slot microbiome_rank Taxonomic level used for microbiome features.
#' @slot thresholds A list describing edge filtering thresholds.
#' @exportClass microbe_metabolite_network
setClass(
  "microbe_metabolite_network",
  slots = c(
    node_data = "data.frame",
    edge_data = "data.frame",
    method = "character",
    microbiome_rank = "character",
    thresholds = "list"
  )
)


#' @title differential_abundance_result class
#' @docType class
#' @slot result A data.frame of differential abundance results.
#' @slot method Differential abundance method.
#' @slot taxonomic_rank Taxonomic level used in the result.
#' @slot group Comparison or grouping label.
#' @exportClass differential_abundance_result
setClass(
  "differential_abundance_result",
  slots = c(
    result = "data.frame",
    method = "character",
    taxonomic_rank = "character",
    group = "character"
  )
)


#' @title crossomics_mechanism class
#' @docType class
#' @slot result A data.frame of mechanism link results.
#' @slot method Mechanism inference method.
#' @slot evidence_type Evidence type used in the result.
#' @exportClass crossomics_mechanism
setClass(
  "crossomics_mechanism",
  slots = c(
    result = "data.frame",
    method = "character",
    evidence_type = "character"
  )
)


#' @title crossomics_integration class
#' @docType class
#' @slot sample_coord A data.frame of integration sample coordinates.
#' @slot microbiome_loading A data.frame of microbiome feature loadings.
#' @slot metabolome_loading A data.frame of metabolome feature loadings.
#' @slot sample_info A data.frame of paired sample metadata.
#' @slot method Integration method.
#' @slot microbiome_rank Taxonomic level used for microbiome features.
#' @slot ncomp Number of latent components.
#' @exportClass crossomics_integration
setClass(
  "crossomics_integration",
  slots = c(
    sample_coord = "data.frame",
    microbiome_loading = "data.frame",
    metabolome_loading = "data.frame",
    sample_info = "data.frame",
    method = "character",
    microbiome_rank = "character",
    ncomp = "integer"
  )
)


#' @title longitudinal_mixed_effect_result class
#' @docType class
#' @name mixed_effect_result_class
#' @aliases longitudinal_mixed_effect_result-class
#' @aliases longitudinal_mixed_effect_result
#' @slot result A data.frame of longitudinal mixed-effect results.
#' @slot method Modeling method label.
#' @slot time_variable Time variable used in the model.
#' @slot group_variable Optional grouping variable.
#' @exportClass longitudinal_mixed_effect_result
setClass(
  "longitudinal_mixed_effect_result",
  slots = c(
    result = "data.frame",
    method = "character",
    time_variable = "character",
    group_variable = "character"
  )
)


setMethod(
  f = "show",
  signature = "microbiome_metabolome_dataset",
  definition = function(object) {
    cat(crayon::green("microbiome_metabolome_dataset\n"))
    cat("Samples linked: ", nrow(object@sample_link), "\n", sep = "")
    cat(
      "Microbiome dimensions: ",
      nrow(object@microbiome_data@expression_data),
      " x ",
      ncol(object@microbiome_data@expression_data),
      "\n",
      sep = ""
    )
    cat(
      "Metabolome dimensions: ",
      nrow(object@metabolome_data@expression_data),
      " x ",
      ncol(object@metabolome_data@expression_data),
      "\n",
      sep = ""
    )
    if (!is.null(object@feature_link)) {
      cat("Feature links: ", nrow(object@feature_link), "\n", sep = "")
    }
    if (!is.null(object@pathway_link)) {
      cat("Pathway links: ", nrow(object@pathway_link), "\n", sep = "")
    }
  }
)


setMethod(
  f = "show",
  signature = "microbe_metabolite_association",
  definition = function(object) {
    cat(crayon::green("microbe_metabolite_association\n"))
    cat("Method: ", object@method, "\n", sep = "")
    cat("Microbiome rank: ", object@microbiome_rank, "\n", sep = "")
    cat("Rows: ", nrow(object@result), "\n", sep = "")
  }
)


setMethod(
  f = "show",
  signature = "crossomics_mechanism",
  definition = function(object) {
    cat(crayon::green("crossomics_mechanism\n"))
    cat("Method: ", object@method, "\n", sep = "")
    cat("Evidence type: ", object@evidence_type, "\n", sep = "")
    cat("Rows: ", nrow(object@result), "\n", sep = "")
  }
)


setMethod(
  f = "show",
  signature = "microbe_metabolite_network",
  definition = function(object) {
    cat(crayon::green("microbe_metabolite_network\n"))
    cat("Method: ", object@method, "\n", sep = "")
    cat("Microbiome rank: ", object@microbiome_rank, "\n", sep = "")
    cat("Nodes: ", nrow(object@node_data), "\n", sep = "")
    cat("Edges: ", nrow(object@edge_data), "\n", sep = "")
  }
)


setMethod(
  f = "show",
  signature = "differential_abundance_result",
  definition = function(object) {
    cat(crayon::green("differential_abundance_result\n"))
    cat("Method: ", object@method, "\n", sep = "")
    cat("Taxonomic rank: ", object@taxonomic_rank, "\n", sep = "")
    if (nzchar(object@group)) {
      cat("Group: ", object@group, "\n", sep = "")
    }
    cat("Rows: ", nrow(object@result), "\n", sep = "")
  }
)


setMethod(
  f = "show",
  signature = "crossomics_integration",
  definition = function(object) {
    cat(crayon::green("crossomics_integration\n"))
    cat("Method: ", object@method, "\n", sep = "")
    cat("Microbiome rank: ", object@microbiome_rank, "\n", sep = "")
    cat("Components: ", object@ncomp, "\n", sep = "")
    cat("Samples: ", nrow(object@sample_coord), "\n", sep = "")
    cat("Microbiome loadings: ", nrow(object@microbiome_loading), "\n", sep = "")
    cat("Metabolome loadings: ", nrow(object@metabolome_loading), "\n", sep = "")
  }
)


setMethod(
  f = "show",
  signature = "longitudinal_mixed_effect_result",
  definition = function(object) {
    cat(crayon::green("longitudinal_mixed_effect_result\n"))
    cat("Method: ", object@method, "\n", sep = "")
    cat("Time variable: ", object@time_variable, "\n", sep = "")
    if (nzchar(object@group_variable)) {
      cat("Group variable: ", object@group_variable, "\n", sep = "")
    }
    cat("Rows: ", nrow(object@result), "\n", sep = "")
  }
)
