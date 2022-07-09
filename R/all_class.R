#' @importClassesFrom tidytree treedata
setClassUnion("treedata_or_null", c("treedata", "NULL"))

#' @importClassesFrom Biostrings XStringSet
setClassUnion("xstringset_or_null", c("XStringSet", "NULL"))

#' @title microbiome_dataset class
#' @docType class
#' @slot otu_tree A treedata object of tidytree package or NULL.
#' @slot taxa_tree A treedata object of tidytree package or NULL.
#' @slot ref_seq A XStringSet object of Biostrings package or NULL.
#' @slot ... Other slots from \code{\link[massdataset:mass_dataset]{mass_dataset}}
#' @importClassesFrom massdataset mass_dataset
#' @exportClass microbiome_dataset
setClass(
  "microbiome_dataset",
  contains = "mass_dataset",
  slots    = c(
    otu_tree  = "treedata_or_null",
    taxa_tree = "treedata_or_null",
    ref_seq = "xstringset_or_null"
  ),
  prototype = list(
    otu_tree  = NULL,
    taxa_tree = NULL,
    ref_seq   = NULL
  )
)


#' @title create_microbiome_dataset
#' @description Create the microbiome_dataset object.
#' @docType methods
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param expression_data MS1 peak table name.
#' \url{https://tidymicrobiome.github.io/microbiomedataset/articles/data_import_and_export.html}
#' @param sample_info Sample information name.
#' \url{https://tidymicrobiome.github.io/microbiomedataset/articles/data_import_and_export.html}
#' @param variable_info MS1 peak table name.
#' Columns are samples and rows are variables.
#' @param otu_tree otu_tree
#' @param taxa_tree taxa_tree
#' @param ref_seq ref_seq
#' \url{https://tidymicrobiome.github.io/microbiomedataset/articles/data_import_and_export.html}
#' @param sample_info_note Sample information name.
#' \url{https://tidymicrobiome.github.io/microbiomedataset/articles/data_import_and_export.html}
#' @param variable_info_note Sample information name.
#' \url{https://tidymicrobiome.github.io/microbiomedataset/articles/data_import_and_export.html}
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
           ref_seq,
           sample_info_note,
           variable_info_note) {
    if (!missing(expression_data)) {
      expression_data <-
        as.data.frame(expression_data)
    }
    
    if (!missing(sample_info)) {
      sample_info <-
        as.data.frame(sample_info)
    }
    
    if (!missing(variable_info)) {
      variable_info <-
        as.data.frame(variable_info)
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
        as.data.frame(variable_info_note)
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
    
    check_result <-
      check_microbiome_dataset(
        expression_data = expression_data,
        sample_info = sample_info,
        variable_info = variable_info,
        otu_tree = otu_tree,
        taxa_tree = taxa_tree,
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
    sample_info <- as.data.frame(sample_info)
    variable_info <- as.data.frame(variable_info)
    sample_info_note <- as.data.frame(sample_info_note)
    variable_info_note <- as.data.frame(variable_info_note)
    
    object <- new(
      Class = "microbiome_dataset",
      expression_data = expression_data,
      sample_info = sample_info,
      variable_info = variable_info,
      sample_info_note = sample_info_note,
      variable_info_note = variable_info_note,
      otu_tree  = otu_tree,
      taxa_tree = taxa_tree,
      ref_seq = ref_seq,
      process_info = process_info,
      # version = as.character(utils::packageVersion(pkg = "microbiomedataset"))
      version = "0.99.1"
    )
    invisible(object)
  }
