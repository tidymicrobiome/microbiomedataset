#' Validate the Format and Consistency of a Microbiome Dataset
#'
#' This function checks the structure and consistency of various components of a 
#' microbiome dataset, including expression data, sample information, variable information, 
#' and their respective notes.
#'
#' @param expression_data A data frame representing expression data.
#' @param sample_info A data frame containing sample information.
#' @param variable_info A data frame containing variable information.
#' @param otu_tree An optional parameter for OTU tree data.
#' @param taxa_tree An optional parameter for taxa tree data.
#' @param otu_tree_link An optional explicit mapping between `variable_id` and
#' `otu_tree` labels.
#' @param taxa_tree_link An optional explicit mapping between `variable_id` and
#' `taxa_tree` labels.
#' @param ref_seq An optional parameter for reference sequence data.
#' @param sample_info_note A data frame containing notes on sample information.
#' @param variable_info_note A data frame containing notes on variable information.
#'
#' @details The function performs a series of checks to ensure:
#'   - Required data frames (expression_data, sample_info, variable_info) are provided.
#'   - All data frames are of the correct class.
#'   - Key columns exist and there are no duplicated items in critical fields.
#'   - The dimensions of the data frames are consistent with each other.
#' Errors in these checks will result in descriptive error messages.
#'
#' @return A string "all good." if all checks pass, otherwise returns a descriptive 
#' error message indicating the issue found.
#'
#' @export
#' @examples
#' data("global_patterns", package = "microbiomedataset")
#'
#' check_microbiome_dataset(
#'   expression_data = global_patterns@expression_data,
#'   sample_info = global_patterns@sample_info,
#'   variable_info = global_patterns@variable_info,
#'   otu_tree = global_patterns@otu_tree,
#'   taxa_tree = global_patterns@taxa_tree,
#'   ref_seq = global_patterns@ref_seq,
#'   sample_info_note = global_patterns@sample_info_note,
#'   variable_info_note = global_patterns@variable_info_note
#' )
#' @author Xiaotao Shen \email{xiaotao.shen@@outlook.com}
check_microbiome_dataset <-
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
    taxonomy_ranks <- microbiome_taxonomy_ranks()
    
    if (missing(expression_data) |
        missing(sample_info) | missing(variable_info)) {
      check_result = "error: expression_data,
      sample_info and variable_info should be provided."
      return(check_result)
    }
    
    if (all(!stringr::str_detect(class(expression_data), "data.frame"))) {
      check_result <- "error: expression_data must be a data.frame."
      return(check_result)
    }
    
    if (all(!stringr::str_detect(class(variable_info), "data.frame"))) {
      check_result = "error: variable_info must be a data.frame."
      return(check_result)
    } else{
      variable_info <- normalize_variable_info_schema(variable_info)
      
      if (all(colnames(variable_info) != "variable_id")) {
        check_result = "error: variable_info must have variable_id."
        return(check_result)
      }
      
      if (sum(duplicated(variable_info$variable_id)) > 0) {
        check_result = "error: variable_id has duplicated items."
        return(check_result)
      }
      
      taxonomy_columns <- intersect(taxonomy_ranks, colnames(variable_info))
      
      if (length(taxonomy_columns) > 0) {
        taxonomy_table <- variable_info[, taxonomy_columns, drop = FALSE]
        if (nrow(taxonomy_table) > 0 &&
            any(vapply(taxonomy_table,
                       function(x)
                         !is.character(x) && !is.factor(x),
                       FUN.VALUE = logical(1)))) {
          check_result <- "error: taxonomy columns in variable_info must be character-like."
          return(check_result)
        }
      }
      
    }
    
    if (all(!stringr::str_detect(class(sample_info), "data.frame"))) {
      check_result = "error: sample_info must be a data.frame."
      return(check_result)
    } else{
      sample_info <- normalize_sample_info_schema(sample_info)
      
      if (all(colnames(sample_info) != "sample_id")) {
        check_result = "error: sample_info must have sample_id."
        return(check_result)
      }
      
      if (sum(duplicated(sample_info$sample_id)) > 0) {
        check_result = "error: sample_id has duplicated items."
        return(check_result)
      }
      
      if (all(colnames(sample_info) != "class")) {
        check_result = "error: sample_info must have class."
        return(check_result)
      }
    }
    
    if (!missing(sample_info_note)) {
      if (all(!stringr::str_detect(class(sample_info_note), "data.frame"))) {
        check_result = "error: sample_info_note must be a data.frame."
        return(check_result)
      } else{
        if (all(colnames(sample_info_note) != "name")) {
          check_result = "error: sample_info_note must have column: name."
          return(check_result)
        }
        
        if (all(colnames(sample_info_note) != "meaning")) {
          check_result = "error: sample_info_note must have column: meaning"
          return(check_result)
        }
      }
    }
    
    if (!missing(variable_info_note)) {
      if (all(!stringr::str_detect(class(variable_info_note), "data.frame"))) {
        check_result = "error: variable_info_note must be a data.frame."
        return(check_result)
      } else{
        variable_info_note <- normalize_variable_info_note_schema(variable_info_note)
        
        if (all(colnames(variable_info_note) != "name")) {
          check_result = "error: variable_info_note must have column: name."
          return(check_result)
        }
        
        if (all(colnames(variable_info_note) != "meaning")) {
          check_result = "error: variable_info_note must have column: meaning"
          return(check_result)
        }
        
        if (sum(duplicated(variable_info_note$name)) > 0) {
          check_result = "error: variable_info_note$name has duplicated items."
          return(check_result)
        }
      }
    }
    
    if (ncol(expression_data) != nrow(sample_info)) {
      check_result = "error: expression_data's column number should be same with sample_info's row number."
      return(check_result)
    } else{
      if (sum(colnames(expression_data) != sample_info$sample_id) > 0) {
        check_result = "error: expression_data's column names must be identical with sample_info's sample_id."
        return(check_result)
      }
    }
    
    if (nrow(expression_data) != nrow(variable_info)) {
      check_result = "error: expression_data's row number should be same with variable_info's row number."
      return(check_result)
    } else{
      if (sum(rownames(expression_data) != variable_info$variable_id) > 0) {
        check_result = "error: expression_data's row names must be identical with variable_info's variable_id"
        return(check_result)
      }
    }
    
    if (!missing(sample_info_note)) {
      if (ncol(sample_info) != nrow(sample_info_note)) {
        check_result = "error: sample_info's column number should be same with sample_info_note's row number."
        return(check_result)
      } else{
        if (sum(colnames(sample_info) != sample_info_note$name) > 0) {
          check_result = "error: sample_info's column names must be identical with sample_info_note's name."
          return(check_result)
        }
      }
    }
    
    if (!missing(variable_info_note)) {
      if (ncol(variable_info) != nrow(variable_info_note)) {
        check_result = "error: variable_info's column number should be same with variable_info_note's row number."
        return(check_result)
      } else{
        if (sum(colnames(variable_info) != variable_info_note$name) > 0) {
          check_result = "error: variable_info's column names must be identical with variable_info_note's name."
          return(check_result)
        }
      }
    }
    
    if (!missing(otu_tree) && !is.null(otu_tree)) {
      if (!methods::is(otu_tree, "treedata")) {
        check_result <- "error: otu_tree must be a tidytree::treedata object or NULL."
        return(check_result)
      }
      otu_tips <- get_tree_tip_labels(otu_tree)
      if (!is.null(otu_tips) &&
          !all(as.character(otu_tips) %in% as.character(variable_info$variable_id))) {
        check_result <- "error: otu_tree tip labels must be contained in variable_info$variable_id."
        return(check_result)
      }
    }
    
    if (!missing(otu_tree_link) && !is.null(otu_tree_link)) {
      otu_tree_link <- normalize_tree_link_table(otu_tree_link, tree = "otu_tree")
      if (anyDuplicated(otu_tree_link$variable_id) > 0) {
        return("error: otu_tree_link$variable_id must be unique.")
      }
      if (!all(otu_tree_link$variable_id %in% variable_info$variable_id)) {
        return("error: otu_tree_link$variable_id must exist in variable_info$variable_id.")
      }
      if (is.null(otu_tree)) {
        return("error: otu_tree_link requires otu_tree.")
      }
      otu_tree_table <- as.data.frame(tidytree::as_tibble(otu_tree))
      otu_tips <- get_tree_tip_labels(otu_tree)
      if (!all(otu_tree_link$node %in% otu_tree_table$node)) {
        return("error: otu_tree_link$node must exist in otu_tree nodes.")
      }
      if (!all(otu_tree_link$node_label %in% otu_tips)) {
        return("error: otu_tree_link$node_label must exist in otu_tree tip labels.")
      }
      if (!setequal(otu_tree_link$node_label, otu_tips)) {
        return("error: otu_tree_link must cover all otu_tree tip labels.")
      }
    }
    
    if (!missing(taxa_tree) && !is.null(taxa_tree)) {
      if (!methods::is(taxa_tree, "treedata")) {
        check_result <- "error: taxa_tree must be a tidytree::treedata object or NULL."
        return(check_result)
      }
      if (ncol(get_taxonomy_table(variable_info)) == 0) {
        check_result <- "error: taxa_tree requires taxonomy columns in variable_info."
        return(check_result)
      }
    }
    
    if (!missing(taxa_tree_link) && !is.null(taxa_tree_link)) {
      taxa_tree_link <- normalize_tree_link_table(taxa_tree_link, tree = "taxa_tree")
      if (!all(taxa_tree_link$variable_id %in% variable_info$variable_id)) {
        return("error: taxa_tree_link$variable_id must exist in variable_info$variable_id.")
      }
      if (is.null(taxa_tree)) {
        return("error: taxa_tree_link requires taxa_tree.")
      }
      taxa_tree_table <- as.data.frame(tidytree::as_tibble(taxa_tree))
      taxa_labels <- get_tree_node_labels(taxa_tree)
      if (!all(taxa_tree_link$node %in% taxa_tree_table$node)) {
        return("error: taxa_tree_link$node must exist in taxa_tree nodes.")
      }
      if (!all(taxa_tree_link$node_label %in% taxa_labels)) {
        return("error: taxa_tree_link$node_label must exist in taxa_tree node labels.")
      }
    }
    
    if (!missing(ref_seq) && !is.null(ref_seq)) {
      if (!methods::is(ref_seq, "XStringSet")) {
        check_result <- "error: ref_seq must be a Biostrings::XStringSet object or NULL."
        return(check_result)
      }
      
      if (length(ref_seq) != nrow(variable_info)) {
        check_result <- "error: ref_seq length must equal the number of variables."
        return(check_result)
      }
      
      ref_names <- names(ref_seq)
      if (is.null(ref_names) ||
          !setequal(as.character(ref_names), as.character(variable_info$variable_id))) {
        check_result <- "error: ref_seq names must match variable_info$variable_id."
        return(check_result)
      }
    }
    
    return("all good.")
  }


#' Check Integrity and Structure of Microbiome Dataset Object
#'
#' Performs a series of checks on a microbiome dataset object to ensure its
#' integrity and correct structure. This includes verifying the format of
#' various components such as sample_info, variable_info, and expression_data.
#'
#' @param object A microbiome dataset object to be checked.
#'
#' @details The function performs checks to ensure:
#'   - `variable_info` has a `variable_id` column and no duplicated variable IDs.
#'   - `sample_info` has `sample_id` and `class` columns with no duplicated sample IDs.
#'   - `sample_info_note` and `variable_info_note` have appropriate columns if they exist.
#'   - Consistency between the `expression_data` and the `sample_info` and `variable_info`.
#'   - Consistency between `sample_info`, `variable_info` and their respective notes.
#' Any issues found are returned as character strings in a vector.
#'
#' @return TRUE if the dataset passes all checks, otherwise returns a character
#' vector listing the found errors.
#'
#' @export
#' @examples
#' data("global_patterns", package = "microbiomedataset")
#'
#' check_microbiome_dataset_class(global_patterns)
#' @author Xiaotao Shen \email{xiaotao.shen@@outlook.com}
check_microbiome_dataset_class <-
  function(object) {
    errors <- character()
    sample_info <- normalize_sample_info_schema(object@sample_info)
    variable_info <- normalize_variable_info_schema(object@variable_info)
    variable_info_note <- normalize_variable_info_note_schema(object@variable_info_note)
    
    if (all(colnames(variable_info) != "variable_id")) {
      msg <- "variable_info must have variable_id."
      errors <- c(errors, msg)
    }
    
    if (sum(duplicated(variable_info$variable_id)) > 0) {
      msg <- "variable_id has duplicated items."
      errors <- c(errors, msg)
    }
    
    if (all(colnames(sample_info) != "sample_id")) {
      msg <- "sample_info must have sample_id."
      errors <- c(errors, msg)
    }
    
    if (sum(duplicated(sample_info$sample_id)) > 0) {
      msg <- "sample_id has duplicated items."
      errors <- c(errors, msg)
    }
    
    if (all(colnames(sample_info) != "class")) {
      msg <- "sample_info must have class."
      errors <- c(errors, msg)
    }
    
    if (nrow(object@sample_info_note) > 0) {
      if (all(colnames(object@sample_info_note) != "name")) {
        msg <- "sample_info_note must have column: name."
        errors <- c(errors, msg)
      }
      
      if (all(colnames(object@sample_info_note) != "meaning")) {
        msg <- "sample_info_note must have column: meaning"
        errors <- c(errors, msg)
      }
    }
    
    if (nrow(variable_info_note) > 0) {
      if (all(colnames(variable_info_note) != "name")) {
        msg <- "variable_info_note must have column: name."
        errors <- c(errors, msg)
      }
      
      if (all(colnames(variable_info_note) != "meaning")) {
        msg <- "variable_info_note must have column: meaning"
        errors <- c(errors, msg)
      }
      
      if (sum(duplicated(variable_info_note$name)) > 0) {
        msg <- "variable_info_note$name has duplicated items."
        errors <- c(errors, msg)
      }
    }
    
    if (ncol(object@expression_data) != nrow(object@sample_info)) {
      msg <-
        "expression_data's column number should be same with sample_info's row number."
      errors <- c(errors, msg)
    } else{
      if (sum(colnames(object@expression_data) != object@sample_info$sample_id) > 0) {
        msg <-
          "expression_data's column names must be identical with sample_info's sample_id."
        errors <- c(errors, msg)
      }
    }
    
    if (nrow(object@expression_data) != nrow(variable_info)) {
      msg <-
        "expression_data's row number should be same with variable_info's row number."
      errors <- c(errors, msg)
    } else{
      if (sum(rownames(object@expression_data) != variable_info$variable_id) > 0) {
        msg <-
          "expression_data's row names must be identical with variable_info's variable_id"
        errors <- c(errors, msg)
      }
    }
    
    if (nrow(object@sample_info_note) > 0) {
      if (ncol(sample_info) != nrow(object@sample_info_note)) {
        msg <-
          "sample_info's column number should be same with sample_info_note's row number."
        errors <- c(errors, msg)
      } else{
        if (sum(colnames(sample_info) != object@sample_info_note$name) > 0) {
          msg <-
            "sample_info's column names must be identical with sample_info_note's name."
          errors <- c(errors, msg)
        }
      }
    }
    
    if (nrow(variable_info_note) > 0) {
      if (ncol(variable_info) != nrow(variable_info_note)) {
        msg <-
          "variable_info's column number should be same with variable_info_note's row number."
        errors <- c(errors, msg)
      } else{
        if (sum(colnames(variable_info) != variable_info_note$name) > 0) {
          msg <-
            "variable_info's column names must be identical with variable_info_note's name."
          errors <- c(errors, msg)
        }
      }
    }
    
    if (!is.null(object@otu_tree)) {
      otu_tips <- get_tree_tip_labels(object@otu_tree)
      if (!is.null(otu_tips) &&
          !all(as.character(otu_tips) %in% as.character(variable_info$variable_id))) {
        errors <- c(errors, "otu_tree tip labels must be contained in variable_info$variable_id.")
      }
    }
    
    if (methods::.hasSlot(object, "otu_tree_link") && !is.null(object@otu_tree_link)) {
      otu_tree_link <- normalize_tree_link_table(object@otu_tree_link, tree = "otu_tree")
      if (anyDuplicated(otu_tree_link$variable_id) > 0) {
        errors <- c(errors, "otu_tree_link$variable_id must be unique.")
      }
      if (!all(otu_tree_link$variable_id %in% variable_info$variable_id)) {
        errors <- c(errors, "otu_tree_link$variable_id must exist in variable_info$variable_id.")
      }
      if (is.null(object@otu_tree)) {
        errors <- c(errors, "otu_tree_link requires otu_tree.")
      } else {
        otu_tree_table <- as.data.frame(tidytree::as_tibble(object@otu_tree))
        otu_tips <- get_tree_tip_labels(object@otu_tree)
        if (!all(otu_tree_link$node %in% otu_tree_table$node)) {
          errors <- c(errors, "otu_tree_link$node must exist in otu_tree nodes.")
        }
        if (!all(otu_tree_link$node_label %in% otu_tips)) {
          errors <- c(errors, "otu_tree_link$node_label must exist in otu_tree tip labels.")
        }
      }
      if (!setequal(otu_tree_link$node_label, otu_tips)) {
        errors <- c(errors, "otu_tree_link must cover all otu_tree tip labels.")
      }
    }
    
    if (!is.null(object@taxa_tree) && ncol(get_taxonomy_table(variable_info)) == 0) {
      errors <- c(errors, "taxa_tree requires taxonomy columns in variable_info.")
    }
    
    if (methods::.hasSlot(object, "taxa_tree_link") && !is.null(object@taxa_tree_link)) {
      taxa_tree_link <- normalize_tree_link_table(object@taxa_tree_link, tree = "taxa_tree")
      if (!all(taxa_tree_link$variable_id %in% variable_info$variable_id)) {
        errors <- c(errors, "taxa_tree_link$variable_id must exist in variable_info$variable_id.")
      }
      if (is.null(object@taxa_tree)) {
        errors <- c(errors, "taxa_tree_link requires taxa_tree.")
      } else {
        taxa_tree_table <- as.data.frame(tidytree::as_tibble(object@taxa_tree))
        taxa_labels <- get_tree_node_labels(object@taxa_tree)
        if (!all(taxa_tree_link$node %in% taxa_tree_table$node)) {
          errors <- c(errors, "taxa_tree_link$node must exist in taxa_tree nodes.")
        }
        if (!all(taxa_tree_link$node_label %in% taxa_labels)) {
          errors <- c(errors, "taxa_tree_link$node_label must exist in taxa_tree node labels.")
        }
      }
    }
    
    if (!is.null(object@ref_seq)) {
      ref_names <- names(object@ref_seq)
      if (length(object@ref_seq) != nrow(variable_info)) {
        errors <- c(errors, "ref_seq length must equal the number of variables.")
      }
      if (is.null(ref_names) ||
          !setequal(as.character(ref_names), as.character(variable_info$variable_id))) {
        errors <- c(errors, "ref_seq names must match variable_info$variable_id.")
      }
    }
    
    if (length(errors) == 0)
      TRUE
    else
      errors
  }

massdataset::check_mass_dataset_class
massdataset::check_mass_dataset
