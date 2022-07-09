#' @title check_microbiome_dataset_class
#' @description Check microbiome_dataset class object.
#' @docType methods
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param object microbiome_dataset class.
#' @return Notice of data checking.
#' @export

check_microbiome_dataset_class <-
  function(object) {
    errors <- character()
    ##check variable_info format
    if (all(colnames(object@variable_info) != "variable_id")) {
      msg <- "variable_info must have variable_id."
      errors <- c(errors, msg)
    }
    
    if (sum(duplicated(object@variable_info$variable_id)) > 0) {
      msg <- "variable_id has duplicated items."
      errors <- c(errors, msg)
    }
    
    ##check sample_info format
    if (all(colnames(object@sample_info) != "sample_id")) {
      msg <- "sample_info must have sample_id."
      errors <- c(errors, msg)
    }
    
    if (sum(duplicated(object@sample_info$sample_id)) > 0) {
      msg <- "sample_id has duplicated items."
      errors <- c(errors, msg)
    }
    
    if (all(colnames(object@sample_info) != "class")) {
      msg <- "sample_info must have class."
      errors <- c(errors, msg)
    }
    
    # if (any(is.na(object@sample_info$class))) {
    #   msg <- "class should not have NA."
    #   errors <- c(errors, msg)
    # }
    
    ##check sample_info_note format
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
    
    ##check variable_info_note format
    if (nrow(object@variable_info_note) > 0) {
      if (all(colnames(object@variable_info_note) != "name")) {
        msg <- "variable_info_note must have column: name."
        errors <- c(errors, msg)
      }
      
      if (all(colnames(object@variable_info_note) != "meaning")) {
        msg <- "variable_info_note must have column: meaning"
        errors <- c(errors, msg)
      }
    }
    
    ###expression_data and sample_info
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
    
    ###expression_data and variable_info
    if (nrow(object@expression_data) != nrow(object@variable_info)) {
      msg <-
        "expression_data's row number should be same with variable_info's row number."
      errors <- c(errors, msg)
    } else{
      if (sum(rownames(object@expression_data) != object@variable_info$variable_id) > 0) {
        msg <-
          "expression_data's row names must be identical with variable_info's variable_id"
        errors <- c(errors, msg)
      }
    }
    
    
    ###sample_info and sample_info_note
    if (nrow(object@sample_info_note) > 0) {
      if (ncol(object@sample_info) != nrow(object@sample_info_note)) {
        msg <-
          "sample_info's column number should be same with sample_info_note's row number."
        errors <- c(errors, msg)
      } else{
        if (sum(colnames(object@sample_info) != object@sample_info_note$name) > 0) {
          msg <-
            "sample_info's column names must be identical with sample_info_note's name."
          errors <- c(errors, msg)
        }
      }
    }
    
    ###variable_info and variable_info_note
    if (nrow(object@variable_info_note) > 0) {
      if (ncol(object@variable_info) != nrow(object@variable_info_note)) {
        msg <-
          "variable_info's column number should be same with variable_info_note's row number."
        errors <- c(errors, msg)
      } else{
        if (sum(colnames(object@variable_info) != object@variable_info_note$name) > 0) {
          msg <-
            "variable_info's column names must be identical with variable_info_note's name."
          errors <- c(errors, msg)
        }
      }
    }
    
    if (length(errors) == 0)
      TRUE
    else
      errors
  }


massdataset::check_mass_dataset_class
massdataset::check_mass_dataset
