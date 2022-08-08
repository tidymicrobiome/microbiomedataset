####merge samples
#' @title merge samples
#' @param object microbiome_dataset
#' @param what which you want to use
#' @param variable_id variables should be merged
#' @param variable_index variables should be merged
#' @param remain_variable_info_id which variable information should be used.
#' @param remain_variable_info_index which variable information should be used.
#' @param group_by group all variables by column in variable_info
#' @param ... other params
#' @return microbiome_dataset
#' @export
summarise_variables <-
  function(object,
           what = c("sum_intensity",
                    "mean_intensity",
                    "median_intensity"),
           variable_id,
           variable_index,
           remain_variable_info_id,
           remain_variable_info_index,
           group_by,
           ...) {
    UseMethod("summarise_variables")
  }

#' @rdname summarise_variables
#' @export
summarize_variables <- summarise_variables

#' @method summarise_variables microbiome_dataset
#' @rdname summarise_variables
#' @importFrom phyloseq sample_data tax_table phyloseq otu_table
#' @importFrom tibble column_to_rownames
#' @importFrom massdataset check_column_name
#' @export
#' @examples
#' data(global_patterns)
#' dim(global_patterns)
#' global_patterns
#' 
#' global_patterns <-
#'   global_patterns %>%
#'   activate_microbiome_dataset(what = "variable_info") %>%
#'   dplyr::filter(!is.na(Genus))
#' 
#' object <-
#'   global_patterns %>%
#'   summarize_variables(what = "sum_intensity", group_by = "Genus")
#' 
#' dim(object)


summarise_variables.microbiome_dataset <-
  function(object,
           what = c("sum_intensity",
                    "mean_intensity",
                    "median_intensity"),
           variable_id,
           variable_index,
           remain_variable_info_id,
           remain_variable_info_index,
           group_by,
           ...) {
    what <-
      match.arg(what)
    ####if group_by is provided
    variable_info <-
      extract_variable_info(object)
    
    sample_info <-
      extract_sample_info(object)
    
    expression_data <-
      extract_expression_data(object)
    
    if (!group_by %in% colnames(variable_info)) {
      stop(group_by, " must be in the variable_info.")
    }
    
    variable_id <- get_variable_id(object)
    sample_id <- get_sample_id(object)
    
    variable_id2 <-
      variable_info %>%
      dplyr::pull(group_by) %>%
      as.character()
    
    if (sum(is.na(variable_id2)) > 0) {
      stop(group_by, " contains NA.")
    }
    
    expression_data2 <-
      unique(variable_id2) %>%
      purrr::map(function(x) {
        temp <-
          expression_data[which(variable_id2 == x), , drop = FALSE] %>%
          apply(2, function(y) {
            calculate(y, what = what)
          })
        temp
      }) %>%
      dplyr::bind_rows() %>%
      as.data.frame()
    
    rownames(expression_data2) <-
      unique(variable_id2)
    
    variable_info2 <-
      variable_info
    
    variable_info2 <-
      variable_info2 %>%
      dplyr::distinct(!!!syms(group_by), .keep_all = TRUE) %>%
      as.data.frame()
    
    rownames(expression_data2) <-
      variable_info2$variable_id
    
    process_info <-
      slot(object, name = "process_info")
    
    parameter <- new(
      Class = "tidymass_parameter",
      pacakge_name = "microbiomedataset",
      function_name = "summarise_variables()",
      parameter = list("what" = what,
                       "group_by" = group_by),
      time = Sys.time()
    )
    
    if (all(names(process_info) != "summarise_variables")) {
      process_info$summarise_variables <- parameter
    } else{
      process_info$summarise_variables <-
        c(process_info$summarise_variables,
          parameter)
    }
    slot(object, "process_info") <- process_info
    slot(object, "variable_info") <- variable_info2
    slot(object, "expression_data") <- expression_data2
    return(object)
    
    ###if don't provide the group_by
    if (missing(variable_id) & missing(variable_index)) {
      return(object)
    }
    
    if (missing(remain_variable_info_id) &
        missing(remain_variable_info_index)) {
      remain_variable_info_index <- 1
    }
    
    variable_info <-
      extract_variable_info(object)
    
    sample_info <-
      extract_sample_info(object)
    
    expression_data <-
      extract_expression_data(object)
    
    if (!missing(variable_id)) {
      variable_index <- match(variable_id, variable_info$variable_id)
    }
    
    variable_index <-
      variable_index[!is.na(variable_index)]
    
    variable_index <-
      variable_index[variable_index %in% seq_len(nrow(variable_info))]
    
    if (length(variable_index) == 0) {
      return(object)
    }
    
    if (!missing(remain_variable_info_id)) {
      remain_variable_info_index <-
        match(remain_variable_info_id, variable_id)
    }
    
    remain_variable_info_index <-
      remain_variable_info_index[remain_variable_info_index %in% seq_along(variable_index)]
    
    variable_id <- get_variable_id(object)
    sample_id <- get_sample_id(object)
    
    expression_data1 <-
      expression_data[variable_index,] %>%
      apply(2, function(x) {
        calculate(x, what = what)
      }) %>%
      data.frame() %>%
      t() %>%
      as.data.frame()
    
    rownames(expression_data1) <-
      variable_info$variable_id[remain_variable_info_index]
    
    expression_data2 <-
      expression_data[-variable_index,]
    
    expression_data <-
      rbind(expression_data1,
            expression_data2) %>%
      as.data.frame()
    
    variable_info <-
      variable_info[match(rownames(expression_data), variable_info$variable_id), ]
    
    process_info <-
      slot(object, name = "process_info")
    
    parameter <- new(
      Class = "tidymass_parameter",
      pacakge_name = "microbiomedataset",
      function_name = "summarise_variables()",
      parameter = list(
        "what" = what,
        "variable_index" = variable_index,
        remain_variable_info_index = remain_variable_info_index
      ),
      time = Sys.time()
    )
    
    if (all(names(process_info) != "summarise_variables")) {
      process_info$summarise_variables <- parameter
    } else{
      process_info$summarise_variables <-
        c(process_info$summarise_variables,
          parameter)
    }
    slot(object, "process_info") <- process_info
    slot(object, "variable_info") <- variable_info
    slot(object, "expression_data") <- expression_data
    return(object)
  }
