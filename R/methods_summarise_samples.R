#' Summarize Samples in a Microbiome Dataset
#'
#' This generic function summarizes the expression data in a microbiome dataset object
#' based on specified criteria like mean, median, or sum of intensities, grouped by a specified factor.
#'
#' @param object A microbiome dataset object.
#' @param what A character string specifying the type of summary required. 
#'             Options are "mean_intensity", "median_intensity", or "sum_intensity". 
#'             Default is "mean_intensity".
#' @param group_by A character string specifying the grouping variable.
#' @param ... Additional arguments.
#'
#' @return A modified microbiome dataset object with summarized expression data.
#'
#' @details The `summarise_samples` function is a generic method, which calls the appropriate
#' method (e.g., `summarise_samples.microbiome_dataset`) based on the class of the `object`.
#' It summarizes the expression data based on the given `what` parameter and groups by `group_by`.
#'
#' @export
summarise_samples <-
  function(object,
           what = c("mean_intensity",
                    "median_intensity",
                    "sum_intensity"),
           group_by,
           ...) {
    UseMethod("summarise_samples")
  }

#' @rdname summarise_samples
#' @export
summarize_samples <- summarise_samples

#' Summarize Samples for microbiome_dataset Class
#'
#' Specific implementation of `summarise_samples` for objects of class `microbiome_dataset`.
#' It performs the summarization of expression data based on the specified criteria and grouping.
#'
#' @param object A microbiome dataset object of class `microbiome_dataset`.
#' @return A microbiome dataset object with updated sample_info and expression_data reflecting the summarization.
#' @details This method specifically handles objects of the `microbiome_dataset` class.
#' It applies the summarization as specified by the `what` and `group_by` parameters
#' and updates the object accordingly.
#' @method summarise_samples microbiome_dataset
#' @rdname summarise_samples
#' @importFrom phyloseq sample_data tax_table phyloseq otu_table
#' @importFrom tibble column_to_rownames
#' @importFrom massdataset check_column_name
#' @export

summarise_samples.microbiome_dataset <-
  function(object,
           what = c("sum_intensity",
                    "mean_intensity",
                    "median_intensity"),
           group_by,
           ...) {
    if (missing(group_by)) {
      return(object)
    }
    
    variable_info <-
      extract_variable_info(object)
    
    sample_info <-
      extract_sample_info(object)
    
    expression_data <-
      extract_expression_data(object)
    
    if (!group_by %in% colnames(sample_info)) {
      stop(group_by, " must be in the sample_info.")
    }
    
    what <-
      match.arg(what)
    
    variable_id <- get_variable_id(object)
    sample_id <- get_sample_id(object)
    
    sample_id2 <-
      sample_info %>%
      dplyr::pull(group_by) %>%
      as.character()
    
    if (sum(is.na(sample_id2)) > 0) {
      stop(group_by, " contains NA.")
    }
    
    expression_data2 <-
      unique(sample_id2) %>%
      purrr::map(function(x) {
        temp <-
          expression_data[, which(sample_id2 == x), drop = FALSE] %>%
          apply(1, function(y) {
            calculate(y, what = what)
          }) %>%
          data.frame(x = .)
        colnames(temp) <- x
        temp
      }) %>%
      dplyr::bind_cols() %>%
      as.data.frame()
    
    sample_info2 <-
      sample_info
    
    sample_info2$sample_id <- sample_id2
    
    sample_info2 <-
      sample_info2 %>%
      dplyr::distinct(sample_id, .keep_all = TRUE) %>%
      as.data.frame()
    
    process_info <-
      slot(object, name = "process_info")
    
    parameter <- new(
      Class = "tidymass_parameter",
      pacakge_name = "microbiomedataset",
      function_name = "summarise_samples()",
      parameter = list("what" = what,
                       "group_by" = group_by),
      time = Sys.time()
    )
    
    if (all(names(process_info) != "summarise_samples")) {
      process_info$summarise_samples <- parameter
    } else{
      process_info$summarise_samples <-
        c(process_info$summarise_samples,
          parameter)
    }
    slot(object, "process_info") <- process_info
    slot(object, "sample_info") <- sample_info2
    slot(object, "expression_data") <- expression_data2
    return(object)
  }
