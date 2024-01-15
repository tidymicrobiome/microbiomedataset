#' Mutate New Columns into Variable Info of Microbiome Dataset
#'
#' This function adds new columns to the `variable_info` of a `microbiome_dataset`. 
#' It calculates various statistics like mean, median, sum of intensities, and proportions 
#' of NA and zero values, based on the provided samples.
#'
#' @param object A `microbiome_dataset` object on which the mutation will be performed.
#' @param what A character vector specifying the statistics to be calculated and added. 
#'   Options include "mean_intensity", "median_intensity", "sum_intensity", "na_number", 
#'   "na_prop", "zero_number", "zero_prop". Default is all options.
#' @param according_to_samples A character vector specifying the samples to be used for 
#'   calculations. Default is "all", which uses all samples. If only specific samples are 
#'   needed, provide their names as a vector.
#' @param ... Additional arguments passed to other methods.
#'
#' @return Returns the modified `microbiome_dataset` object with additional columns in 
#'   `variable_info`.
#'
#' @importFrom phyloseq sample_data tax_table phyloseq otu_table
#' @importFrom tibble column_to_rownames
#' @importFrom massdataset check_column_name
#' @export 
mutate2variable <-
  function(object,
           what = c("mean_intensity",
                    "median_intensity",
                    "sum_intensity",
                    "na_number",
                    "na_prop",
                    "zero_number",
                    "zero_prop"),
           according_to_samples = "all",
           ...) {
    UseMethod("mutate2variable")
  }

#' @method mutate2variable microbiome_dataset
#' @rdname mutate2variable
#' @importFrom phyloseq sample_data tax_table phyloseq otu_table
#' @importFrom tibble column_to_rownames
#' @importFrom massdataset check_column_name
#' @export

mutate2variable.microbiome_dataset <-
  function(object,
           what = c("mean_intensity",
                    "median_intensity",
                    "sum_intensity",
                    "na_number",
                    "na_prop",
                    "zero_number",
                    "zero_prop"),
           according_to_samples = "all",
           ...) {
    what <-
      match.arg(what)
    
    variable_info <-
      extract_variable_info(object)
    
    sample_info <-
      extract_sample_info(object)
    
    variable_id <- get_variable_id(object)
    sample_id <- get_sample_id(object)
    
    if (any(according_to_samples == "all")) {
      according_to_samples <- sample_id
    } else{
      according_to_samples <-
        sample_id[sample_id %in% according_to_samples]
    }
    
    if (length(according_to_samples) == 0) {
      stop(
        "All the samples you provide in according_to_samples are not in the object.
           Please check."
      )
    }
    
    expression_data <-
      extract_expression_data(object)
    
    if (grepl("na", what)) {
      if (what == "na_number") {
        new_info <-
          expression_data[, according_to_samples, drop = FALSE] %>%
          apply(1, function(x) {
            sum(is.na(x))
          })
      }

      if (what == "zero_number") {
        new_info <-
          expression_data[, according_to_samples, drop = FALSE] %>%
          apply(1, function(x) {
            sum(x == 0)
          })
      }

      if (what == "na_prop") {
        new_info <-
          expression_data[, according_to_samples, drop = FALSE] %>%
          apply(1, function(x) {
            sum(is.na(x)) / length(x)
          })
      }

      if (what == "zero_prop") {
        new_info <-
          expression_data[, according_to_samples, drop = FALSE] %>%
          apply(1, function(x) {
            sum(x == 0) / length(x)
          })
      }

    } else{
      new_info <-
        expression_data[, according_to_samples, drop = FALSE] %>%
        apply(1, function(x) {
          calculate(x, what = what)
        })
      
    }
    
    new_column_name <-
      massdataset::check_column_name(slot(object, name = "variable_info") ,
                                     column.name = what)
    
    variable_info <-
      cbind(variable_info,
            new_info)
    
    colnames(variable_info)[ncol(variable_info)] <-
      new_column_name
    
    slot(object, "variable_info") <-
      variable_info
    
    object <-
      massdataset::update_variable_info(object = object)
    
    process_info <-
      slot(object, name = "process_info")
    
    parameter <- new(
      Class = "tidymass_parameter",
      pacakge_name = "microbiomedataset",
      function_name = "mutate2variable()",
      parameter = list("what" = what,
                       "according_to_samples" = according_to_samples),
      time = Sys.time()
    )
    
    if (all(names(process_info) != "mutate2variable")) {
      process_info$mutate2variable <- parameter
    } else{
      process_info$mutate2variable <-
        c(process_info$mutate2variable,
          parameter)
    }
    
    slot(object, "process_info") <- process_info
    return(object)
  }
