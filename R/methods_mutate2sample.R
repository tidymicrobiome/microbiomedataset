####Mutate new columns into sample_info
#' @title Mutate new columns into sample_info
#' @param object microbiome_dataset
#' @param what which you want to mutate
#' @param according_to_variables (required) What variables used to calculate.
#' Default is "all". If you
#' want to use only several variables, provide their names as a vector.
#' @param ... other params
#' @return microbiome_dataset or data.frame.
#' @export
mutate2sample <-
  function(object,
           what = c("mean_intensity",
                    "median_intensity",
                    "sum_intensity",
                    "na_number",
                    "na_prop"),
           according_to_variables = "all",
           ...) {
    UseMethod("mutate2sample")
  }

#' @method mutate2sample microbiome_dataset
#' @rdname mutate2sample
#' @importFrom phyloseq sample_data tax_table phyloseq otu_table
#' @importFrom tibble column_to_rownames
#' @importFrom massdataset check_column_name
#' @export

mutate2sample.microbiome_dataset <-
  function(object,
           what = c("sum_intensity",
                    "mean_intensity",
                    "median_intensity",
                    "na_number",
                    "na_prop"),
           according_to_variables = "all",
           ...) {
    what <-
      match.arg(what)
    
    variable_info <-
      extract_variable_info(object)
    
    sample_info <-
      extract_sample_info(object)
    
    variable_id <- get_variable_id(object)
    sample_id <- get_sample_id(object)
    
    if (any(according_to_variables == "all")) {
      according_to_variables <- variable_id
    } else{
      according_to_variables <-
        variable_id[variable_id %in% according_to_variables]
    }
    
    if (length(according_to_variables) == 0) {
      stop(
        "All the variables you provide in according_to_variables are not in the object.
           Please check."
      )
    }
    
    expression_data <-
      extract_expression_data(object)
    
    if (grepl("na", what)) {
      if (what == "na_number") {
        new_info <-
          expression_data[according_to_variables, ,drop = FALSE] %>%
          apply(2, function(x) {
            sum(is.na(x))
          })
      }
      
      if (what == "na_prop") {
        new_info <-
          expression_data[according_to_variables, ,drop = FALSE] %>%
          apply(2, function(x) {
            sum(is.na(x)) / length(x)
          })
      }
    } else{
      new_info <-
        expression_data[according_to_variables, ,drop = FALSE] %>%
        apply(2, function(x) {
          calculate(x, what = what)
        })
      
    }
    
    new_column_name <-
      massdataset::check_column_name(slot(object, name = "sample_info") ,
                                     column.name = what)
    
    sample_info <-
      cbind(sample_info,
            new_info)
    
    colnames(sample_info)[ncol(sample_info)] <-
      new_column_name
    
    slot(object, "sample_info") <-
      sample_info
    
    object <-
      massdataset::update_sample_info(object = object)
    
    process_info <-
      slot(object, name = "process_info")
    
    parameter <- new(
      Class = "tidymass_parameter",
      pacakge_name = "microbiomedataset",
      function_name = "mutate2sample()",
      parameter = list("what" = what,
                       "according_to_variables" = according_to_variables),
      time = Sys.time()
    )
    
    if (all(names(process_info) != "mutate2sample")) {
      process_info$mutate2sample <- parameter
    } else{
      process_info$mutate2sample <-
        c(process_info$mutate2sample,
          parameter)
    }
    slot(object, "process_info") <- process_info
    return(object)
  }
