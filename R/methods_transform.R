#' @title transform data to relative data
#' @param object mass_dataset, microbiome_dataset or Taxa table
#' @param ... other params
#' @return mass_dataset, microbiome_dataset or Taxa table
#' @export
transform2relative_intensity <-
  function(object,
           ...) {
    UseMethod("transform2relative_intensity")
  }

#' @method transform2relative_intensity microbiome_dataset
#' @rdname transform2relative_intensity
#' @importFrom tibble column_to_rownames rownames_to_column
#' @export

transform2relative_intensity.microbiome_dataset <-
  function(object,
           ...) {
    expression_data <-
      extract_expression_data(object) %>%
      as.matrix()
    
    expression_data <-
      expression_data %>%
      apply(2, function(x) {
        x / sum(x, na.rm = TRUE)
      }) %>%
      as.data.frame()
    
    attr(expression_data, "counts_or_relative") <-
      "relative"
    
    slot(object = object, name = "expression_data") <-
      expression_data
    
    process_info <- slot(object, name = "process_info")
    
    parameter <- new(
      Class = "tidymass_parameter",
      pacakge_name = "microbiomedataset",
      function_name = "transform2relative()",
      parameter = list("no" = "no"),
      time = Sys.time()
    )
    
    if (all(names(process_info) != "transform2relative")) {
      process_info$transform2relative <- parameter
    } else{
      process_info$transform2relative <-
        c(process_info$transform2relative,
          parameter)
    }
    
    slot(object, name = "process_info") <-
      process_info
    return(object)
  }
