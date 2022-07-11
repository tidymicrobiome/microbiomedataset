####Mutate new columns in sample_info
#' @title Mutate new columns in sample_info
#' @param object microbiome_dataset
#' @param what which you want to mutate
#' @param group_by should be one column from variable_info
#' @param na.rm na.rm
#' @param relative relative intensity or not.
#' @param return_same_object return same object or not.
#' @param data_type if return_same_object is FALSE, the return data.frame is
#' longer or wider.
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
           group_by,
           na.rm = TRUE,
           relative = TRUE,
           data_type = c("longer", "wider"),
           return_same_object = TRUE,
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
           what = c("mean_intensity",
                    "median_intensity",
                    "sum_intensity",
                    "na_number",
                    "na_prop"),
           group_by,
           na.rm = TRUE,
           relative = TRUE,
           data_type = c("longer", "wider"),
           return_same_object = TRUE,
           ...) {
    what <-
      match.arg(what)
    data_type <-
      match.arg(data_type)
    
    sample_info <-
      extract_sample_info(object)
    
    variable_info <-
      extract_variable_info(object)
    
    expression_data <-
      extract_expression_data(object)
    
    if (!missing(group_by)) {
      if (!group_by %in% colnames(variable_info)) {
        stop(group_by, " should be one column of variable_info")
      }
    } else{
      group_by <- "all"
      variable_info <-
        variable_info %>%
        dplyr::mutate(all = "Total")
    }
    
    temp_data <-
      tidyr::pivot_longer(data = object) %>%
      dplyr::left_join(variable_info %>%
                         dplyr::select(variable_id, group_by),
                       by = "variable_id")
    colnames(temp_data)[ncol(temp_data)] <- "class"
    
    if (grepl("na", what)) {
      if (what == "na_number") {
        new_columns <-
          temp_data %>%
          dplyr::group_by(sample_id, class) %>%
          dplyr::summarise(new_column = sum(is.na(.)),
                           .groups = "keep") %>%
          dplyr::ungroup() %>%
          tidyr::pivot_wider(names_from = "class",
                             values_from = "new_column")
      }
      
      if (what == "na_prop") {
        new_columns <-
          temp_data %>%
          dplyr::group_by(sample_id, class) %>%
          dplyr::summarise(new_column = sum(is.na(.)) / length(.),
                           .groups = "keep") %>%
          dplyr::ungroup() %>%
          tidyr::pivot_wider(names_from = "class",
                             values_from = "new_column")
      }
    } else{
      new_columns <-
        temp_data %>%
        dplyr::group_by(sample_id, class) %>%
        dplyr::summarise(new_column = calculate(value, what = what, na.rm = na.rm),
                         .groups = "keep") %>%
        dplyr::ungroup() %>%
        tidyr::pivot_wider(names_from = "class", values_from = "new_column")
    }
    
    if (!return_same_object) {
      if (data_type == "wider") {
        return(as.data.frame(new_columns))
      } else{
        new_columns <-
          new_columns %>%
          tidyr::pivot_longer(
            cols = -sample_id,
            names_to = "group_by",
            values_to = "value"
          ) %>%
          as.data.frame()
        return(new_columns)
      }
      
    }
    
    colnames(new_columns)[-1] <-
      paste(colnames(new_columns)[-1], what, sep = "_")
    
    colnames(new_columns)[-1] <-
      colnames(new_columns)[-1] %>%
      purrr::map(function(x) {
        massdataset::check_column_name(sample_info,
                                       x)
      }) %>%
      unlist()
    
    sample_info <-
      sample_info %>%
      dplyr::left_join(new_columns, by = "sample_id") %>%
      as.data.frame()
    
    slot(object = object, name = "sample_info") <-
      sample_info
    
    object <-
      massdataset::update_sample_info(object = object)
    
    process_info <- object@process_info
    
    parameter <- new(
      Class = "tidymass_parameter",
      pacakge_name = "massmicrobiome",
      function_name = "mutate2sample()",
      parameter = list(
        "what" = what,
        "group_by" = group_by,
        "na.rm" = na.rm
      ),
      time = Sys.time()
    )
    
    if (all(names(process_info) != "mutate2sample")) {
      process_info$mutate2sample <- parameter
    } else{
      process_info$mutate2sample <-
        c(process_info$mutate2sample,
          parameter)
    }
    
    object@process_info <- process_info
    return(object)
    
  }

calculate <-
  function(value,
           what = c("mean_intensity",
                    "median_intensity",
                    "sum_intensity",
                    "na_number",
                    "na_freq"),
           ...) {
    switch(
      EXPR = what,
      mean_intensity = mean(value, ...),
      median_intensity = median(value, ...),
      sum_intensity = sum(value, ...)
    )
  }