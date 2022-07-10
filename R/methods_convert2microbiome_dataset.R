#' @title convert to microbiome_dataset class object.
#' @param object phylosep or other class
#' @param ... other params
#' @return microbiome_dataset class object.
#' @export
convert2microbiome_dataset <- function(object, ...) {
  UseMethod("convert2microbiome_dataset")
}

#' @rdname convert2microbiome_dataset
#' @export
as.microbiome_dataset <- convert2microbiome_dataset

#' @method convert2microbiome_dataset phyloseq
#' @rdname convert2microbiome_dataset
#' @importFrom phyloseq sample_data tax_table phyloseq otu_table
#' @importFrom tibble column_to_rownames
#' @export

convert2microbiome_dataset.phyloseq <-
  function(object,
           ...) {
    expression_data <-
      phyloseq::otu_table(object = object) %>%
      as.data.frame()
    
    sample_info <-
      tryCatch(
        phyloseq::sample_data(object = object),
        error = function(e)
          NULL
      )
    
    if (is.null(sample_info)) {
      sample_info <-
        data.frame(sample_id = colnames(expression_data),
                   class = "Subject")
    } else{
      new_sample_info <-
        slot(sample_info, name = ".Data") %>%
        dplyr::bind_cols() %>%
        as.data.frame()
      colnames(new_sample_info) <-
        slot(sample_info, name = "names")
      rownames(new_sample_info) <- NULL
      new_sample_info <-
        new_sample_info %>%
        dplyr::rename(sample_id = "X.SampleID") %>%
        dplyr::mutate(class = "Subject")
      sample_info <-
        new_sample_info
    }
    
    variable_info <-
      tryCatch(
        phyloseq::tax_table(object = object),
        error = function(e)
          NULL
      )
    
    if (is.null(variable_info)) {
      variable_info <-
        data.frame(variable_id = rownames(expression_data))
    } else{
      new_variable_info <-
        slot(variable_info, name = ".Data") %>% 
        as.data.frame() %>% 
        tibble::rownames_to_column(var = "variable_id") %>% 
        dplyr::select(variable_id, dplyr::everything())
      
      variable_info <-
        new_variable_info
    }
    
    new_object <-
      create_microbiome_dataset(
        expression_data = expression_data,
        sample_info = sample_info,
        variable_info = variable_info
      )
    
    return(new_object)
  }
