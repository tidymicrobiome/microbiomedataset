#' @title convert to mass_dataset class object.
#' @param object microbiomedataset
#' @param ... other params
#' @return mass_dataset class object.
#' @export
convert2mass_dataset <- function(object, ...) {
  UseMethod("convert2mass_dataset")
}

#' @rdname convert2mass_dataset
#' @export
as.mass_dataset <- convert2mass_dataset

#' @method convert2mass_dataset microbiome_dataset
#' @rdname convert2mass_dataset
#' @importFrom tibble column_to_rownames
#' @importFrom massdataset create_mass_dataset
#' @export

convert2mass_dataset.microbiome_dataset <-
  function(object,
           ...) {
    new_object <-
      massdataset::create_mass_dataset(
        expression_data = object@expression_data,
        sample_info = object@sample_info,
        variable_info = object@variable_info,
        sample_info_note = object@sample_info_note,
        variable_info_note = object@variable_info_note
      )
    
    return(new_object)
  }
