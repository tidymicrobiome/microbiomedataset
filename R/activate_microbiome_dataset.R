#' @title activate_microbiome_dataset
#' @description Determine the context of subsequent manipulations
#' @docType methods
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param .data (required) A microbiome_dataset class object
#' @param what (required) What should get be activated?
#' Possible values are `sample_info`,
#' `expression_data`, or `variable_info`.
#' @return A microbiome_dataset class object
#' @export

activate_microbiome_dataset <-
  function(.data,
           what = c("sample_info",
                    "variable_info",
                    "expression_data")) {
    what <- match.arg(what)
    slot(object = .data, name = "activated") <- what
    # .data@activated = what
    return(.data)
  }
