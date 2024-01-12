#' Activate a Specific Component of a Microbiome Dataset
#'
#' This function sets a specific component of a microbiome dataset object as active. 
#' It allows the user to specify which part of the dataset to work with.
#'
#' @param .data The microbiome dataset object.
#' @param what A character string specifying the component to activate. 
#'             Options are "sample_info", "variable_info", or "expression_data". 
#'             Default is "sample_info".
#'
#' @details The function uses the `slot` function to assign the active component to 
#' the 'activated' slot of the microbiome dataset object. This makes it easier to 
#' work with specific parts of the dataset in subsequent analyses.
#'
#' @return The modified microbiome dataset object with the specified component activated.
#'
#' @export
#' @note It's essential to ensure that the microbiome dataset object has the 
#' necessary structure and slots to use this function effectively.
#'
#' @author Xiaotao Shen \email{shenxt1990@@outlook.com}
#'

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
