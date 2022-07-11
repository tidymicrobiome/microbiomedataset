#' @title extract_intensity
#' @param object microbiome_dataset
#' @param what which you want to mutate
#' @param taxonomic_rank taxonomic_rank
#' @param data_type if return_same_object is FALSE, the return data.frame
#' @param relative relative intensity or not
#' @param what which you want to mutate
#' @param na.rm na.rm
#' @param ... other params
#' @return microbiome_dataset
#' @export
extract_intensity <-
  function(object,
           taxonomic_rank = c("Kingdom",
                              "Phylum",
                              "Class",
                              "Order",
                              "Family",
                              "Genus",
                              "Species"),
           data_type = c("wider", "longer"),
           what = c("sum_intensity",
                    "mean_intensity",
                    "median_intensity"),
           na.rm = TRUE,
           relative = TRUE,
           ...) {
    UseMethod("extract_intensity")
  }

#' @method extract_intensity microbiome_dataset
#' @rdname extract_intensity
#' @importFrom phyloseq sample_data tax_table phyloseq otu_table
#' @importFrom tibble column_to_rownames
#' @export
#' @examples
#' data("global_patterns", package = "microbiomedataset")
#' 
#' x <-
#'   extract_intensity(
#'     object = global_patterns,
#'     taxonomic_rank = "Genus",
#'     data_type = "longer",
#'     relative = TRUE
#'   )
#' head(x)
#' x <-
#'   extract_intensity(
#'     object = global_patterns,
#'     taxonomic_rank = "Genus",
#'     data_type = "wider",
#'     relative = FALSE,
#'     what = "sum_intensity"
#'   )
#' head(x[, 1:5])

extract_intensity.microbiome_dataset <-
  function(object,
           taxonomic_rank = c("Kingdom",
                              "Phylum",
                              "Class",
                              "Order",
                              "Family",
                              "Genus",
                              "Species"),
           data_type = c("longer", "wider"),
           what = c("sum_intensity",
                    "mean_intensity",
                    "median_intensity"),
           na.rm = TRUE,
           relative = TRUE,
           ...) {
    data_type <- match.arg(data_type)
    what = match.arg(what)
    taxonomic_rank <- match.arg(taxonomic_rank)
    sample_info <-
      extract_sample_info(object)
    
    variable_info <-
      extract_variable_info(object)
    
    expression_data <-
      extract_expression_data(object)
    
    variable_info <-
      variable_info %>%
      dplyr::select(variable_id, taxonomic_rank) %>%
      dplyr::rename(taxa = taxonomic_rank) %>%
      dplyr::filter(!is.na(taxa))
    
    temp_data <-
      expression_data[variable_info$variable_id, ] %>%
      tibble::rownames_to_column(var = "variable_id") %>%
      dplyr::left_join(variable_info, by = "variable_id") %>%
      tidyr::pivot_longer(
        cols = -c("variable_id", "taxa"),
        names_to = "sample_id",
        values_to = "value"
      ) %>%
      dplyr::select(-variable_id) %>%
      dplyr::group_by(sample_id, taxa) %>%
      dplyr::summarise(value = calculate(value, what = what, na.rm = na.rm),
                       .groups = "keep") %>%
      dplyr::arrange(sample_id, taxa) %>%
      dplyr::ungroup()
    
    if (relative) {
      temp_data <-
        temp_data %>%
        plyr::dlply(.variables = plyr::.(sample_id)) %>%
        purrr::map(function(x) {
          x$value <- x$value * 100 / sum(x$value)
          x
        }) %>%
        dplyr::bind_rows()
    }
    
    if (data_type == "longer") {
      colnames(temp_data)[2] <- taxonomic_rank
      return(temp_data)
    }
    
    temp_data <-
      temp_data %>%
      tidyr::pivot_wider(names_from = "sample_id",
                         values_from = "value") %>%
      tibble::column_to_rownames(var = "taxa") %>%
      as.data.frame()
    
    temp_data[which(is.na(temp_data), arr.ind = TRUE)] <- 0
    
    return(temp_data)
  }
