#' @title extract_intensity
#' @param object microbiome_dataset
#' @param sample_wise sample_wise, default is sample_id
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
           sample_wise = "sample_id",
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
           sample_wise = "sample_id",
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
    what <- match.arg(what)
    taxonomic_rank <- match.arg(taxonomic_rank)
    sample_info <-
      extract_sample_info(object)
    
    if (sample_wise != "sample_id") {
      if (!sample_wise %in% colnames(sample_info)) {
        stop(sample_wise, " must is one of columns of sample_info")
      }
    }
    
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
    
    if (sample_wise != "sample_id") {
      temp_data <-
        temp_data %>%
        dplyr::left_join(sample_info[, c("sample_id", sample_wise)],
                         by = "sample_id") %>%
        dplyr::select(-sample_id)
      colnames(temp_data)[ncol(temp_data)] <- "sample_id"
      temp_data <-
        temp_data %>% 
        dplyr::group_by(sample_id, taxa) %>%
        dplyr::summarise(value = calculate(value, what = what, na.rm = na.rm),
                         .groups = "keep") %>%
        dplyr::arrange(sample_id, taxa) %>%
        dplyr::ungroup()
    }
    
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
      colnames(temp_data)[1:2] <- c(sample_wise, taxonomic_rank)
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


#' Melt a microbiome_dataset into a Long Table
#'
#' Convert a `microbiome_dataset` into a long table that combines abundance,
#' `sample_info`, and `variable_info`.
#'
#' @param object A `microbiome_dataset` object.
#' @param relative Should abundance be converted to relative abundance before
#'   melting?
#' @param value_name Name of the abundance column in the output.
#'
#' @return A data frame in long format.
#' @export
#' @examples
#' data("global_patterns", package = "microbiomedataset")
#'
#' x <- psmelt_microbiome_dataset(global_patterns)
#' head(x[, 1:8])
#'
#' y <- psmelt_microbiome_dataset(global_patterns, relative = TRUE)
#' head(y[, c("sample_id", "variable_id", "abundance")])
psmelt_microbiome_dataset <-
  function(object,
           relative = FALSE,
           value_name = "abundance") {
    if (!is(object, "microbiome_dataset")) {
      stop("object must be a microbiome_dataset.")
    }
    
    if (!is.character(value_name) || length(value_name) != 1) {
      stop("value_name must be a single character string.")
    }
    
    if (relative) {
      object <- transform_counts(object, method = "relative")
    }
    
    expression_data <- extract_expression_data(object)
    sample_info <- normalize_sample_info_schema(extract_sample_info(object))
    variable_info <- normalize_variable_info_schema(extract_variable_info(object))
    
    shared_columns <-
      intersect(
        setdiff(colnames(sample_info), "sample_id"),
        setdiff(colnames(variable_info), "variable_id")
      )
    
    if (length(shared_columns) > 0) {
      colnames(sample_info)[match(shared_columns, colnames(sample_info))] <-
        paste0(shared_columns, "_sample")
      colnames(variable_info)[match(shared_columns, colnames(variable_info))] <-
        paste0(shared_columns, "_variable")
    }
    
    result <-
      expression_data %>%
      tibble::rownames_to_column(var = "variable_id") %>%
      tidyr::pivot_longer(
        cols = -variable_id,
        names_to = "sample_id",
        values_to = value_name
      ) %>%
      dplyr::left_join(sample_info, by = "sample_id") %>%
      dplyr::left_join(variable_info, by = "variable_id") %>%
      as.data.frame()
    
    result
  }


#' Melt Features into a Long Table
#'
#' Neutral alias of [psmelt_microbiome_dataset()] for feature-level long table
#' export.
#'
#' @inheritParams psmelt_microbiome_dataset
#'
#' @return A data frame in long format.
#' @export
#' @examples
#' data("global_patterns", package = "microbiomedataset")
#'
#' x <- melt_features(global_patterns)
#' head(x[, c("sample_id", "variable_id", "abundance")])
melt_features <-
  function(object,
           relative = FALSE,
           value_name = "abundance") {
    psmelt_microbiome_dataset(
      object = object,
      relative = relative,
      value_name = value_name
    )
  }


#' Melt Taxonomic Abundance into a Long Table
#'
#' Convert a `microbiome_dataset` into a taxonomic-rank long table by
#' aggregating abundance at a chosen rank and attaching `sample_info`.
#'
#' @param object A `microbiome_dataset` object.
#' @param taxonomic_rank Taxonomic rank used for aggregation.
#' @param relative Should abundance be expressed as relative abundance?
#' @param what Aggregation method used within each rank.
#' @param na.rm Should missing values be removed before aggregation?
#' @param value_name Name of the abundance column in the output.
#'
#' @return A data frame in long format.
#' @export
#' @examples
#' data("global_patterns", package = "microbiomedataset")
#'
#' x <- psmelt_taxa(global_patterns, taxonomic_rank = "Genus")
#' head(x[, c("sample_id", "Genus", "abundance")])
#'
#' y <- psmelt_taxa(global_patterns, taxonomic_rank = "Phylum", relative = TRUE)
#' head(y[, c("sample_id", "Phylum", "abundance")])
psmelt_taxa <-
  function(object,
           taxonomic_rank = c("Kingdom",
                              "Phylum",
                              "Class",
                              "Order",
                              "Family",
                              "Genus",
                              "Species"),
           relative = TRUE,
           what = c("sum_intensity",
                    "mean_intensity",
                    "median_intensity"),
           na.rm = TRUE,
           value_name = "abundance") {
    taxonomic_rank <- match.arg(taxonomic_rank)
    what <- match.arg(what)
    
    if (!is(object, "microbiome_dataset")) {
      stop("object must be a microbiome_dataset.")
    }
    
    if (!is.character(value_name) || length(value_name) != 1) {
      stop("value_name must be a single character string.")
    }
    
    result <-
      extract_intensity(
        object = object,
        sample_wise = "sample_id",
        taxonomic_rank = taxonomic_rank,
        data_type = "longer",
        what = what,
        na.rm = na.rm,
        relative = relative
      )
    
    colnames(result)[colnames(result) == "value"] <- value_name
    
    sample_info <- normalize_sample_info_schema(extract_sample_info(object))
    sample_columns <- setdiff(colnames(sample_info), "sample_id")
    
    if (length(sample_columns) > 0) {
      result <-
        result %>%
        dplyr::left_join(sample_info, by = "sample_id") %>%
        as.data.frame()
    }
    
    result
  }


#' Melt Taxa into a Rank-level Long Table
#'
#' Neutral alias of [psmelt_taxa()] for taxonomic-rank long table export.
#'
#' @inheritParams psmelt_taxa
#'
#' @return A data frame in long format.
#' @export
#' @examples
#' data("global_patterns", package = "microbiomedataset")
#'
#' x <- melt_taxa(global_patterns, taxonomic_rank = "Genus")
#' head(x[, c("sample_id", "Genus", "abundance")])
melt_taxa <-
  function(object,
           taxonomic_rank = c("Kingdom",
                              "Phylum",
                              "Class",
                              "Order",
                              "Family",
                              "Genus",
                              "Species"),
           relative = TRUE,
           what = c("sum_intensity",
                    "mean_intensity",
                    "median_intensity"),
           na.rm = TRUE,
           value_name = "abundance") {
    psmelt_taxa(
      object = object,
      taxonomic_rank = taxonomic_rank,
      relative = relative,
      what = what,
      na.rm = na.rm,
      value_name = value_name
    )
  }
