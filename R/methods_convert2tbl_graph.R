#' @title convert to tbl_graph class object in tidygraph.
#' @param object microbiomedataset or Taxa table
#' @param intensity_method for each node, the how to calculate the intensity for
#' it accross all the samples
#' @param ... other params
#' @return tbl_graph class object.
#' @export
convert2tbl_graph <-
  function(object,
           intensity_method = c("sum", "mean", "median"),
           ...) {
    UseMethod("convert2tbl_graph")
  }


#' @rdname convert2tbl_graph
#' @export
as.tbl_graph <- convert2tbl_graph

#' @method convert2tbl_graph microbiome_dataset
#' @rdname convert2tbl_graph
#' @importFrom tidygraph tbl_graph as_tbl_graph
#' @importFrom tidygraph tbl_graph
#' @importFrom tibble column_to_rownames rownames_to_column
#' @export

convert2tbl_graph.microbiome_dataset <-
  function(object,
           intensity_method = c("sum", "mean", "median"),
           ...) {
    intensity_method <- match.arg(intensity_method)
    expression_data <-
      extract_expression_data(object) %>%
      as.matrix()
    
    sample_info <-
      extract_sample_info(object)
    
    variable_info <-
      extract_variable_info(object)
    
    variable_info <-
      remove_na_from_taxa_table(variable_info)
    
    ####get the node_data first
    ##Kingdom
    kingdowm_intensity <-
      extract_intensity(
        object = object,
        sample_wise = "sample_id",
        taxonomic_rank = "Kingdom",
        data_type = "wider",
        na.rm = TRUE,
        relative = FALSE
      ) %>%
      apply(1, switch(
        intensity_method,
        "sum" = sum,
        "mean" = mean,
        "median" = median,
      ))
    
    names(kingdowm_intensity) <-
      paste0("k__", names(kingdowm_intensity))
    
    ##Phylum
    phylum_intensity <-
      extract_intensity(
        object = object,
        sample_wise = "sample_id",
        taxonomic_rank = "Phylum",
        data_type = "wider",
        na.rm = TRUE,
        relative = FALSE
      ) %>%
      apply(1, switch(
        intensity_method,
        "sum" = sum,
        "mean" = mean,
        "median" = median,
      ))
    
    names(phylum_intensity) <-
      paste0("p__", names(phylum_intensity))
    
    ##Class
    class_intensity <-
      extract_intensity(
        object = object,
        sample_wise = "sample_id",
        taxonomic_rank = "Class",
        data_type = "wider",
        na.rm = TRUE,
        relative = FALSE
      ) %>%
      apply(1, switch(
        intensity_method,
        "sum" = sum,
        "mean" = mean,
        "median" = median,
      ))
    
    names(class_intensity) <-
      paste0("c__", names(class_intensity))
    
    ##Order
    order_intensity <-
      extract_intensity(
        object = object,
        sample_wise = "sample_id",
        taxonomic_rank = "Order",
        data_type = "wider",
        na.rm = TRUE,
        relative = FALSE
      ) %>%
      apply(1, switch(
        intensity_method,
        "sum" = sum,
        "mean" = mean,
        "median" = median,
      ))
    
    names(order_intensity) <-
      paste0("o__", names(order_intensity))
    
    ##Family
    family_intensity <-
      extract_intensity(
        object = object,
        sample_wise = "sample_id",
        taxonomic_rank = "Family",
        data_type = "wider",
        na.rm = TRUE,
        relative = FALSE
      ) %>%
      apply(1, switch(
        intensity_method,
        "sum" = sum,
        "mean" = mean,
        "median" = median,
      ))
    
    names(family_intensity) <-
      paste0("f__", names(family_intensity))
    
    ##Genus
    genus_intensity <-
      extract_intensity(
        object = object,
        sample_wise = "sample_id",
        taxonomic_rank = "Genus",
        data_type = "wider",
        na.rm = TRUE,
        relative = FALSE
      ) %>%
      apply(1, switch(
        intensity_method,
        "sum" = sum,
        "mean" = mean,
        "median" = median,
      ))
    
    names(genus_intensity) <-
      paste0("g__", names(genus_intensity))
    
    ##Species
    species_intensity <-
      extract_intensity(
        object = object,
        sample_wise = "sample_id",
        taxonomic_rank = "Species",
        data_type = "wider",
        na.rm = TRUE,
        relative = FALSE
      ) %>%
      apply(1, switch(
        intensity_method,
        "sum" = sum,
        "mean" = mean,
        "median" = median,
      ))
    
    names(species_intensity) <-
      paste0("s__", names(species_intensity))
    
    root_intensity <-
      c("Root" = sum(kingdowm_intensity))
    
    node_data <-
      data.frame(
        intensity = c(
          root_intensity,
          kingdowm_intensity,
          phylum_intensity,
          class_intensity,
          order_intensity,
          family_intensity,
          genus_intensity,
          species_intensity
        )
      ) %>%
      tibble::rownames_to_column(var = "node")
    
    ##edge_data
    temp_data <-
      variable_info %>%
      dplyr::select(Kingdom, Phylum, Class, Order, Family, Genus, Species) %>%
      dplyr::mutate(Root = "Root") %>%
      dplyr::select(Root, dplyr::everything())
    
    temp_data$Kingdom <- paste0("k__",  temp_data$Kingdom)
    temp_data$Phylum <- paste0("p__",  temp_data$Phylum)
    temp_data$Class <- paste0("c__",  temp_data$Class)
    temp_data$Order <- paste0("o__",  temp_data$Order)
    temp_data$Family <- paste0("f__",  temp_data$Family)
    temp_data$Genus <- paste0("g__",  temp_data$Genus)
    temp_data$Species <- paste0("s__",  temp_data$Species)
    
    edge_data <-
      seq_len(ncol(temp_data) - 1) %>%
      purrr::map(function(i) {
        temp <-
          temp_data[, c(i, i + 1)] %>%
          dplyr::distinct()
        colnames(temp) <- c("from", "to")
        temp
      }) %>%
      dplyr::bind_rows() %>%
      dplyr::filter(!stringr::str_detect(from, "__NA") &
                      !stringr::str_detect(to, "__NA"))
    
    node_data <-
      node_data %>%
      dplyr::filter(node %in% c(edge_data$from, edge_data$to)) %>%
      dplyr::mutate(index = dplyr::row_number())
    
    edge_data <-
      edge_data %>%
      dplyr::filter(from %in% node_data$node &
                      to %in% node_data$node)
    
    result <-
      tidygraph::tbl_graph(nodes = node_data, edges = edge_data)
    
    return(result)
    
  }
