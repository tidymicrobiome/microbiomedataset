#' @title convert to phyloseq class object.
#' @param object microbiomedataset
#' @param ... other params
#' @return phyloseq class object.
#' @export
convert2phyloseq <- function(object, ...) {
  UseMethod("convert2phyloseq")
}


#' @rdname convert2phyloseq
#' @export
as.phyloseq <- convert2phyloseq

#' @method convert2phyloseq microbiome_dataset
#' @rdname convert2phyloseq
#' @importFrom phyloseq sample_data tax_table phyloseq otu_table
#' @importFrom tibble column_to_rownames
#' @export

convert2phyloseq.microbiome_dataset <-
  function(object,
           ...) {
    otu_data <-
      extract_expression_data(object) %>%
      as.matrix() %>%
      phyloseq::otu_table(taxa_are_rows = TRUE)
    
    sample_data <-
      extract_sample_info(object) %>%
      tibble::column_to_rownames(var = "sample_id") %>%
      phyloseq::sample_data()
    
    taxa_data <-
      extract_variable_info(object) %>%
      dplyr::select(Domain, Phylum, Class, Order, Family, Genus, Species) %>%
      as.matrix() %>%
      phyloseq::tax_table()
    
    new_object <-
      phyloseq::phyloseq(otu_data, sample_data, taxa_data)
    
    return(new_object)
  }
