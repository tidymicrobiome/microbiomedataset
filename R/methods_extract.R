


#' @title extract_otu_tree
#' @description Extract OTU tree.
#' @rdname extract-microbiome_dataset
#' @docType methods
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param object (required) microbiome_dataset class object.
#' @return A treedata
#' @export

extract_otu_tree <-
  function(object) {
    otu_tree <- object@otu_tree
    otu_tree
  }


#' @title extract_taxa_tree
#' @description Extract Taxa tree.
#' @rdname extract-microbiome_dataset
#' @docType methods
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param object (required) microbiome_dataset class object.
#' @return A treedata
#' @export

extract_taxa_tree <-
  function(object) {
    taxa_tree <- object@taxa_tree
    taxa_tree
  }



#' @title extract_ref_seq
#' @description Extract ref sequence.
#' @rdname extract-microbiome_dataset
#' @docType methods
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param object (required) microbiome_dataset class object.
#' @return A xstringset
#' @export

extract_ref_seq <-
  function(object) {
    ref_seq <- object@ref_seq
    ref_seq
  }