####some code from Guangchuang Yu, credites should go there
#' @title convert data.frame contained hierarchical relationship or other classes to treedata class
#' @param object taxonomyTable class (phyloseq package) or other class (data.frame)
#' @param type type
#' @param ... other params
#' @return treedata class object (ggtree package).
#' @export
convert2treedata <-
  function(object,
           type = "species",
           ...) {
    UseMethod("convert2treedata")
  }


#' @rdname convert2treedata
#' @export
as.treedata <- convert2treedata


####some code is from MicrobiotaProcess, credit should go its developer
#' @method convert2treedata data.frame
#' @rdname convert2treedata
#' @importFrom phyloseq sample_data tax_table phyloseq otu_table
#' @importFrom phyloseq sample_data tax_table phyloseq otu_table
#' @importClassesFrom phyloseq taxonomyTable
#' @importFrom tibble column_to_rownames
#' @importFrom tidytree treedata
#' @export

convert2treedata.data.frame <-
  function(object,
           type = "species",
           ...) {
    data <- as.data.frame(object)
    
    if (!"fill_taxa_table_na" %in% names(attributes(data))) {
      data <- fill_taxa_table_na(data, type = type)
    }
    
    data$variable_id <- rownames(data)
    
    data <-
      data %>%
      dplyr::mutate(Root = "r__root") %>%
      dplyr::select(Root, dplyr::everything())
    
    if (!"fill_taxa_table_na" %in% names(attributes(data))) {
      data <- fill_taxa_table_na(data, type = type)
    }
    
    new_data <-
      seq_len(ncol(data) - 1) %>%
      purrr::map(function(i) {
        temp_data <-
          data[, c(i, i + 1)] %>%
          dplyr::mutate(nodeClass = colnames(data)[i + 1],
                        nodeDepth = i) %>%
          dplyr::distinct()
        colnames(temp_data)[1:2] <- c("parent", "child")
        # temp_data %>%
        #   dplyr::filter(!is.na(parent) & !is.na(child))
        temp_data
      }) %>%
      dplyr::bind_rows() %>%
      dplyr::distinct()
    
    isTip <-
      !as.vector(new_data$child) %in% as.vector(new_data$parent)
    index <- rep(NA, length(isTip))
    index[isTip] <- seq(1, sum(isTip))
    index[!isTip] <- seq(sum(isTip) + 2, length(isTip) + 1)
    mapping <-
      data.frame(node = index,
                 labelnames = as.vector(new_data$child),
                 isTip)
    indxx <- match(mapping$labelnames, new_data$child)
    mapping$nodeClass <- new_data[indxx, "nodeClass"]
    mapping$nodeDepth <- new_data[indxx, "nodeDepth"]
    parentnode <-
      mapping[match(as.vector(new_data$parent), as.vector(mapping$labelnames)),]$node
    childnode <-
      mapping[match(as.vector(new_data$child), as.vector(mapping$labelnames)),]$node
    edges <- cbind(parentnode, childnode)
    colnames(edges) <- NULL
    edges[is.na(edges)] <- sum(isTip) + 1
    root <- data.frame(
      node = sum(isTip) + 1,
      labelnames = "r__root",
      isTip = FALSE,
      nodeClass = "Root",
      nodeDepth = 0
    )
    mapping <- rbind(root, mapping)
    mapping <- mapping[order(mapping$node),]
    node.label <- as.vector(mapping$labelnames)[!mapping$isTip]
    tip.label <- as.vector(mapping$labelnames)[mapping$isTip]
    mapping <-
      mapping[, colnames(mapping) %in% c("node", "nodeClass", "nodeDepth")]
    taxphylo <-
      structure(
        list(
          edge = edges,
          node.label = node.label,
          tip.label = tip.label,
          Nnode = length(node.label)
        ),
        class = "phylo"
      )
    
    result <-
      tidytree::treedata(phylo = taxphylo,
                         data = as_tibble(mapping))
    return(result)
  }

#' @method convert2treedata taxonomyTable
#' @rdname convert2treedata
#' @importFrom phyloseq sample_data tax_table phyloseq otu_table
#' @importFrom phyloseq sample_data tax_table phyloseq otu_table
#' @importClassesFrom phyloseq taxonomyTable
#' @importFrom tibble column_to_rownames
#' @importFrom tidytree treedata
#' @export
convert2treedata.taxonomyTable <-
  function(object,
           type = "species",
           ...) {
    convert2treedata.data.frame(object = object,
                                type = type,
                                ...)
  }
