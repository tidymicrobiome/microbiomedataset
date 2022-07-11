####This code is from MicrobiotaProcess, credit should go its developer

#' @importFrom magrittr %>%
#' @importFrom tibble rownames_to_column column_to_rownames
#' @keywords internal
duplicated_taxa_check <- function(taxdf) {
  if (ncol(taxdf) == 1) {
    return(taxdf)
  }
  taxdf <- taxdf %>%
    tibble::rownames_to_column()
  for (i in ncol(taxdf):3) {
    tmp <- split(taxdf, taxdf[, i])
    for (j in seq_len(length(tmp))) {
      flag <- length(unique(as.vector(tmp[[j]][, i - 1])))
      if (flag > 1) {
        tmp[[j]][, i] <- paste(tmp[[j]][, i], tmp[[j]][, i - 1], sep = "_")
      }
    }
    taxdf <- do.call("rbind", c(tmp, make.row.names = FALSE))
  }
  return(taxdf)
}

####This code is from MicrobiotaProcess, credit should go its developer

# #' @keywords internal
# #taxlevelchar <- c("k", "p", "c", "o", "f", "g", "s", "st")

new_taxa_name <- function(x, y) {
  y <- as.vector(y)
  x[y] <- paste(taxlevelchar[y], x[y], sep = "__un_")
  x
}

####This code is from MicrobiotaProcess, credit should go its developer

#' @keywords internal
repduplicated_tax_check <- function(taxdf) {
  for (i in seq_len(7)) {
    taxdf <- duplicated_taxa_check(taxdf) %>%
      tibble::column_to_rownames(var = "rowname")
  }
  return(taxdf)
}

####This code is from MicrobiotaProcess, credit should go its developer
#' @importFrom zoo na.locf
#' @keywords internal
fill_taxa_table_name <-
  function(taxdf, type = "species") {
    tmprownames <- rownames(taxdf)
    indexmark <-
      apply(taxdf, 2, function(x) {
        nchar(x, keepNA = TRUE)
      }) <= 4
    taxdf[indexmark] <- NA
    if (any(is.na(taxdf[, 1]))) {
      if (type == "species") {
        prefix <- "k__"
      } else{
        prefix <- "d1__"
      }
      taxdf[is.na(taxdf[, 1]), 1] <- paste0(prefix, "Unknown")
    }
    indextmp <- apply(is.na(taxdf), 1, which)
    if (length(indextmp) == 0) {
      taxdf <- data.frame(taxdf, check.names = FALSE)
      return(taxdf)
    }
    taxdf <- apply(taxdf, 1, zoo::na.locf)
    taxdf <- lapply(seq_len(ncol(taxdf)), function(i)
      taxdf[, i])
    taxdf <- data.frame(t(mapply(new_taxa_name, taxdf, indextmp)),
                        stringsAsFactors = FALSE)
    rownames(taxdf) <- tmprownames
    return(taxdf)
  }

####This code is from MicrobiotaProcess, credit should go its developer
#' @keywords internal
addtaxlevel <- function(taxdf) {
  #, type="species"){
  #if (type != "species"){
  #    taxlevelchar <- paste0("d", seq_len(ncol(taxdf)))
  #}else{
  #    taxlevelchar <- taxlevelchar[seq_len(ncol(taxdf))]
  #}
  taxlevelchar <-
    taxlevelchar[seq_len(length(taxdf))]
  paste(taxlevelchar, taxdf, sep = "__")
}


####This code is from MicrobiotaProcess, credit should go its developer
fill_taxa_table_na <-
  function(taxdf, type = "species") {
    taxdf <- remove_taxonomy_rank_na(taxdf)
    if (type != "species") {
      assign("taxlevelchar", paste0("d", seq_len(ncol(taxdf))), envir = .GlobalEnv)
    } else{
      assign("taxlevelchar",
             c("k", "p", "c", "o", "f", "g", "s", "st"),
             envir = .GlobalEnv)
    }
    if (!(grepl("^k__", taxdf[1, 1]) ||
          grepl("^d1__", taxdf[1, 1]))) {
      tmprownames <- rownames(taxdf)
      tmpcolnames <- colnames(taxdf)
      taxdf <- t(apply(taxdf, 1, as.character))
      taxdf[is.na(taxdf)] <- ""
      taxdf <- data.frame(t(apply(taxdf, 1, addtaxlevel)),
                          stringsAsFactors = FALSE)
      rownames(taxdf) <- tmprownames
      colnames(taxdf) <- tmpcolnames
    }
    taxdf <-
      fill_taxa_table_name(taxdf, type = type)
    taxdf <-
      repduplicated_tax_check(taxdf) #%>% column_to_rownames(var="rowname")
    attr(taxdf, "fill_taxa_table_na") <- TRUE
    return(taxdf)
  }


####This code is from MicrobiotaProcess, credit should go its developer
remove_taxonomy_rank_na <-
  function(x) {
    x <- as.data.frame(x, check.names = FALSE)
    x <- remove_taxa_table_unclassfied(x)
    indx <- vapply(x,
                   function(i)
                     all(is.na(i)),
                   FUN.VALUE = logical(1)) %>%
      setNames(NULL)
    x <- x[, !indx, drop = FALSE]
    return(x)
  }


# ####This code is from MicrobiotaProcess, credit should go its developer
# build_refseq <- function(x) {
#   flag <- guess_rownames(x)
#   refseq <- switch(
#     flag,
#     DNA = Biostrings::DNAStringSet(x),
#     AA = Biostrings::AAStringSet(x),
#     RNA = Biostrings::RNAStringSet(x),
#     Other = NULL
#   )
#   return(refseq)
# }

####This code is from MicrobiotaProcess, credit should go its developer
remove_taxa_table_unclassfied <-
  function(taxdf) {
    taxdf[grepl_data.frame(
      "Unclassified|uncultured|Ambiguous|Unknown|unknown|metagenome|Unassig",
      taxdf,
      ignore.case = TRUE
    )] <- NA
    return(taxdf)
  }

####This code is from MicrobiotaProcess, credit should go its developer
grepl_data.frame <- function(pattern, x, ...) {
  y <- if (length(x)) {
    do.call("cbind", lapply(x, "grepl", pattern = pattern, ...))
  } else{
    matrix(FALSE, length(row.names(x)), 0)
  }
  if (.row_names_info(x) > 0L)
    rownames(y) <- row.names(x)
  y
}


msg <-
  function(..., startup = FALSE) {
    if (startup) {
      if (!isTRUE(getOption("microbiomedataset.quiet"))) {
        packageStartupMessage(text_col(...))
      }
    } else {
      message(text_col(...))
    }
  }

text_col <- function(x) {
  # If RStudio not available, messages already printed in black
  if (!rstudioapi::isAvailable()) {
    return(x)
  }
  
  if (!rstudioapi::hasFun("getThemeInfo")) {
    return(x)
  }
  
  theme <- rstudioapi::getThemeInfo()
  
  if (isTRUE(theme$dark))
    crayon::white(x)
  else
    crayon::black(x)
  
}

#' List all packages in the microbiomedataset
#'
#' @param include_self Include microbiomedataset in the list?
#' @export
#' @return microbiomedataset packages
#' @examples
#' microbiomedataset_packages()
microbiomedataset_packages <-
  function(include_self = TRUE) {
    raw <- utils::packageDescription("microbiomedataset")$Imports
    imports <- strsplit(raw, ",")[[1]]
    parsed <- gsub("^\\s+|\\s+$", "", imports)
    names <-
      vapply(strsplit(parsed, "\\s+"), "[[", 1, FUN.VALUE = character(1))
    
    if (include_self) {
      names <- c(names, "microbiomedataset")
    }
    
    names
  }

invert <- function(x) {
  if (length(x) == 0)
    return()
  stacked <- utils::stack(x)
  tapply(as.character(stacked$ind), stacked$values, list)
}


style_grey <- function(level, ...) {
  crayon::style(paste0(...),
                crayon::make_style(grDevices::grey(level), grey = TRUE))
}


###re-assign NA to downstream level data
remove_na_from_taxa_table <-
  function(x) {
    ###Kingdom
    idx <- which(is.na(x$Kingdom))
    if(length(idx) > 0){
      x[idx, c("Phylum", "Class", "Order", "Family", "Genus", "Species")] <- NA
    }
    
    ###Phylum
    idx <- which(is.na(x$Phylum))
    if(length(idx) > 0){
      x[idx, c("Class", "Order", "Family", "Genus", "Species")] <- NA
    }
    
    ###Class
    idx <- which(is.na(x$Class))
    if(length(idx) > 0){
      x[idx, c("Order", "Family", "Genus", "Species")] <- NA
    }
    
    ###Order
    idx <- which(is.na(x$Order))
    if(length(idx) > 0){
      x[idx, c("Family", "Genus", "Species")] <- NA
    }
    
    ###Family
    idx <- which(is.na(x$Family))
    if(length(idx) > 0){
      x[idx, c("Genus", "Species")] <- NA
    }
    
    ###Genus
    idx <- which(is.na(x$Genus))
    if(length(idx) > 0){
      x[idx, c("Species")] <- NA
    }
    
    return(x)
  }
