#' @title Barplot based on ggplot2
#' @param object microbiome_dataset
#' @param fill fill for barplot
#' @param top_n default is 5
#' @param show.legend show.legend or not
#' @param what which you want to mutate
#' @param na.rm na.rm
#' @param relative relative intensity or not.
#' @param re_calculate_relative re-calculate relative abundance or not.
#' @param x x axis
#' @param color color for barplot
#' @param facet_grid facet_grid
#' @param ... other params
#' @return ggplot2 class object
#' @export

plot_barplot <-
  function(object,
           fill = c("Kingdom",
                    "Phylum",
                    "Class",
                    "Order",
                    "Family",
                    "Genus",
                    "Species"),
           top_n = 5,
           show.legend,
           what = c("sum_intensity",
                    "mean_intensity",
                    "median_intensity"),
           na.rm = TRUE,
           relative = TRUE,
           re_calculate_relative = FALSE,
           x = "sample_id",
           color = "balck",
           facet_grid,
           ...) {
    UseMethod("plot_barplot")
  }

#' @method plot_barplot microbiome_dataset
#' @rdname plot_barplot
#' @importFrom phyloseq sample_data tax_table phyloseq otu_table
#' @importFrom tibble column_to_rownames
#' @importFrom massdataset check_column_name
#' @importFrom ggplot2 ggplot geom_bar aes labs theme_bw
#' @importFrom ggplot2 scale_x_discrete expansion guides guide_legend
#' @importFrom ggplot2 element_text theme facet_grid vars
#' @export

plot_barplot.microbiome_dataset <-
  function(object,
           fill = c("Kingdom",
                    "Phylum",
                    "Class",
                    "Order",
                    "Family",
                    "Genus",
                    "Species"),
           top_n = 5,
           show.legend,
           what = c("sum_intensity",
                    "mean_intensity",
                    "median_intensity"),
           na.rm = TRUE,
           relative = TRUE,
           re_calculate_relative = FALSE,
           x = "sample_id",
           color = "balck",
           facet_grid,
           ...) {
    fill <-
      match.arg(fill)
    what <-
      match.arg(what)
    
    intensity <-
      extract_intensity(
        object = object,
        sample_wise = x,
        taxonomic_rank = fill,
        data_type = "longer",
        what = what,
        na.rm = na.rm,
        relative = relative
      )
    
    colnames(intensity)[1] <- "sample_id"
    
    intensity <-
      intensity %>%
      dplyr::group_by(sample_id) %>%
      dplyr::slice_max(order_by = value, n = top_n) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(dplyr::desc(value)) %>%
      as.data.frame()
    
    if (relative & re_calculate_relative) {
      intensity <-
        intensity %>%
        plyr::dlply(.variables = plyr::.(sample_id)) %>%
        purrr::map(function(x) {
          x$value <- x$value * 100 / sum(x$value)
          x
        }) %>%
        dplyr::bind_rows() %>%
        dplyr::arrange(dplyr::desc(value)) %>%
        as.data.frame()
    }
    
    intensity[, 2] <-
      factor(intensity[, 2], levels = unique(intensity[, 2]))
    
    if (missing(show.legend)) {
      if (length(unique(intensity[, 2, drop = TRUE])) > 10) {
        show.legend <- FALSE
      } else{
        show.legend <- TRUE
      }
    }
    
    colnames(intensity)[1] <- x
    
    sample_info <-
      extract_sample_info(object)
    
    intensity <-
      intensity %>%
      dplyr::left_join(sample_info %>%
                         dplyr::distinct(!!as.symbol(x),
                                         .keep_all = TRUE),
                       by = setNames(x, x))
    plot <-
      intensity %>%
      ggplot(aes(x = get(x),
                 y = value)) +
      geom_bar(
        stat = "identity",
        aes(fill = get(fill)),
        show.legend = show.legend,
        color = "black"
      ) +
      labs(x = x) +
      theme_bw() +
      scale_x_discrete(expand = expansion(mult = c(0, 0))) +
      scale_y_continuous(expand = expansion(mult = c(0, 0))) +
      guides(fill = guide_legend(title = fill)) +
      theme(axis.text.x = element_text(
        angle = 45,
        hjust = 1,
        vjust = 1
      ))
    
    if (relative) {
      plot <-
        plot +
        labs(y = "Relative abundance (%)")
    } else{
      plot <-
        plot +
        labs(y = "Absolute abundance")
    }
    
    if (!missing(facet_grid)) {
      plot <-
        plot +
        facet_grid(cols = vars(!!as.symbol(facet_grid)),
                   scales = "free_x")
    }
    plot
  }
