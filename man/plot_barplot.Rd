% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plots_barplot.R
\name{plot_barplot}
\alias{plot_barplot}
\alias{plot_barplot.microbiome_dataset}
\title{Barplot based on ggplot2}
\usage{
plot_barplot(
  object,
  fill = c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species"),
  top_n = 5,
  show.legend,
  what = c("sum_intensity", "mean_intensity", "median_intensity"),
  na.rm = TRUE,
  relative = TRUE,
  re_calculate_relative = FALSE,
  x = "sample_id",
  color = "balck",
  facet_grid,
  ...
)

\method{plot_barplot}{microbiome_dataset}(
  object,
  fill = c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species"),
  top_n = 5,
  show.legend,
  what = c("sum_intensity", "mean_intensity", "median_intensity"),
  na.rm = TRUE,
  relative = TRUE,
  re_calculate_relative = FALSE,
  x = "sample_id",
  color = "balck",
  facet_grid,
  ...
)
}
\arguments{
\item{object}{microbiome_dataset}

\item{fill}{fill for barplot}

\item{top_n}{default is 5}

\item{show.legend}{show.legend or not}

\item{what}{which you want to mutate}

\item{na.rm}{na.rm}

\item{relative}{relative intensity or not.}

\item{re_calculate_relative}{re-calculate relative abundance or not.}

\item{x}{x axis}

\item{color}{color for barplot}

\item{facet_grid}{facet_grid}

\item{...}{other params}
}
\value{
ggplot2 class object
}
\description{
Barplot based on ggplot2
}
