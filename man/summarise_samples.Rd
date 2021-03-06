% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods_summarise_samples.R
\name{summarise_samples}
\alias{summarise_samples}
\alias{summarize_samples}
\alias{summarise_samples.microbiome_dataset}
\title{merge samples}
\usage{
summarise_samples(
  object,
  what = c("mean_intensity", "median_intensity", "sum_intensity"),
  group_by,
  ...
)

summarize_samples(
  object,
  what = c("mean_intensity", "median_intensity", "sum_intensity"),
  group_by,
  ...
)

\method{summarise_samples}{microbiome_dataset}(
  object,
  what = c("sum_intensity", "mean_intensity", "median_intensity"),
  group_by,
  ...
)
}
\arguments{
\item{object}{microbiome_dataset}

\item{what}{which you want to use}

\item{group_by}{summarize samples by}

\item{...}{other params}
}
\value{
microbiome_dataset
}
\description{
merge samples
}
