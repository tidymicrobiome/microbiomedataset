% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods_mutate2variable.R
\name{mutate2variable}
\alias{mutate2variable}
\alias{mutate2variable.microbiome_dataset}
\title{Mutate new columns into variable_info}
\usage{
mutate2variable(
  object,
  what = c("mean_intensity", "median_intensity", "sum_intensity", "na_number",
    "na_prop"),
  according_to_samples = "all",
  ...
)

\method{mutate2variable}{microbiome_dataset}(
  object,
  what = c("mean_intensity", "median_intensity", "sum_intensity", "na_number",
    "na_prop"),
  according_to_samples = "all",
  ...
)
}
\arguments{
\item{object}{microbiome_dataset}

\item{what}{which you want to mutate}

\item{according_to_samples}{(required) What samples used to calculate.
Default is "all". If you
want to use only several samples, provide their names as a vector.}

\item{...}{other params}
}
\value{
microbiome_dataset or data.frame.
}
\description{
Mutate new columns into variable_info
}
