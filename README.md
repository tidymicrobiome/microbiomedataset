<!-- README.md is generated from README.Rmd. Please edit that file -->

# microbiomedataset <img src="man/figures/microbiomedataset_logo.png" align="right" alt="microbiomedataset logo" width="120" />

[![R-CMD-check](https://github.com/tidymicrobiome/microbiomedataset/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/tidymicrobiome/microbiomedataset/actions/workflows/R-CMD-check.yaml)
[![pkgdown](https://github.com/tidymicrobiome/microbiomedataset/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/tidymicrobiome/microbiomedataset/actions/workflows/pkgdown.yaml)
[![Lifecycle experimental badge](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

`microbiomedataset` is part of [tidymicrobiome](https://www.tidymicrobiome.org/).

## Why this package

`microbiomedataset` is a data infrastructure package for microbiome analysis.
It provides a standardized object system for:

- abundance matrices
- sample metadata
- taxonomy metadata
- phylogenetic trees
- reference sequences
- process tracking

The package is designed around the same data-engineering philosophy as
`massdataset`, so microbiome and metabolome data can be analyzed with aligned
workflows and linked in cross-omics studies.

## What is new

This package is built around three manuscript-facing ideas.

1. A tree-aware microbiome data model.
`microbiome_dataset` extends the `massdataset` object model with taxonomy,
explicit tree-link metadata, and reference sequences.

2. A unified analysis and visualization layer.
The package standardizes preprocessing, diversity, ordination, differential
abundance, network analysis, and publication-style plotting around a single
input object.

3. A microbiome-metabolome integration layer.
`microbiomedataset` can work directly with `massdataset` objects for paired
sample alignment, correlation analysis, multiblock integration, and
mechanism-oriented taxon-pathway-metabolite linking.

## Installation

Install the development version from GitHub:

``` r
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

remotes::install_github("tidymicrobiome/microbiomedataset")
```

## Core workflows

### 1. Microbiome data infrastructure

``` r
library(microbiomedataset)

data("global_patterns", package = "microbiomedataset")

global_patterns

plot_composition(global_patterns, taxonomic_rank = "Phylum", top_n = 8)
```

### 2. Differential abundance and ordination

``` r
library(microbiomedataset)

data("demo_crossomics", package = "microbiomedataset")

microbiome_object <- summarise_taxa(
  demo_crossomics$microbiome_data,
  taxonomic_rank = "Genus"
)

ordination_result <- run_ordination(microbiome_object, method = "PCoA")
plot_ordination(ordination_result, color_by = "study_group")
```

### 3. Microbiome-metabolome integration

``` r
library(microbiomedataset)

data("demo_crossomics", package = "microbiomedataset")

correlation_result <- calculate_correlation(
  microbiome_data = demo_crossomics$microbiome_data,
  metabolome_data = demo_crossomics$metabolome_data,
  sample_link = demo_crossomics$sample_link,
  microbiome_rank = "Genus",
  method = "spearman",
  metabolome_transform = "none"
)

network_object <- build_correlation_network(
  correlation_result,
  min_abs_correlation = 0.2,
  max_q_value = 1,
  top_n = 25
)

plot_correlation_network(network_object)
```

## Tutorials

Documentation site: <https://tidymicrobiome.github.io/microbiomedataset/>

Key tutorials:

- Get started
- Import and preprocess
- Tree and sequence handling
- Microbiome visualization
- Cross-omics workflow
- Advanced visualization

## Citation

If you use `microbiomedataset`, please cite the package and the associated
paper when available.

## Contact

Xiaotao Shen  
<xiaotao.shen@outlook.com>
