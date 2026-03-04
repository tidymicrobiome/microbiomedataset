demo_global_patterns <- function(n_variables = 200, n_samples = 6) {
  data("global_patterns", package = "microbiomedataset", envir = environment())
  object <- global_patterns
  keep_variables <- as.character(object@variable_info$variable_id[seq_len(n_variables)])
  keep_samples <- as.character(object@sample_info$sample_id[seq_len(n_samples)])
  
  object@expression_data <-
    object@expression_data[keep_variables, keep_samples, drop = FALSE]
  object@variable_info <-
    object@variable_info[match(keep_variables, object@variable_info$variable_id), , drop = FALSE]
  object@sample_info <-
    object@sample_info[match(keep_samples, object@sample_info$sample_id), , drop = FALSE]
  object@sample_info$sample_id <- as.character(object@sample_info$sample_id)
  object@sample_info$class <- as.character(object@sample_info$class)
  object@variable_info$variable_id <- as.character(object@variable_info$variable_id)
  object@otu_tree <- NULL
  object@taxa_tree <- NULL
  object@ref_seq <- NULL
  object
}


demo_crossomics_data <- function() {
  data("demo_crossomics", package = "microbiomedataset", envir = environment())
  demo_crossomics
}


mock_metabolome_dataset <- function(sample_info,
                                    n_metabolites = 5) {
  expression_data <- matrix(
    seq_len(n_metabolites * nrow(sample_info)),
    nrow = n_metabolites,
    ncol = nrow(sample_info)
  )
  expression_data <- as.data.frame(expression_data)
  rownames(expression_data) <- paste0("M", seq_len(nrow(expression_data)))
  colnames(expression_data) <- sample_info$sample_id
  
  variable_info <- data.frame(
    variable_id = rownames(expression_data),
    mz = seq(100, by = 10, length.out = nrow(expression_data)),
    rt = seq(1, by = 0.5, length.out = nrow(expression_data)),
    stringsAsFactors = FALSE
  )
  
  object <- massdataset::create_mass_dataset(
    expression_data = expression_data,
    sample_info = sample_info,
    variable_info = variable_info
  )
  
  object@annotation_table <- data.frame(
    variable_id = variable_info$variable_id,
    compound_name = paste0("compound_", seq_len(nrow(variable_info))),
    pathway_id = rep(c("pathway_a", "pathway_b"), length.out = nrow(variable_info)),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  
  object
}


test_that("create_microbiome_dataset normalizes taxonomy schema from demo data", {
  object <- demo_global_patterns(n_variables = 120, n_samples = 5)
  variable_info <- object@variable_info
  colnames(variable_info)[colnames(variable_info) == "Kingdom"] <- "Domain"
  
  created <- microbiomedataset::create_microbiome_dataset(
    expression_data = object@expression_data,
    sample_info = object@sample_info,
    variable_info = variable_info
  )
  
  expect_true(methods::is(created, "microbiome_dataset"))
  expect_true("Kingdom" %in% colnames(created@variable_info))
  expect_false("Domain" %in% colnames(created@variable_info))
  expect_identical(microbiomedataset::check_microbiome_dataset_class(created), TRUE)
})


test_that("phyloseq conversion round-trip preserves demo data dimensions and taxonomy columns", {
  object <- demo_global_patterns(n_variables = 150, n_samples = 5)
  
  phyloseq_object <- microbiomedataset::convert2phyloseq(object)
  roundtrip <- microbiomedataset::convert2microbiome_dataset(phyloseq_object)
  
  expect_identical(dim(roundtrip@expression_data), dim(object@expression_data))
  expect_identical(roundtrip@sample_info$sample_id, object@sample_info$sample_id)
  expect_true(all(microbiomedataset:::microbiome_taxonomy_ranks() %in% colnames(roundtrip@variable_info)))
  expect_identical(microbiomedataset::check_microbiome_dataset_class(roundtrip), TRUE)
})


test_that("summarise_variables keeps expression_data and variable_info aligned", {
  object <- demo_global_patterns(n_variables = 300, n_samples = 6)
  object <-
    object %>%
    microbiomedataset::activate_microbiome_dataset("variable_info") %>%
    dplyr::filter(!is.na(Genus))
  
  expected <-
    rowsum(
      object@expression_data,
      group = object@variable_info$Genus,
      reorder = FALSE
    )
  
  summarised <-
    microbiomedataset::summarise_variables(
      object = object,
      what = "sum_intensity",
      group_by = "Genus"
    )
  
  observed_groups <- summarised@variable_info$Genus
  observed <- summarised@expression_data
  expected <- expected[observed_groups, , drop = FALSE]
  rownames(expected) <- summarised@variable_info$variable_id
  
  expect_identical(dim(observed), dim(expected))
  expect_equal(as.matrix(observed), as.matrix(expected))
  expect_identical(microbiomedataset::check_microbiome_dataset_class(summarised), TRUE)
})


test_that("filter keeps sample and variable metadata synchronized", {
  object <- demo_global_patterns(n_variables = 180, n_samples = 8)
  
  filtered_samples <-
    object %>%
    microbiomedataset::activate_microbiome_dataset("sample_info") %>%
    dplyr::filter(SampleType == "Soil")
  
  expect_identical(
    colnames(filtered_samples@expression_data),
    filtered_samples@sample_info$sample_id
  )
  expect_identical(microbiomedataset::check_microbiome_dataset_class(filtered_samples), TRUE)
  
  filtered_variables <-
    object %>%
    microbiomedataset::activate_microbiome_dataset("variable_info") %>%
    dplyr::filter(!is.na(Family))
  
  expect_identical(
    rownames(filtered_variables@expression_data),
    filtered_variables@variable_info$variable_id
  )
  expect_identical(microbiomedataset::check_microbiome_dataset_class(filtered_variables), TRUE)
})


test_that("prune_samples keeps sample_info and expression_data synchronized", {
  object <- demo_global_patterns(n_variables = 160, n_samples = 8)
  keep_samples <- as.character(object@sample_info$sample_id[c(1, 3, 5)])
  
  pruned <- prune_samples(object, sample_id = keep_samples)
  
  expect_identical(pruned@sample_info$sample_id, keep_samples)
  expect_identical(colnames(pruned@expression_data), keep_samples)
  expect_identical(microbiomedataset::check_microbiome_dataset_class(pruned), TRUE)
})


test_that("prune_taxa keeps variable_info, tree, and ref_seq synchronized", {
  object <- demo_global_patterns(n_variables = 180, n_samples = 6)
  keep_taxa <- as.character(object@variable_info$variable_id[c(2, 4, 6, 8, 10)])
  
  phyloseq_object <- microbiomedataset::convert2phyloseq(object)
  object <- microbiomedataset::convert2microbiome_dataset(phyloseq_object)
  otu_phylo <- ape::rtree(
    n = nrow(object@variable_info),
    tip.label = object@variable_info$variable_id
  )
  object@otu_tree <-
    tidytree::treedata(
      phylo = otu_phylo,
      data = tibble::tibble(node = seq_len(length(otu_phylo$tip.label) + otu_phylo$Nnode))
    )
  object@ref_seq <- Biostrings::DNAStringSet(rep("ACGT", nrow(object@variable_info)))
  names(object@ref_seq) <- object@variable_info$variable_id
  
  pruned <- prune_taxa(object, variable_id = keep_taxa)
  
  expect_identical(pruned@variable_info$variable_id, keep_taxa)
  expect_identical(rownames(pruned@expression_data), keep_taxa)
  expect_identical(sort(names(pruned@ref_seq)), sort(keep_taxa))
  expect_identical(sort(microbiomedataset:::get_tree_tip_labels(pruned@otu_tree)), sort(keep_taxa))
  expect_identical(microbiomedataset::check_microbiome_dataset_class(pruned), TRUE)
})


test_that("agglomerate_taxa collapses taxa to the requested rank", {
  object <- demo_global_patterns(n_variables = 300, n_samples = 6)
  
  agglomerated <-
    agglomerate_taxa(
      object = object,
      taxonomic_rank = "Genus",
      what = "sum_intensity",
      na_remove = TRUE
    )
  
  expect_true(all(!is.na(agglomerated@variable_info$Genus)))
  expect_identical(
    agglomerated@variable_info$variable_id,
    agglomerated@variable_info$Genus
  )
  expect_null(agglomerated@otu_tree)
  expect_null(agglomerated@ref_seq)
  expect_identical(microbiomedataset::check_microbiome_dataset_class(agglomerated), TRUE)
})


test_that("merge_samples collapses samples by metadata column", {
  object <- demo_global_patterns(n_variables = 180, n_samples = 8)
  
  merged <-
    merge_samples(
      object = object,
      group_by = "SampleType",
      what = "sum_intensity"
    )
  
  expect_identical(
    merged@sample_info$sample_id,
    unique(as.character(object@sample_info$SampleType))
  )
  expect_identical(
    colnames(merged@expression_data),
    merged@sample_info$sample_id
  )
  expect_identical(microbiomedataset::check_microbiome_dataset_class(merged), TRUE)
})


test_that("sample_sums, taxa_sums, and taxa_prevalence return expected summaries", {
  object <- demo_global_patterns(n_variables = 120, n_samples = 6)
  
  observed_sample_sums <- sample_sums(object)
  expected_sample_sums <- stats::setNames(colSums(object@expression_data), colnames(object@expression_data))
  expect_equal(observed_sample_sums, expected_sample_sums)
  
  observed_taxa_sums <- taxa_sums(object)
  expected_taxa_sums <- stats::setNames(rowSums(object@expression_data), rownames(object@expression_data))
  expect_equal(observed_taxa_sums, expected_taxa_sums)
  
  observed_prevalence <- taxa_prevalence(object, detection = 0)
  expected_prevalence <- stats::setNames(
    rowSums(object@expression_data > 0, na.rm = TRUE),
    rownames(object@expression_data)
  )
  expect_equal(observed_prevalence, expected_prevalence)
})


test_that("filter_taxa prunes taxa using abundance and prevalence thresholds", {
  object <- demo_global_patterns(n_variables = 160, n_samples = 6)
  
  prevalence <- taxa_prevalence(object, detection = 0)
  abundance <- taxa_sums(object)
  keep_ids <- names(prevalence)[prevalence >= 3 & abundance >= 50]
  
  filtered <- filter_taxa(
    object,
    min_prevalence = 3,
    min_abundance = 50
  )
  
  expect_identical(filtered@variable_info$variable_id, keep_ids)
  expect_identical(rownames(filtered@expression_data), keep_ids)
  expect_identical(microbiomedataset::check_microbiome_dataset_class(filtered), TRUE)
})


test_that("filter_samples prunes samples using abundance thresholds", {
  object <- demo_global_patterns(n_variables = 160, n_samples = 8)
  
  totals <- sample_sums(object)
  keep_ids <- names(totals)[totals >= stats::median(totals)]
  
  filtered <- filter_samples(
    object,
    min_abundance = stats::median(totals)
  )
  
  expect_identical(filtered@sample_info$sample_id, keep_ids)
  expect_identical(colnames(filtered@expression_data), keep_ids)
  expect_identical(microbiomedataset::check_microbiome_dataset_class(filtered), TRUE)
})


test_that("transform_counts supports relative, log, presence_absence, and clr", {
  object <- demo_global_patterns(n_variables = 80, n_samples = 5)
  
  relative <- transform_counts(object, method = "relative")
  expect_equal(unname(colSums(relative@expression_data)), rep(1, ncol(relative@expression_data)))
  expect_identical(attr(relative@expression_data, "counts_or_relative"), "relative")
  expect_identical(attr(relative@expression_data, "transform_method"), "relative")
  
  logged <- transform_counts(object, method = "log", pseudocount = 1)
  expect_true(all(logged@expression_data >= 0))
  expect_identical(attr(logged@expression_data, "transform_method"), "log")
  
  pa <- transform_counts(object, method = "presence_absence")
  expect_true(all(unique(as.vector(as.matrix(pa@expression_data))) %in% c(0, 1, NA)))
  expect_identical(attr(pa@expression_data, "transform_method"), "presence_absence")
  
  clr_object <- transform_counts(object, method = "clr", pseudocount = 1)
  expect_equal(
    unname(colMeans(clr_object@expression_data)),
    rep(0, ncol(clr_object@expression_data)),
    tolerance = 1e-8
  )
  expect_identical(attr(clr_object@expression_data, "transform_method"), "clr")
})


test_that("transform2relative_intensity delegates to transform_counts", {
  object <- demo_global_patterns(n_variables = 80, n_samples = 5)
  
  legacy <- transform2relative_intensity(object)
  current <- transform_counts(object, method = "relative")
  
  expect_equal(legacy@expression_data, current@expression_data)
  expect_identical(attr(legacy@expression_data, "counts_or_relative"), "relative")
})


test_that("psmelt_microbiome_dataset returns a long table with sample and variable metadata", {
  object <- demo_global_patterns(n_variables = 60, n_samples = 5)
  
  melted <- psmelt_microbiome_dataset(object)
  
  expect_identical(nrow(melted), nrow(object@expression_data) * ncol(object@expression_data))
  expect_true(all(c("sample_id", "variable_id", "abundance") %in% colnames(melted)))
  expect_true(all(c("SampleType", "class", "Kingdom", "Phylum") %in% colnames(melted)))
  
  first_row <- melted[1, , drop = FALSE]
  expect_identical(
    first_row$abundance,
    object@expression_data[first_row$variable_id, first_row$sample_id, drop = TRUE]
  )
})


test_that("psmelt_microbiome_dataset supports relative abundance export", {
  object <- demo_global_patterns(n_variables = 60, n_samples = 5)
  
  melted <- psmelt_microbiome_dataset(object, relative = TRUE)
  sample_totals <- tapply(melted$abundance, melted$sample_id, sum)
  
  expect_equal(as.vector(sample_totals), rep(1, length(sample_totals)))
})


test_that("psmelt_taxa returns rank-level long table with sample metadata", {
  object <- demo_global_patterns(n_variables = 80, n_samples = 5)
  
  melted <- psmelt_taxa(
    object,
    taxonomic_rank = "Genus",
    relative = FALSE,
    what = "sum_intensity"
  )
  
  expect_true(all(c("sample_id", "Genus", "abundance", "SampleType", "class") %in% colnames(melted)))
  
  expected <- extract_intensity(
    object,
    taxonomic_rank = "Genus",
    data_type = "longer",
    relative = FALSE,
    what = "sum_intensity"
  )
  colnames(expected)[colnames(expected) == "value"] <- "abundance"
  
  merged <- dplyr::left_join(
    melted[, c("sample_id", "Genus", "abundance")],
    expected,
    by = c("sample_id", "Genus", "abundance")
  )
  expect_identical(nrow(merged), nrow(expected))
})


test_that("psmelt_taxa supports relative abundance export", {
  object <- demo_global_patterns(n_variables = 80, n_samples = 5)
  
  melted <- psmelt_taxa(
    object,
    taxonomic_rank = "Phylum",
    relative = TRUE
  )
  sample_totals <- tapply(melted$abundance, melted$sample_id, sum)
  
  expect_equal(as.vector(sample_totals), rep(100, length(sample_totals)))
})


test_that("melt_features and melt_taxa are neutral aliases of psmelt helpers", {
  object <- demo_global_patterns(n_variables = 80, n_samples = 5)
  
  feature_a <- psmelt_microbiome_dataset(object, relative = TRUE)
  feature_b <- melt_features(object, relative = TRUE)
  expect_equal(feature_b, feature_a)
  
  taxa_a <- psmelt_taxa(object, taxonomic_rank = "Genus", relative = FALSE)
  taxa_b <- melt_taxa(object, taxonomic_rank = "Genus", relative = FALSE)
  expect_equal(taxa_b, taxa_a)
})


test_that("filter_taxa_by_rank keeps requested taxa labels and preserves object consistency", {
  object <- demo_global_patterns(n_variables = 120, n_samples = 6)
  keep_phyla <- unique(stats::na.omit(as.character(object@variable_info$Phylum)))[1:2]
  
  filtered <- filter_taxa_by_rank(
    object = object,
    taxonomic_rank = "Phylum",
    taxa = keep_phyla
  )
  
  expect_true(all(stats::na.omit(as.character(filtered@variable_info$Phylum)) %in% keep_phyla))
  expect_identical(
    rownames(filtered@expression_data),
    filtered@variable_info$variable_id
  )
  expect_identical(microbiomedataset::check_microbiome_dataset_class(filtered), TRUE)
})


test_that("summarise_taxa delegates to agglomerate_taxa", {
  object <- demo_global_patterns(n_variables = 160, n_samples = 6)
  
  observed <- summarise_taxa(object, taxonomic_rank = "Family", what = "sum_intensity")
  expected <- agglomerate_taxa(object, taxonomic_rank = "Family", what = "sum_intensity")
  
  expect_equal(observed@expression_data, expected@expression_data)
  expect_equal(observed@variable_info, expected@variable_info)
})


test_that("filter_samples_by_group keeps requested sample groups", {
  object <- demo_global_patterns(n_variables = 120, n_samples = 8)
  keep_groups <- unique(as.character(object@sample_info$SampleType))[1:2]
  
  filtered <- filter_samples_by_group(
    object = object,
    group_by = "SampleType",
    groups = keep_groups
  )
  
  expect_true(all(as.character(filtered@sample_info$SampleType) %in% keep_groups))
  expect_identical(
    colnames(filtered@expression_data),
    filtered@sample_info$sample_id
  )
  expect_identical(microbiomedataset::check_microbiome_dataset_class(filtered), TRUE)
})


test_that("transform_taxa summarizes rank-level abundance before transformation", {
  object <- demo_global_patterns(n_variables = 160, n_samples = 6)
  
  transformed <- transform_taxa(
    object = object,
    taxonomic_rank = "Genus",
    what = "sum_intensity",
    method = "relative"
  )
  
  expected <- summarise_taxa(
    object = object,
    taxonomic_rank = "Genus",
    what = "sum_intensity"
  )
  expected <- transform_counts(expected, method = "relative")
  
  expect_equal(transformed@expression_data, expected@expression_data)
  expect_identical(transformed@variable_info, expected@variable_info)
  expect_equal(unname(colSums(transformed@expression_data)), rep(1, ncol(transformed@expression_data)))
  expect_identical(attr(transformed@expression_data, "transform_method"), "relative")
})


test_that("summarise_samples_by_group delegates to summarise_samples", {
  object <- demo_global_patterns(n_variables = 160, n_samples = 8)
  
  observed <- summarise_samples_by_group(
    object = object,
    group_by = "SampleType",
    what = "sum_intensity"
  )
  expected <- summarise_samples(
    object = object,
    group_by = "SampleType",
    what = "sum_intensity"
  )
  
  expect_equal(observed@expression_data, expected@expression_data)
  expect_equal(observed@sample_info, expected@sample_info)
})


test_that("transform_samples summarizes sample groups before transformation", {
  object <- demo_global_patterns(n_variables = 160, n_samples = 8)
  
  transformed <- transform_samples(
    object = object,
    group_by = "SampleType",
    what = "sum_intensity",
    method = "relative"
  )
  
  expected <- summarise_samples_by_group(
    object = object,
    group_by = "SampleType",
    what = "sum_intensity"
  )
  expected <- transform_counts(expected, method = "relative")
  
  expect_equal(transformed@expression_data, expected@expression_data)
  expect_identical(transformed@sample_info, expected@sample_info)
  expect_equal(unname(colSums(transformed@expression_data)), rep(1, ncol(transformed@expression_data)))
  expect_identical(attr(transformed@expression_data, "transform_method"), "relative")
})


test_that("summarise_abundance dispatches to taxa and sample summarisation verbs", {
  object <- demo_global_patterns(n_variables = 160, n_samples = 8)
  
  taxa_observed <- summarise_abundance(
    object = object,
    by = "taxa",
    group_by = "Genus",
    what = "sum_intensity"
  )
  taxa_expected <- summarise_taxa(
    object = object,
    taxonomic_rank = "Genus",
    what = "sum_intensity"
  )
  expect_equal(taxa_observed@expression_data, taxa_expected@expression_data)
  expect_equal(taxa_observed@variable_info, taxa_expected@variable_info)
  
  sample_observed <- summarise_abundance(
    object = object,
    by = "samples",
    group_by = "SampleType",
    what = "sum_intensity"
  )
  sample_expected <- summarise_samples_by_group(
    object = object,
    group_by = "SampleType",
    what = "sum_intensity"
  )
  expect_equal(sample_observed@expression_data, sample_expected@expression_data)
  expect_equal(sample_observed@sample_info, sample_expected@sample_info)
})


test_that("transform_abundance_by dispatches to taxa and sample transformation verbs", {
  object <- demo_global_patterns(n_variables = 160, n_samples = 8)
  
  taxa_observed <- transform_abundance_by(
    object = object,
    by = "taxa",
    group_by = "Family",
    what = "sum_intensity",
    method = "relative"
  )
  taxa_expected <- transform_taxa(
    object = object,
    taxonomic_rank = "Family",
    what = "sum_intensity",
    method = "relative"
  )
  expect_equal(taxa_observed@expression_data, taxa_expected@expression_data)
  expect_equal(taxa_observed@variable_info, taxa_expected@variable_info)
  
  sample_observed <- transform_abundance_by(
    object = object,
    by = "samples",
    group_by = "SampleType",
    what = "sum_intensity",
    method = "relative"
  )
  sample_expected <- transform_samples(
    object = object,
    group_by = "SampleType",
    what = "sum_intensity",
    method = "relative"
  )
  expect_equal(sample_observed@expression_data, sample_expected@expression_data)
  expect_equal(sample_observed@sample_info, sample_expected@sample_info)
})


test_that("prune_tree and align_tree update tree slots without breaking object validity", {
  object <- demo_global_patterns(n_variables = 120, n_samples = 6)
  phyloseq_object <- microbiomedataset::convert2phyloseq(object)
  object <- microbiomedataset::convert2microbiome_dataset(phyloseq_object)
  
  otu_phylo <- ape::rtree(
    n = nrow(object@variable_info) + 2,
    tip.label = c(object@variable_info$variable_id, "extra_1", "extra_2")
  )
  object@otu_tree <- tidytree::treedata(
    phylo = otu_phylo,
    data = tibble::tibble(node = seq_len(length(otu_phylo$tip.label) + otu_phylo$Nnode))
  )
  
  current_taxa_tree <- object@taxa_tree
  taxa_phylo <- tidytree::as.phylo(current_taxa_tree)
  keep_taxa_tips <- taxa_phylo$tip.label[seq_len(min(5, length(taxa_phylo$tip.label)))]
  
  pruned_taxa_tree <- prune_tree(
    object = object,
    tree = "taxa_tree",
    tip_label = keep_taxa_tips
  )
  expect_identical(
    sort(microbiomedataset:::get_tree_tip_labels(pruned_taxa_tree@taxa_tree)),
    sort(keep_taxa_tips)
  )
  
  aligned_otu_tree <- align_tree(object, tree = "otu_tree")
  expect_identical(
    sort(microbiomedataset:::get_tree_tip_labels(aligned_otu_tree@otu_tree)),
    sort(object@variable_info$variable_id)
  )
  
  aligned_taxa_tree <- align_tree(aligned_otu_tree, tree = "taxa_tree")
  expect_false(is.null(aligned_taxa_tree@taxa_tree))
  expect_identical(microbiomedataset::check_microbiome_dataset_class(aligned_taxa_tree), TRUE)
})


test_that("extract_tree_data exports treedata, phylo, and table views", {
  object <- demo_global_patterns(n_variables = 120, n_samples = 6)
  object <- microbiomedataset::convert2microbiome_dataset(
    microbiomedataset::convert2phyloseq(object)
  )
  
  taxa_treedata <- extract_tree_data(object, tree = "taxa_tree", data_type = "treedata")
  taxa_phylo <- extract_tree_data(object, tree = "taxa_tree", data_type = "phylo")
  taxa_table <- extract_tree_data(object, tree = "taxa_tree", data_type = "table")
  
  expect_true(methods::is(taxa_treedata, "treedata"))
  expect_true(methods::is(taxa_phylo, "phylo"))
  expect_true(is.data.frame(taxa_table))
  expect_true(all(c("node", "parent") %in% colnames(taxa_table)))
})


test_that("filter_tree_by_rank rebuilds taxa_tree using selected taxonomy labels", {
  object <- demo_global_patterns(n_variables = 120, n_samples = 6)
  object <- microbiomedataset::convert2microbiome_dataset(
    microbiomedataset::convert2phyloseq(object)
  )
  keep_phyla <- unique(stats::na.omit(as.character(object@variable_info$Phylum)))[1:2]
  
  filtered <- filter_tree_by_rank(
    object = object,
    taxonomic_rank = "Phylum",
    taxa = keep_phyla
  )
  
  tree_table <- extract_tree_data(filtered, tree = "taxa_tree", data_type = "table")
  phylum_labels <- paste0("p__", keep_phyla)
  
  expect_false(is.null(filtered@taxa_tree))
  expect_true(any(tree_table$label %in% phylum_labels))
  expect_identical(
    rownames(filtered@expression_data),
    filtered@variable_info$variable_id
  )
  expect_identical(microbiomedataset::check_microbiome_dataset_class(filtered), TRUE)
})


test_that("replace_tree aligns OTU trees and can rebuild taxa trees", {
  object <- demo_global_patterns(n_variables = 100, n_samples = 6)
  object <- microbiomedataset::convert2microbiome_dataset(
    microbiomedataset::convert2phyloseq(object)
  )
  
  otu_phylo <- ape::rtree(
    n = nrow(object@variable_info) + 2,
    tip.label = c(object@variable_info$variable_id, "extra_tip_1", "extra_tip_2")
  )
  otu_tree <- tidytree::treedata(
    phylo = otu_phylo,
    data = tibble::tibble(node = seq_len(length(otu_phylo$tip.label) + otu_phylo$Nnode))
  )
  
  replaced_otu <- replace_tree(
    object = object,
    tree = "otu_tree",
    value = otu_tree,
    align = TRUE
  )
  expect_identical(
    sort(microbiomedataset:::get_tree_tip_labels(replaced_otu@otu_tree)),
    sort(object@variable_info$variable_id)
  )
  
  no_taxa_tree <- object
  no_taxa_tree@taxa_tree <- NULL
  rebuilt_taxa <- replace_tree(
    object = no_taxa_tree,
    tree = "taxa_tree",
    value = NULL,
    align = TRUE
  )
  expect_false(is.null(rebuilt_taxa@taxa_tree))
  expect_identical(microbiomedataset::check_microbiome_dataset_class(rebuilt_taxa), TRUE)
})


test_that("replace_ref_seq, align_ref_seq, and prune_ref_seq keep sequences synchronized", {
  object <- demo_global_patterns(n_variables = 100, n_samples = 6)
  object <- microbiomedataset::convert2microbiome_dataset(
    microbiomedataset::convert2phyloseq(object)
  )
  
  ref_seq <- Biostrings::DNAStringSet(rep("ACGT", nrow(object@variable_info) + 2))
  names(ref_seq) <- c(object@variable_info$variable_id, "extra_seq_1", "extra_seq_2")
  
  replaced <- replace_ref_seq(object, value = ref_seq, align = TRUE)
  expect_identical(
    names(replaced@ref_seq),
    object@variable_info$variable_id
  )
  
  unnamed <- Biostrings::DNAStringSet(rep("ACGT", nrow(object@variable_info)))
  aligned <- replace_ref_seq(object, value = unnamed, align = TRUE)
  expect_identical(
    names(aligned@ref_seq),
    object@variable_info$variable_id
  )
  
  pruned <- prune_ref_seq(
    aligned,
    variable_id = object@variable_info$variable_id[1:5]
  )
  expect_identical(
    names(pruned@ref_seq),
    object@variable_info$variable_id[1:5]
  )
  expect_identical(microbiomedataset::check_microbiome_dataset_class(aligned), TRUE)
})


test_that("melt_tree returns a tidy table view of tree slots", {
  object <- demo_global_patterns(n_variables = 120, n_samples = 6)
  object <- microbiomedataset::convert2microbiome_dataset(
    microbiomedataset::convert2phyloseq(object)
  )
  
  tree_tbl <- melt_tree(object, tree = "taxa_tree")
  
  expect_true(is.data.frame(tree_tbl))
  expect_true(all(c("tree", "node", "parent") %in% colnames(tree_tbl)))
  expect_true(all(tree_tbl$tree == "taxa_tree"))
})


test_that("export_ref_seq writes FASTA from ref_seq", {
  object <- demo_global_patterns(n_variables = 40, n_samples = 5)
  object <- microbiomedataset::convert2microbiome_dataset(
    microbiomedataset::convert2phyloseq(object)
  )
  
  ref_seq <- Biostrings::DNAStringSet(rep("ACGT", nrow(object@variable_info)))
  object <- replace_ref_seq(object, value = ref_seq)
  fasta_file <- tempfile(fileext = ".fasta")
  
  returned_file <- export_ref_seq(object, fasta_file)
  fasta_lines <- readLines(fasta_file)
  
  expect_identical(returned_file, fasta_file)
  expect_true(file.exists(fasta_file))
  expect_true(any(startsWith(fasta_lines, ">")))
  expect_true(any(fasta_lines == "ACGT"))
})


test_that("plot_tree returns a ggplot object for tree slots", {
  object <- demo_global_patterns(n_variables = 120, n_samples = 6)
  object <- microbiomedataset::convert2microbiome_dataset(
    microbiomedataset::convert2phyloseq(object)
  )
  
  p <- plot_tree(object, tree = "taxa_tree")
  
  expect_true(inherits(p, "ggplot"))
})


test_that("import_ref_seq reads FASTA and aligns sequences to variable_info", {
  object <- demo_global_patterns(n_variables = 40, n_samples = 5)
  object <- microbiomedataset::convert2microbiome_dataset(
    microbiomedataset::convert2phyloseq(object)
  )
  
  fasta_file <- tempfile(fileext = ".fasta")
  ref_seq <- Biostrings::DNAStringSet(rep("ACGT", nrow(object@variable_info) + 1))
  names(ref_seq) <- c(object@variable_info$variable_id, "extra_seq")
  Biostrings::writeXStringSet(ref_seq, fasta_file, format = "fasta")
  
  imported <- import_ref_seq(object, fasta_file, align = TRUE)
  
  expect_identical(
    names(imported@ref_seq),
    object@variable_info$variable_id
  )
  expect_identical(microbiomedataset::check_microbiome_dataset_class(imported), TRUE)
})


test_that("plot_tree supports metadata and abundance mappings", {
  object <- demo_global_patterns(n_variables = 120, n_samples = 6)
  object <- microbiomedataset::convert2microbiome_dataset(
    microbiomedataset::convert2phyloseq(object)
  )
  
  otu_phylo <- ape::rtree(
    n = nrow(object@variable_info),
    tip.label = object@variable_info$variable_id
  )
  object@otu_tree <- tidytree::treedata(
    phylo = otu_phylo,
    data = tibble::tibble(node = seq_len(length(otu_phylo$tip.label) + otu_phylo$Nnode))
  )
  
  p1 <- plot_tree(object, tree = "taxa_tree", color_by = "nodeClass")
  p2 <- plot_tree(object, tree = "otu_tree", color_by = "Phylum", size_by = "abundance")
  p3 <- plot_tree(
    object,
    tree = "taxa_tree",
    color_by = "abundance",
    taxonomic_rank = "Phylum"
  )
  
  expect_true(inherits(p1, "ggplot"))
  expect_true(inherits(p2, "ggplot"))
  expect_true(inherits(p3, "ggplot"))
})


test_that("plot_tree can fall back to label-based abundance mapping", {
  object <- demo_global_patterns(n_variables = 60, n_samples = 5)
  object <- microbiomedataset::convert2microbiome_dataset(
    microbiomedataset::convert2phyloseq(object)
  )
  
  p <- plot_tree(
    object,
    tree = "taxa_tree",
    color_by = "abundance",
    taxonomic_rank = "Phylum",
    use_link = FALSE
  )
  
  expect_true(inherits(p, "ggplot"))
})


test_that("import_tree reads Newick and aligns OTU trees", {
  object <- demo_global_patterns(n_variables = 60, n_samples = 5)
  object <- microbiomedataset::convert2microbiome_dataset(
    microbiomedataset::convert2phyloseq(object)
  )
  
  tree_file <- tempfile(fileext = ".nwk")
  otu_phylo <- ape::rtree(
    n = nrow(object@variable_info) + 1,
    tip.label = c(object@variable_info$variable_id, "extra_tip")
  )
  ape::write.tree(otu_phylo, file = tree_file)
  
  imported <- import_tree(object, file = tree_file, tree = "otu_tree", align = TRUE)
  
  expect_identical(
    sort(microbiomedataset:::get_tree_tip_labels(imported@otu_tree)),
    sort(object@variable_info$variable_id)
  )
  expect_identical(microbiomedataset::check_microbiome_dataset_class(imported), TRUE)
})


test_that("export_tree writes Newick and Nexus files", {
  object <- demo_global_patterns(n_variables = 60, n_samples = 5)
  object <- microbiomedataset::convert2microbiome_dataset(
    microbiomedataset::convert2phyloseq(object)
  )
  
  newick_file <- tempfile(fileext = ".nwk")
  nexus_file <- tempfile(fileext = ".nex")
  
  returned_newick <- export_tree(object, file = newick_file, tree = "taxa_tree", format = "newick")
  returned_nexus <- export_tree(object, file = nexus_file, tree = "taxa_tree", format = "nexus")
  
  expect_identical(returned_newick, newick_file)
  expect_identical(returned_nexus, nexus_file)
  expect_true(file.exists(newick_file))
  expect_true(file.exists(nexus_file))
  expect_true(length(readLines(newick_file, warn = FALSE)) >= 1)
  expect_true(any(grepl("#NEXUS", readLines(nexus_file, warn = FALSE), fixed = TRUE)))
})


test_that("calculate_alpha_diversity returns an objectized alpha diversity result", {
  object <- demo_global_patterns(n_variables = 80, n_samples = 6)
  
  alpha <- calculate_alpha_diversity(object, metric = "shannon")
  
  expect_true(methods::is(alpha, "microbiome_diversity"))
  expect_identical(alpha@type, "alpha")
  expect_identical(alpha@method, "shannon")
  expect_true(all(c("sample_id", "value") %in% colnames(alpha@result)))
  expect_identical(alpha@result$sample_id, object@sample_info$sample_id)
})


test_that("calculate_beta_diversity returns a distance-based diversity object", {
  object <- demo_global_patterns(n_variables = 80, n_samples = 6)
  
  beta <- calculate_beta_diversity(object, method = "bray")
  
  expect_true(methods::is(beta, "microbiome_diversity"))
  expect_identical(beta@type, "beta")
  expect_identical(beta@method, "bray")
  expect_true(inherits(beta@result, "dist"))
  expect_identical(attr(beta@result, "Labels"), object@sample_info$sample_id)
})


test_that("run_ordination returns objectized PCoA and PCA results", {
  object <- demo_global_patterns(n_variables = 80, n_samples = 6)
  
  pcoa <- run_ordination(object, method = "PCoA", distance_method = "bray", n_axes = 2)
  pca <- run_ordination(object, method = "PCA", n_axes = 2)
  
  expect_true(methods::is(pcoa, "microbiome_ordination"))
  expect_identical(pcoa@method, "PCoA")
  expect_identical(pcoa@distance_method, "bray")
  expect_true(all(c("sample_id", "Axis.1", "Axis.2") %in% colnames(pcoa@sample_coord)))
  
  expect_true(methods::is(pca, "microbiome_ordination"))
  expect_identical(pca@method, "PCA")
  expect_identical(pca@distance_method, "")
  expect_true(all(c("sample_id", "PC1", "PC2") %in% colnames(pca@sample_coord)))
})


test_that("agglomerate_tree collapses OTU abundance using tree clusters", {
  object <- demo_global_patterns(n_variables = 80, n_samples = 6)
  object <- microbiomedataset::convert2microbiome_dataset(
    microbiomedataset::convert2phyloseq(object)
  )
  otu_phylo <- ape::rtree(
    n = nrow(object@variable_info),
    tip.label = object@variable_info$variable_id
  )
  object@otu_tree <- tidytree::treedata(
    phylo = otu_phylo,
    data = tibble::tibble(node = seq_len(length(otu_phylo$tip.label) + otu_phylo$Nnode))
  )
  
  agglomerated <- agglomerate_tree(object, k = 10, what = "sum_intensity")
  
  expect_true(methods::is(agglomerated, "microbiome_dataset"))
  expect_true(nrow(agglomerated@expression_data) <= 10)
  expect_true(all(startsWith(agglomerated@variable_info$variable_id, "tree_cluster_")))
  expect_null(agglomerated@otu_tree)
  expect_null(agglomerated@ref_seq)
  expect_identical(
    rownames(agglomerated@expression_data),
    agglomerated@variable_info$variable_id
  )
  expect_identical(microbiomedataset::check_microbiome_dataset_class(agglomerated), TRUE)
})


test_that("extractors and as_tibble methods expose diversity and ordination results", {
  object <- demo_global_patterns(n_variables = 80, n_samples = 6)
  
  alpha <- calculate_alpha_diversity(object, metric = "simpson")
  beta <- calculate_beta_diversity(object, method = "euclidean")
  ord <- run_ordination(object, method = "PCoA", distance_method = "euclidean", n_axes = 2)
  
  expect_true(is.data.frame(extract_diversity_result(alpha)))
  expect_true(inherits(extract_diversity_result(beta), "dist"))
  expect_true(is.data.frame(extract_ordination_result(ord)))
  
  alpha_tbl <- as_tibble_diversity(alpha)
  beta_tbl <- as_tibble_diversity(beta)
  ord_tbl <- as_tibble_ordination(ord)
  
  expect_true(is.data.frame(alpha_tbl))
  expect_true(is.data.frame(beta_tbl))
  expect_true(is.data.frame(ord_tbl))
  expect_true(all(c("sample_id", "value") %in% colnames(alpha_tbl)))
  expect_true("sample_id" %in% colnames(beta_tbl))
  expect_true("sample_id" %in% colnames(ord_tbl))
})


test_that("plot_ordination returns a ggplot object", {
  object <- demo_global_patterns(n_variables = 80, n_samples = 6)
  ord <- run_ordination(object, method = "PCoA", distance_method = "bray", n_axes = 2)
  
  p <- plot_ordination(ord, color_by = "SampleType", label_samples = TRUE)
  
  expect_true(inherits(p, "ggplot"))
})


test_that("plot_alpha_diversity and plot_beta_diversity return ggplot objects", {
  object <- demo_global_patterns(n_variables = 80, n_samples = 6)
  alpha <- calculate_alpha_diversity(object, metric = "shannon")
  beta <- calculate_beta_diversity(object, method = "bray")
  annotation <- stats::setNames(as.character(object@sample_info$SampleType), object@sample_info$sample_id)
  
  p_alpha <- plot_alpha_diversity(alpha, x = "SampleType", color_by = "SampleType")
  p_beta <- plot_beta_diversity(beta, annotate_by = annotation, cluster = TRUE)
  
  expect_true(inherits(p_alpha, "ggplot"))
  expect_true(inherits(p_beta, "ggplot"))
})


test_that("plot_ordination supports ellipse, centroid, and loading layers", {
  object <- demo_global_patterns(n_variables = 80, n_samples = 6)
  
  pcoa <- run_ordination(object, method = "PCoA", distance_method = "bray", n_axes = 2)
  pca <- run_ordination(object, method = "PCA", n_axes = 2)
  
  p1 <- plot_ordination(
    pcoa,
    color_by = "SampleType",
    ellipse_by = "SampleType",
    centroid_by = "SampleType"
  )
  p2 <- plot_ordination(
    pca,
    color_by = "SampleType",
    show_loading = TRUE,
    loading_scale = 2
  )
  
  expect_true(inherits(p1, "ggplot"))
  expect_true(inherits(p2, "ggplot"))
})


test_that("show methods report concise summaries for diversity and ordination objects", {
  object <- demo_global_patterns(n_variables = 80, n_samples = 6)
  alpha <- calculate_alpha_diversity(object, metric = "observed")
  ord <- run_ordination(object, method = "PCA", n_axes = 2)
  
  alpha_out <- capture.output(show(alpha))
  ord_out <- capture.output(show(ord))
  
  expect_true(any(grepl("microbiome_diversity", alpha_out, fixed = TRUE)))
  expect_true(any(grepl("Method: observed", alpha_out, fixed = TRUE)))
  expect_true(any(grepl("microbiome_ordination", ord_out, fixed = TRUE)))
  expect_true(any(grepl("Method: PCA", ord_out, fixed = TRUE)))
})


test_that("TreeSummarizedExperiment conversion preserves core microbiome data", {
  testthat::skip_if_not_installed("TreeSummarizedExperiment")
  
  object <- demo_global_patterns(n_variables = 60, n_samples = 6)
  object <- microbiomedataset::convert2microbiome_dataset(
    microbiomedataset::convert2phyloseq(object)
  )
  ref_seq <- Biostrings::DNAStringSet(rep("ACGT", nrow(object@variable_info)))
  object <- replace_ref_seq(object, ref_seq)
  
  tse <- convert2TreeSummarizedExperiment(object)
  roundtrip <- convert2microbiome_dataset(tse)
  
  expect_s4_class(tse, "TreeSummarizedExperiment")
  expect_identical(dim(roundtrip@expression_data), dim(object@expression_data))
  expect_identical(roundtrip@sample_info$sample_id, object@sample_info$sample_id)
  expect_identical(roundtrip@variable_info$variable_id, object@variable_info$variable_id)
  expect_identical(names(roundtrip@ref_seq), object@variable_info$variable_id)
})


test_that("import_biom builds a microbiome_dataset from a BIOM object", {
  testthat::skip_if_not_installed("biomformat")
  
  counts <- matrix(
    c(10, 0, 3, 8),
    nrow = 2,
    dimnames = list(c("OTU1", "OTU2"), c("Sample1", "Sample2"))
  )
  sample_info <- data.frame(
    sample_id = c("Sample1", "Sample2"),
    SampleType = c("A", "B"),
    class = "Subject"
  )
  variable_info <- data.frame(
    variable_id = c("OTU1", "OTU2"),
    Kingdom = "Bacteria",
    Phylum = c("Firmicutes", "Actinobacteria")
  )
  
  biom <- biomformat::make_biom(data = counts)
  object <- import_biom(
    biom,
    sample_info = sample_info,
    variable_info = variable_info
  )
  
  expect_s4_class(object, "microbiome_dataset")
  expect_identical(dim(object@expression_data), c(2L, 2L))
  expect_identical(object@sample_info$sample_id, sample_info$sample_id)
  expect_identical(object@variable_info$variable_id, variable_info$variable_id)
})


test_that("import_dada2 creates a microbiome_dataset from sequence tables", {
  seqtab <- matrix(
    c(10, 0, 3, 8),
    nrow = 2,
    dimnames = list(c("ASV1", "ASV2"), c("Sample1", "Sample2"))
  )
  taxonomy <- data.frame(
    Kingdom = "Bacteria",
    Phylum = c("Firmicutes", "Bacteroidota"),
    row.names = c("ASV1", "ASV2")
  )
  sample_info <- data.frame(
    sample_id = c("Sample1", "Sample2"),
    SampleType = c("Mock", "Soil"),
    class = "Subject"
  )
  
  object <- import_dada2(
    seqtab = seqtab,
    taxonomy_table = taxonomy,
    sample_info = sample_info,
    taxa_are_rows = TRUE
  )
  
  expect_s4_class(object, "microbiome_dataset")
  expect_identical(rownames(object@expression_data), c("ASV1", "ASV2"))
  expect_identical(colnames(object@expression_data), c("Sample1", "Sample2"))
  expect_true(all(c("Kingdom", "Phylum") %in% colnames(object@variable_info)))
})


test_that("optional interoperability wrappers fail with clear messages when packages are absent", {
  object <- demo_global_patterns(n_variables = 40, n_samples = 4)
  
  if (!requireNamespace("MicrobiotaProcess", quietly = TRUE)) {
    expect_error(convert2MPSE(object), "MicrobiotaProcess")
  }
  
  if (!requireNamespace("qiime2R", quietly = TRUE)) {
    expect_error(import_qiime2(matrix(1, nrow = 1, ncol = 1)), "qiime2R")
  }
})


test_that("tree link metadata is created and synchronized with tree verbs", {
  object <- demo_global_patterns(n_variables = 60, n_samples = 5)
  object <- microbiomedataset::convert2microbiome_dataset(
    microbiomedataset::convert2phyloseq(object)
  )
  otu_phylo <- ape::rtree(
    n = nrow(object@variable_info),
    tip.label = object@variable_info$variable_id
  )
  object <- replace_tree(
    object,
    tree = "otu_tree",
    value = tidytree::treedata(
      phylo = otu_phylo,
      data = tibble::tibble(node = seq_len(length(otu_phylo$tip.label) + otu_phylo$Nnode))
    )
  )
  
  expect_true(is.data.frame(extract_tree_link_data(object, tree = "taxa_tree")))
  expect_true(is.data.frame(extract_tree_link_data(object, tree = "otu_tree")))
  expect_true(all(
    extract_tree_link_data(object, tree = "otu_tree")$variable_id %in%
      object@variable_info$variable_id
  ))
  
  keep_tips <- head(object@variable_info$variable_id, 10)
  pruned <- prune_tree(object, tree = "otu_tree", tip_label = keep_tips)
  otu_link <- extract_tree_link_data(pruned, tree = "otu_tree")
  
  expect_identical(sort(otu_link$variable_id), sort(keep_tips))
  expect_identical(sort(otu_link$node_label), sort(keep_tips))
  expect_identical(check_microbiome_dataset_class(pruned), TRUE)
})


test_that("taxa tree link metadata records deepest available taxonomy rank", {
  object <- demo_global_patterns(n_variables = 40, n_samples = 5)
  object <- microbiomedataset::convert2microbiome_dataset(
    microbiomedataset::convert2phyloseq(object)
  )
  
  taxa_link <- extract_tree_link_data(object, tree = "taxa_tree")
  
  expect_true(is.data.frame(taxa_link))
  expect_true(all(c("variable_id", "node_label", "tree", "link_rank") %in% colnames(taxa_link)))
  expect_true(all(taxa_link$tree == "taxa_tree"))
  expect_true(all(taxa_link$variable_id %in% object@variable_info$variable_id))
})


test_that("update_microbiome_dataset upgrades old objects to the current schema", {
  object <- demo_global_patterns(n_variables = 40, n_samples = 5)
  upgraded <- update_microbiome_dataset(object)
  
  expect_true(methods::.hasSlot(upgraded, "otu_tree_link"))
  expect_true(methods::.hasSlot(upgraded, "taxa_tree_link"))
  expect_identical(check_microbiome_dataset_class(upgraded), TRUE)
})


test_that("check_tree_link and plot_tree_link expose link quality", {
  object <- demo_global_patterns(n_variables = 40, n_samples = 5)
  object <- microbiomedataset::convert2microbiome_dataset(
    microbiomedataset::convert2phyloseq(object)
  )
  
  summary_tbl <- check_tree_link(object, tree = "taxa_tree")
  plot_obj <- plot_tree_link(object, tree = "taxa_tree")
  
  expect_true(is.data.frame(summary_tbl))
  expect_true(all(c("tree", "has_tree", "has_link", "n_links") %in% colnames(summary_tbl)))
  expect_true(inherits(plot_obj, "ggplot"))
})


test_that("create_microbiome_metabolome_dataset builds aligned paired object", {
  microbiome_object <- demo_global_patterns(n_variables = 20, n_samples = 5)
  metabolome_object <- mock_metabolome_dataset(microbiome_object@sample_info, n_metabolites = 4)
  
  crossomics_object <- microbiomedataset::create_microbiome_metabolome_dataset(
    microbiome_data = microbiome_object,
    metabolome_data = metabolome_object
  )
  
  expect_s4_class(crossomics_object, "microbiome_metabolome_dataset")
  expect_equal(nrow(crossomics_object@sample_link), 5)
  expect_identical(
    crossomics_object@sample_link$microbiome_sample_id,
    crossomics_object@sample_link$metabolome_sample_id
  )
})


test_that("extract_crossomics_matrix returns matched matrices and metadata", {
  microbiome_object <- demo_global_patterns(n_variables = 30, n_samples = 6)
  metabolome_object <- mock_metabolome_dataset(microbiome_object@sample_info, n_metabolites = 5)
  
  crossomics_object <- microbiomedataset::create_microbiome_metabolome_dataset(
    microbiome_data = microbiome_object,
    metabolome_data = metabolome_object
  )
  
  extracted <- microbiomedataset::extract_crossomics_matrix(
    crossomics_object,
    microbiome_rank = "Genus",
    microbiome_transform = "relative"
  )
  
  expect_true(all(colnames(extracted$microbiome_matrix) == colnames(extracted$metabolome_matrix)))
  expect_equal(ncol(extracted$microbiome_matrix), nrow(extracted$sample_info))
  expect_true(all(c("microbiome_sample_id", "metabolome_sample_id", "pair_id") %in% colnames(extracted$sample_info)))
})


test_that("extract_crossomics_matrix works directly on microbiome and metabolome objects", {
  microbiome_object <- demo_global_patterns(n_variables = 30, n_samples = 6)
  metabolome_object <- mock_metabolome_dataset(microbiome_object@sample_info, n_metabolites = 5)
  
  extracted <- microbiomedataset::extract_crossomics_matrix(
    microbiome_data = microbiome_object,
    metabolome_data = metabolome_object,
    microbiome_rank = "Genus"
  )
  
  expect_equal(ncol(extracted$microbiome_matrix), ncol(extracted$metabolome_matrix))
  expect_true(all(colnames(extracted$microbiome_matrix) == colnames(extracted$metabolome_matrix)))
})


test_that("associate_microbe_metabolite returns association table with q values", {
  microbiome_object <- demo_global_patterns(n_variables = 30, n_samples = 5)
  metabolome_object <- mock_metabolome_dataset(microbiome_object@sample_info, n_metabolites = 4)
  
  crossomics_object <- microbiomedataset::create_microbiome_metabolome_dataset(
    microbiome_data = microbiome_object,
    metabolome_data = metabolome_object
  )
  
  association <- microbiomedataset::associate_microbe_metabolite(
    crossomics_object,
    microbiome_rank = "Genus"
  )
  
  expect_s4_class(association, "microbe_metabolite_association")
  expect_true(all(c("taxon_id", "metabolite_id", "estimate", "p_value", "q_value") %in% colnames(association@result)))
  expect_true(nrow(association@result) > 0)
})


test_that("associate_microbe_metabolite works directly on microbiome and metabolome objects", {
  microbiome_object <- demo_global_patterns(n_variables = 30, n_samples = 5)
  metabolome_object <- mock_metabolome_dataset(microbiome_object@sample_info, n_metabolites = 4)
  
  association <- microbiomedataset::associate_microbe_metabolite(
    microbiome_data = microbiome_object,
    metabolome_data = metabolome_object,
    microbiome_rank = "Genus"
  )
  
  expect_s4_class(association, "microbe_metabolite_association")
  expect_true(nrow(association@result) > 0)
})


test_that("standardize_metabolite_annotation and standardize_pathway_link normalize schemas", {
  annotation_table <- data.frame(
    variable_id = c("M1", "M2"),
    Compound = c("acetate", "lactate"),
    Pathway = c("pathway_a", "pathway_b"),
    stringsAsFactors = FALSE
  )
  pathway_link <- data.frame(
    taxon_id = "Genus_A",
    metabolite_id = "M1",
    pathway_id = "pathway_a",
    stringsAsFactors = FALSE
  )
  
  annotation_table <- microbiomedataset::standardize_metabolite_annotation(annotation_table)
  pathway_link <- microbiomedataset::standardize_pathway_link(pathway_link)
  
  expect_true(all(c("variable_id", "compound_name", "pathway_id", "annotation_level") %in% colnames(annotation_table)))
  expect_true(all(c("taxon_id", "metabolite_id", "pathway_id", "evidence_type") %in% colnames(pathway_link)))
})


test_that("standardize_taxon_pathway_link normalizes schema", {
  taxon_pathway_link <- data.frame(
    taxon_id = "Genus_A",
    pathway_id = "pathway_a",
    stringsAsFactors = FALSE
  )
  
  taxon_pathway_link <- microbiomedataset::standardize_taxon_pathway_link(taxon_pathway_link)
  
  expect_true(all(c("taxon_id", "pathway_id", "evidence_type") %in% colnames(taxon_pathway_link)))
})


test_that("infer_metabolic_link combines association and pathway metadata", {
  microbiome_object <- demo_global_patterns(n_variables = 30, n_samples = 5)
  metabolome_object <- mock_metabolome_dataset(microbiome_object@sample_info, n_metabolites = 4)
  genus_id <- unique(stats::na.omit(as.character(microbiome_object@variable_info$Genus)))[1]
  
  pathway_link <- data.frame(
    taxon_id = genus_id,
    metabolite_id = "M1",
    pathway_id = "pathway_a",
    evidence_type = "annotation_overlap",
    stringsAsFactors = FALSE
  )
  
  crossomics_object <- microbiomedataset::create_microbiome_metabolome_dataset(
    microbiome_data = microbiome_object,
    metabolome_data = metabolome_object,
    pathway_link = pathway_link
  )
  
  mechanism <- microbiomedataset::infer_metabolic_link(
    crossomics_object,
    microbiome_rank = "Genus",
    q_value_cutoff = 1
  )
  
  expect_s4_class(mechanism, "crossomics_mechanism")
  expect_true(all(c("taxon_id", "metabolite_id", "pathway_id", "mechanism_score", "compound_name", "annotation_level", "mechanism_tier") %in% colnames(mechanism@result)))
})


test_that("summarise_pathways aggregates pathway-level mechanism evidence", {
  microbiome_object <- demo_global_patterns(n_variables = 30, n_samples = 5)
  metabolome_object <- mock_metabolome_dataset(microbiome_object@sample_info, n_metabolites = 4)
  genus_id <- unique(stats::na.omit(as.character(microbiome_object@variable_info$Genus)))[1]
  
  pathway_link <- data.frame(
    taxon_id = genus_id,
    metabolite_id = "M1",
    pathway_id = "pathway_a",
    evidence_type = "annotation_overlap",
    stringsAsFactors = FALSE
  )
  taxon_pathway_link <- data.frame(
    taxon_id = genus_id,
    pathway_id = "pathway_a",
    pathway_name = "Pathway A",
    stringsAsFactors = FALSE
  )
  
  mechanism <- microbiomedataset::infer_metabolic_link(
    microbiome_data = microbiome_object,
    metabolome_data = metabolome_object,
    pathway_link = pathway_link,
    microbiome_rank = "Genus",
    q_value_cutoff = 1
  )
  summary_tbl <- microbiomedataset::summarise_pathways(
    mechanism_result = mechanism,
    taxon_pathway_link = taxon_pathway_link
  )
  
  expect_true(all(c("pathway_id", "n_taxa", "n_metabolites", "mean_mechanism_score") %in% colnames(summary_tbl)))
  expect_true(nrow(summary_tbl) >= 1)
})


test_that("integrate_microbe_metabolite returns object and plotting works when mixOmics is available", {
  microbiome_object <- demo_global_patterns(n_variables = 120, n_samples = 6)
  metabolome_object <- mock_metabolome_dataset(microbiome_object@sample_info, n_metabolites = 8)
  
  if (!requireNamespace("mixOmics", quietly = TRUE)) {
    expect_error(
      microbiomedataset::integrate_microbe_metabolite(
        microbiome_data = microbiome_object,
        metabolome_data = metabolome_object,
        method = "spls"
      ),
      "mixOmics"
    )
    return()
  }
  
  integration <- microbiomedataset::integrate_microbe_metabolite(
    microbiome_data = microbiome_object,
    metabolome_data = metabolome_object,
    method = "spls",
    microbiome_rank = NULL,
    ncomp = 2
  )
  
  extracted <- microbiomedataset::extract_crossomics_integration(integration)
  plot_object <- microbiomedataset::plot_crossomics_integration(
    integration,
    show_loading = TRUE,
    top_n = 3
  )
  heatmap_object <- microbiomedataset::plot_crossomics_loading_heatmap(
    integration,
    block = "microbiome",
    top_n = 5
  )
  
  expect_s4_class(integration, "crossomics_integration")
  expect_true(all(c("sample_coord", "microbiome_loading", "metabolome_loading") %in% names(extracted)))
  expect_s3_class(plot_object, "ggplot")
  expect_s3_class(heatmap_object, "ggplot")
})


test_that("plot_crossomics_network works for association results", {
  microbiome_object <- demo_global_patterns(n_variables = 30, n_samples = 5)
  metabolome_object <- mock_metabolome_dataset(microbiome_object@sample_info, n_metabolites = 4)
  
  association <- microbiomedataset::associate_microbe_metabolite(
    microbiome_data = microbiome_object,
    metabolome_data = metabolome_object,
    microbiome_rank = "Genus"
  )
  plot_object <- microbiomedataset::plot_crossomics_network(
    association,
    top_n = 10
  )
  
  expect_s3_class(plot_object, "ggplot")
})


test_that("core microbiome visualization helpers return ggplot objects", {
  object <- demo_global_patterns(n_variables = 120, n_samples = 6)
  composition_plot <- microbiomedataset::plot_composition(
    object = object,
    taxonomic_rank = "Phylum",
    top_n = 6,
    x = "SampleType"
  )
  abundance_plot <- microbiomedataset::plot_abundance(
    object = object,
    taxonomic_rank = "Phylum",
    x = "SampleType",
    top_n = 4
  )
  heatmap_plot <- microbiomedataset::plot_heatmap(
    object = object,
    taxonomic_rank = "Phylum",
    top_n = 8,
    sample_label = "SampleType"
  )
  prevalence_plot <- microbiomedataset::plot_prevalence(
    object = object,
    taxonomic_rank = "Phylum",
    top_n = 8
  )
  depth_plot <- microbiomedataset::plot_sample_depth(
    object = object,
    x = "SampleType"
  )
  rarefaction_plot <- microbiomedataset::plot_rarefaction(
    object = object,
    steps = 5,
    color_by = "SampleType"
  )
  
  expect_s3_class(composition_plot, "ggplot")
  expect_s3_class(abundance_plot, "ggplot")
  expect_s3_class(heatmap_plot, "ggplot")
  expect_s3_class(prevalence_plot, "ggplot")
  expect_s3_class(depth_plot, "ggplot")
  expect_s3_class(rarefaction_plot, "ggplot")
})


test_that("extended microbiome visualization helpers return plot objects", {
  object <- demo_global_patterns(n_variables = 120, n_samples = 8)
  tree_object <- align_tree(object, tree = "taxa_tree")
  richness_plot <- microbiomedataset::plot_richness(object, x = "SampleType")
  biplot <- microbiomedataset::plot_ordination_biplot(object, method = "PCA", color_by = "SampleType")
  scree_plot <- microbiomedataset::plot_scree(run_ordination(object, method = "PCA"))
  ordination_3d <- microbiomedataset::plot_ordination_3d(run_ordination(object, method = "PCoA", n_axes = 3), color_by = "SampleType")
  taxa_boxplot <- microbiomedataset::plot_taxa_boxplot(object, taxonomic_rank = "Phylum", x = "SampleType", top_n = 4)
  taxa_violin <- microbiomedataset::plot_taxa_violin(object, taxonomic_rank = "Phylum", x = "SampleType", top_n = 4)
  density_plot <- microbiomedataset::plot_abundance_density(object, taxonomic_rank = "Phylum", top_n = 4)
  heatmap_annotation <- microbiomedataset::plot_heatmap_annotation(object, taxonomic_rank = "Phylum", top_n = 6, sample_annotation = "SampleType")
  distance_heatmap <- microbiomedataset::plot_distance_heatmap(object, annotate_by = "SampleType")
  core_plot <- microbiomedataset::plot_core_taxa(object, taxonomic_rank = "Phylum", top_n = 6)
  prevalence_abundance_plot <- microbiomedataset::plot_prevalence_abundance(object, taxonomic_rank = "Phylum", top_n = 6)
  tree_abundance_plot <- microbiomedataset::plot_tree_abundance(tree_object, tree = "taxa_tree", taxonomic_rank = "Phylum")
  tree_heatmap <- microbiomedataset::plot_tree_heatmap(tree_object, tree = "taxa_tree", taxonomic_rank = "Phylum", top_n = 6)
  longitudinal_plot <- microbiomedataset::plot_longitudinal(
    demo_crossomics$microbiome_data,
    taxonomic_rank = "Genus",
    time_by = "age",
    id_by = "subject_id",
    top_n = 4
  )
  overlap_plot <- microbiomedataset::plot_taxa_overlap(object, group_by = "SampleType", taxonomic_rank = "Phylum")
  upset_plot <- microbiomedataset::plot_prevalence_upset(object, group_by = "SampleType", taxonomic_rank = "Phylum")
  sunburst_plot <- microbiomedataset::plot_taxonomy_sunburst(object, ranks = c("Kingdom", "Phylum", "Class"))
  interactive_tree <- microbiomedataset::plot_tree_interactive(tree_object, tree = "taxa_tree", taxonomic_rank = "Phylum")
  
  expect_s3_class(richness_plot, "ggplot")
  expect_s3_class(biplot, "ggplot")
  expect_s3_class(scree_plot, "ggplot")
  expect_s3_class(ordination_3d, "plotly")
  expect_s3_class(taxa_boxplot, "ggplot")
  expect_s3_class(taxa_violin, "ggplot")
  expect_s3_class(density_plot, "ggplot")
  expect_s4_class(heatmap_annotation, "Heatmap")
  expect_s4_class(distance_heatmap, "Heatmap")
  expect_s3_class(core_plot, "ggplot")
  expect_s3_class(prevalence_abundance_plot, "ggplot")
  expect_s3_class(tree_abundance_plot, "ggplot")
  expect_s4_class(tree_heatmap, "Heatmap")
  expect_s3_class(longitudinal_plot, "ggplot")
  expect_s3_class(overlap_plot, "ggplot")
  expect_s4_class(upset_plot, "Heatmap")
  expect_s3_class(sunburst_plot, "plotly")
  expect_s3_class(interactive_tree, "plotly")
})


test_that("demo_crossomics package data exposes paired microbiome and metabolome objects", {
  demo_data <- demo_crossomics_data()
  
  expect_true(all(c("microbiome_data", "metabolome_data", "sample_link") %in% names(demo_data)))
  expect_s4_class(demo_data$microbiome_data, "microbiome_dataset")
  expect_s4_class(demo_data$metabolome_data, "mass_dataset")
  expect_equal(ncol(demo_data$microbiome_data@expression_data), 40)
  expect_equal(ncol(demo_data$metabolome_data@expression_data), 40)
  expect_equal(nrow(demo_data$sample_link), 40)
})


test_that("demo_crossomics supports direct crossomics matrix extraction and association", {
  demo_data <- demo_crossomics_data()
  
  extracted <- microbiomedataset::extract_crossomics_matrix(
    microbiome_data = demo_data$microbiome_data,
    metabolome_data = demo_data$metabolome_data,
    sample_link = demo_data$sample_link,
    microbiome_rank = "Genus",
    microbiome_transform = "relative",
    metabolome_transform = "none"
  )
  association <- microbiomedataset::associate_microbe_metabolite(
    microbiome_data = demo_data$microbiome_data,
    metabolome_data = demo_data$metabolome_data,
    sample_link = demo_data$sample_link,
    microbiome_rank = "Genus",
    metabolome_transform = "none",
    method = "spearman"
  )
  
  expect_equal(ncol(extracted$microbiome_matrix), 40)
  expect_equal(ncol(extracted$metabolome_matrix), 40)
  expect_s4_class(association, "microbe_metabolite_association")
  expect_true(nrow(association@result) > 0)
})


test_that("correlation workflow builds filtered matrices and tidy tables", {
  demo_data <- demo_crossomics_data()
  
  prepared <- microbiomedataset::prepare_correlation_data(
    microbiome_data = demo_data$microbiome_data,
    metabolome_data = demo_data$metabolome_data,
    sample_link = demo_data$sample_link,
    microbiome_rank = "Genus",
    metabolome_transform = "none",
    min_taxa_prevalence = 3,
    min_metabolite_prevalence = 3
  )
  correlation <- microbiomedataset::calculate_correlation_spearman(
    microbiome_data = demo_data$microbiome_data,
    metabolome_data = demo_data$metabolome_data,
    sample_link = demo_data$sample_link,
    microbiome_rank = "Genus",
    metabolome_transform = "none"
  )
  filtered <- microbiomedataset::filter_correlation(
    correlation,
    min_abs_correlation = 0.2,
    max_q_value = 1
  )
  extracted <- microbiomedataset::extract_correlation_table(filtered)
  
  expect_true(is.matrix(prepared$microbiome_matrix))
  expect_true(is.matrix(prepared$metabolome_matrix))
  expect_s4_class(correlation, "microbe_metabolite_association")
  expect_true(all(c("taxon_id", "metabolite_id", "estimate", "q_value") %in% colnames(extracted)))
})


test_that("network workflow builds graph objects and summaries", {
  demo_data <- demo_crossomics_data()
  correlation <- microbiomedataset::calculate_correlation(
    microbiome_data = demo_data$microbiome_data,
    metabolome_data = demo_data$metabolome_data,
    sample_link = demo_data$sample_link,
    microbiome_rank = "Genus",
    metabolome_transform = "none",
    method = "spearman"
  )
  
  network <- microbiomedataset::build_correlation_network(
    correlation,
    min_abs_correlation = 0.2,
    max_q_value = 1,
    top_n = 20
  )
  tables <- microbiomedataset::extract_network_table(network)
  graph <- microbiomedataset::convert_network_graph(network)
  centrality <- microbiomedataset::calculate_network_centrality(network)
  modules <- microbiomedataset::detect_network_modules(network)
  module_summary <- microbiomedataset::summarise_network_modules(modules)
  plot_object <- microbiomedataset::plot_correlation_network(network)
  
  expect_s4_class(network, "microbe_metabolite_network")
  expect_true(all(c("nodes", "edges") %in% names(tables)))
  expect_s3_class(graph, "tbl_graph")
  expect_true(all(c("degree", "betweenness", "closeness") %in% colnames(centrality)))
  expect_true(all(c("module", "block") %in% colnames(modules)))
  expect_true(all(c("n_nodes", "n_microbiome", "n_metabolome") %in% colnames(module_summary)))
  expect_s3_class(plot_object, "ggplot")
})


test_that("analysis-result visualization helpers return ggplot objects", {
  demo_data <- demo_crossomics_data()
  correlation <- microbiomedataset::calculate_correlation(
    microbiome_data = demo_data$microbiome_data,
    metabolome_data = demo_data$metabolome_data,
    sample_link = demo_data$sample_link,
    microbiome_rank = "Genus",
    metabolome_transform = "none",
    method = "spearman"
  )
  association_none <- microbiomedataset::calculate_correlation(
    microbiome_data = demo_data$microbiome_data,
    metabolome_data = demo_data$metabolome_data,
    sample_link = demo_data$sample_link,
    microbiome_rank = "Genus",
    metabolome_transform = "none",
    method = "spearman"
  )
  network <- microbiomedataset::build_correlation_network(
    correlation,
    min_abs_correlation = 0.2,
    max_q_value = 1,
    top_n = 20
  )
  mechanism <- microbiomedataset::infer_metabolic_link(
    microbiome_data = demo_data$microbiome_data,
    metabolome_data = demo_data$metabolome_data,
    sample_link = demo_data$sample_link,
    pathway_link = microbiomedataset::standardize_pathway_link(
      data.frame(
        taxon_id = summarise_taxa(demo_data$microbiome_data, taxonomic_rank = "Genus")@variable_info$variable_id[1],
        metabolite_id = demo_data$metabolome_data@annotation_table$variable_id[1],
        pathway_id = "pathway_a",
        pathway_name = "Pathway A",
        stringsAsFactors = FALSE
      )
    ),
    association_result = association_none,
    microbiome_rank = "Genus",
    q_value_cutoff = 1
  )
  genus_object <- summarise_taxa(demo_global_patterns(n_variables = 120, n_samples = 6), taxonomic_rank = "Genus")
  da_table <- genus_object@variable_info[1:15, c("variable_id", "Kingdom", "Phylum", "Class", "Order", "Family", "Genus"), drop = FALSE]
  da_table$estimate <- seq(-2, 2, length.out = nrow(da_table))
  da_table$p_value <- seq(0.001, 0.1, length.out = nrow(da_table))
  da_table$q_value <- p.adjust(da_table$p_value, method = "BH")
  da_result <- microbiomedataset::create_differential_abundance_result(
    result = da_table,
    method = "external",
    taxonomic_rank = "Genus"
  )
  
  expect_s3_class(microbiomedataset::plot_correlation_heatmap(correlation, top_n_taxa = 8, top_n_metabolites = 8), "ggplot")
  expect_s3_class(microbiomedataset::plot_correlation_dotplot(correlation, top_n_taxa = 8, top_n_metabolites = 8), "ggplot")
  expect_s3_class(microbiomedataset::plot_network_modules(network), "ggplot")
  expect_s3_class(microbiomedataset::plot_network_centrality(network), "ggplot")
  expect_s3_class(microbiomedataset::plot_module_composition(network), "ggplot")
  expect_s3_class(microbiomedataset::plot_module_summary(network), "ggplot")
  expect_s4_class(microbiomedataset::build_mechanism_network(mechanism, top_n = 10), "microbe_metabolite_network")
  expect_s3_class(microbiomedataset::plot_mechanism_network(mechanism, top_n = 10), "ggplot")
  expect_s3_class(microbiomedataset::plot_mechanism_summary(mechanism, top_n = 10), "ggplot")
  enrichment_tbl <- microbiomedataset::calculate_pathway_enrichment(mechanism)
  expect_true(all(c("pathway_id", "n_observed", "q_value", "enrichment_ratio") %in% colnames(enrichment_tbl)))
  expect_s3_class(microbiomedataset::plot_pathway_enrichment(mechanism, top_n = 10), "ggplot")
  expect_s3_class(microbiomedataset::plot_pathway_summary(mechanism, top_n = 10), "ggplot")
  expect_s4_class(da_result, "differential_abundance_result")
  expect_s3_class(microbiomedataset::plot_differential_ma(da_result), "ggplot")
  expect_s4_class(microbiomedataset::plot_differential_heatmap(da_result, genus_object, top_n = 10), "Heatmap")
  expect_s3_class(microbiomedataset::plot_differential_boxplot(da_result, genus_object, top_n = 10), "ggplot")
  expect_s3_class(microbiomedataset::plot_differential_volcano(da_result), "ggplot")
  expect_s3_class(microbiomedataset::plot_differential_effect_size(da_result, top_n = 10), "ggplot")
  expect_s3_class(microbiomedataset::plot_differential_cladogram(da_result, top_n = 10), "ggplot")
})


test_that("longitudinal mixed-effect result plots return ggplot objects", {
  mixed_effect_result <- microbiomedataset::create_longitudinal_mixed_effect_result(
    result = data.frame(
      feature = paste0("Taxon_", 1:8),
      term = rep(c("age", "study_groupcontrol"), each = 4),
      estimate = seq(-0.4, 0.5, length.out = 8),
      lower_ci = seq(-0.6, 0.3, length.out = 8),
      upper_ci = seq(-0.2, 0.7, length.out = 8),
      p_value = seq(0.001, 0.1, length.out = 8),
      stringsAsFactors = FALSE
    ),
    method = "external",
    time_variable = "age",
    group_variable = "study_group"
  )
  
  expect_s4_class(mixed_effect_result, "longitudinal_mixed_effect_result")
  expect_s3_class(microbiomedataset::plot_longitudinal_mixed_effect(mixed_effect_result, top_n = 6), "ggplot")
  expect_s3_class(microbiomedataset::plot_longitudinal_effect_heatmap(mixed_effect_result, top_n = 6), "ggplot")
})


test_that("run_differential_abundance wraps ANCOMBC when available", {
  demo_object <- microbiomedataset::prune_taxa(
    demo_crossomics_data()$microbiome_data,
    variable_id = demo_crossomics_data()$microbiome_data@variable_info$variable_id[1:40]
  )
  
  if (!requireNamespace("ANCOMBC", quietly = TRUE)) {
    expect_error(
      microbiomedataset::run_differential_abundance(
        object = demo_object,
        formula = "SampleType",
        group = "SampleType",
        method = "ancombc"
      ),
      "ANCOMBC"
    )
    return()
  }
  
  result <- microbiomedataset::run_differential_abundance(
    object = demo_object,
    formula = "study_group",
    method = "ancombc"
  )
  
  expect_s4_class(result, "differential_abundance_result")
  expect_true(all(c("variable_id", "estimate", "p_value", "q_value", "coefficient") %in% colnames(result@result)))
  expect_equal(length(unique(result@result$coefficient)), 1)
})


test_that("sparcc correlation errors cleanly when dependency is unavailable", {
  demo_data <- demo_crossomics_data()
  
  if (requireNamespace("SpiecEasi", quietly = TRUE)) {
    expect_s4_class(
      microbiomedataset::calculate_correlation_sparcc(
        microbiome_data = demo_data$microbiome_data,
        metabolome_data = demo_data$metabolome_data,
        sample_link = demo_data$sample_link,
        microbiome_rank = "Genus",
        metabolome_transform = "none"
      ),
      "microbe_metabolite_association"
    )
  } else {
    expect_error(
      microbiomedataset::calculate_correlation_sparcc(
        microbiome_data = demo_data$microbiome_data,
        metabolome_data = demo_data$metabolome_data,
        sample_link = demo_data$sample_link,
        microbiome_rank = "Genus",
        metabolome_transform = "none"
      ),
      "SpiecEasi"
    )
  }
})
