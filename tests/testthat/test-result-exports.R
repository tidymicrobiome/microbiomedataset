test_that("association and network results expose standard extractors", {
  data("demo_crossomics", package = "microbiomedataset")
  
  correlation <- calculate_correlation(
    microbiome_data = demo_crossomics$microbiome_data,
    metabolome_data = demo_crossomics$metabolome_data,
    sample_link = demo_crossomics$sample_link,
    microbiome_rank = "Genus",
    metabolome_transform = "none"
  )
  
  expect_s3_class(as_tibble_association(correlation), "tbl_df")
  expect_true(all(c("taxon_id", "metabolite_id", "estimate") %in% colnames(
    extract_association_result(correlation)
  )))
  
  network <- build_correlation_network(
    correlation,
    min_abs_correlation = 0.2,
    max_q_value = 1,
    top_n = 20
  )
  
  network_tbl <- extract_network_result(network, what = "both")
  expect_true(all(c("nodes", "edges") %in% names(network_tbl)))
  expect_s3_class(as_tibble_network(network, what = "nodes"), "tbl_df")
  expect_s3_class(as_tibble_network(network, what = "edges"), "tbl_df")
})


test_that("mechanism and reaction summaries expose standardized outputs", {
  data("demo_crossomics", package = "microbiomedataset")
  
  genus_object <- summarise_taxa(
    demo_crossomics$microbiome_data,
    taxonomic_rank = "Genus"
  )
  pathway_link <- standardize_pathway_link(
    data.frame(
      taxon_id = genus_object@variable_info$variable_id[1:2],
      metabolite_id = demo_crossomics$metabolome_data@annotation_table$variable_id[1:2],
      pathway_id = c("pathway_a", "pathway_b"),
      pathway_name = c("Pathway A", "Pathway B"),
      reaction_id = c("reaction_a", "reaction_b"),
      reaction_name = c("Reaction A", "Reaction B"),
      stringsAsFactors = FALSE
    )
  )
  association <- calculate_correlation(
    microbiome_data = demo_crossomics$microbiome_data,
    metabolome_data = demo_crossomics$metabolome_data,
    sample_link = demo_crossomics$sample_link,
    microbiome_rank = "Genus",
    metabolome_transform = "none"
  )
  mechanism <- suppressWarnings(infer_metabolic_link(
    microbiome_data = demo_crossomics$microbiome_data,
    metabolome_data = demo_crossomics$metabolome_data,
    sample_link = demo_crossomics$sample_link,
    pathway_link = pathway_link,
    association_result = association,
    microbiome_rank = "Genus",
    q_value_cutoff = 1
  )
  )
  
  expect_s3_class(as_tibble_mechanism(mechanism), "tbl_df")
  expect_true(all(c("reaction_id", "reaction_name") %in% colnames(
    extract_mechanism_result(mechanism)
  )))
  
  reaction_summary <- summarise_reactions(mechanism)
  expect_true(all(c("reaction_id", "n_taxa", "n_metabolites") %in% colnames(
    reaction_summary
  )))
  
  standardized_reaction_link <- standardize_reaction_link(
    data.frame(
      taxon_id = "Genus_A",
      metabolite_id = "M1",
      reaction_id = "R1",
      stringsAsFactors = FALSE
    )
  )
  expect_true(all(c("reaction_name", "pathway_id") %in% colnames(
    standardized_reaction_link
  )))
  
  standardized_taxon_reaction <- standardize_taxon_reaction_link(
    data.frame(
      taxon_id = "Genus_A",
      reaction_id = "R1",
      stringsAsFactors = FALSE
    )
  )
  expect_true(all(c("reaction_name", "pathway_id") %in% colnames(
    standardized_taxon_reaction
  )))
})


test_that("integration and longitudinal results expose tibble helpers", {
  data("demo_crossomics", package = "microbiomedataset")
  
  if (requireNamespace("mixOmics", quietly = TRUE)) {
    integration <- suppressWarnings(integrate_microbe_metabolite(
      microbiome_data = demo_crossomics$microbiome_data,
      metabolome_data = demo_crossomics$metabolome_data,
      sample_link = demo_crossomics$sample_link,
      method = "spls"
    ))
    integration_tbl <- as_tibble_crossomics_integration(
      integration,
      block = "sample_coord"
    )
    expect_s3_class(integration_tbl, "tbl_df")
  }
  
  longitudinal_result <- create_longitudinal_mixed_effect_result(
    result = data.frame(
      feature = paste0("Taxon", 1:3),
      term = "time",
      estimate = c(0.3, -0.1, 0.2),
      stringsAsFactors = FALSE
    ),
    time_variable = "time"
  )
  expect_s3_class(as_tibble_longitudinal_mixed_effect(longitudinal_result), "tbl_df")
  expect_true(all(c("feature", "term", "estimate") %in% colnames(
    extract_longitudinal_mixed_effect_result(longitudinal_result)
  )))
})


test_that("differential abundance wrappers return standardized results", {
  data("demo_crossomics", package = "microbiomedataset")
  microbiome_object <- prune_taxa(
    demo_crossomics$microbiome_data,
    variable_id = demo_crossomics$microbiome_data@variable_info$variable_id[1:30]
  )
  
  testthat::skip_if_not_installed("ANCOMBC")
  ancombc2_result <- suppressWarnings(run_differential_abundance(
    object = microbiome_object,
    formula = "study_group",
    method = "ancombc2"
  ))
  expect_s4_class(ancombc2_result, "differential_abundance_result")
  expect_s3_class(as_tibble_differential_abundance(ancombc2_result), "tbl_df")
  expect_true(all(c("variable_id", "estimate", "q_value") %in% colnames(
    extract_differential_abundance_result(ancombc2_result)
  )))
})


test_that("maaslin2 wrapper fails clearly when dependency is missing", {
  data("demo_crossomics", package = "microbiomedataset")
  microbiome_object <- prune_taxa(
    demo_crossomics$microbiome_data,
    variable_id = demo_crossomics$microbiome_data@variable_info$variable_id[1:20]
  )
  
  if (!requireNamespace("Maaslin2", quietly = TRUE)) {
    expect_error(
      run_differential_abundance(
        object = microbiome_object,
        formula = "study_group",
        method = "maaslin2"
      ),
      "Package 'Maaslin2' is required"
    )
  }
})
