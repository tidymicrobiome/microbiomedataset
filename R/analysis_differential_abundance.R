#' Run Differential Abundance Analysis
#'
#' Run a supported differential abundance method on a `microbiome_dataset` and
#' return a standardized [differential_abundance_result-class] object.
#'
#' @param object A `microbiome_dataset` object.
#' @param formula Model formula passed to the differential abundance method.
#' @param group Optional grouping variable used by the method.
#' @param method Differential abundance method. Supports `"ancombc"`,
#'   `"ancombc2"`, and `"maaslin2"`.
#' @param taxonomic_rank Optional taxonomy rank used to aggregate the
#'   microbiome data before modeling.
#' @param random_effects Optional random-effects formula used by methods that
#'   support mixed-effects models.
#' @param coefficient Optional coefficient name extracted from the fitted model.
#'   When omitted, the first non-intercept coefficient is used.
#' @param p_adj_method Multiple-testing correction method.
#' @param prv_cut Prevalence cutoff passed to `ANCOMBC::ancombc()`.
#' @param lib_cut Library-size cutoff passed to `ANCOMBC::ancombc()`.
#' @param struc_zero Whether to detect structural zeros.
#' @param neg_lb Whether to use the negative lower bound option.
#' @param tol Numerical tolerance passed to `ANCOMBC::ancombc()`.
#' @param max_iter Maximum iteration count passed to `ANCOMBC::ancombc()`.
#' @param conserve Whether to use the conservative variance estimate.
#' @param alpha Significance cutoff passed to `ANCOMBC::ancombc()`.
#' @param global Whether to run the global test.
#' @param n_cl Number of parallel workers passed to `ANCOMBC::ancombc()`.
#' @param verbose Whether to print method progress.
#' @param normalization Normalization method passed to `Maaslin2::Maaslin2()`.
#' @param transform Transformation passed to `Maaslin2::Maaslin2()`.
#' @param analysis_method Modeling backend passed to `Maaslin2::Maaslin2()`.
#' @param standardize Whether to standardize metadata in
#'   `Maaslin2::Maaslin2()`.
#'
#' @return A `differential_abundance_result` object.
#' @export
#' @examplesIf requireNamespace("ANCOMBC", quietly = TRUE)
#' data("demo_crossomics", package = "microbiomedataset")
#'
#' demo_object <- prune_taxa(
#'   demo_crossomics$microbiome_data,
#'   variable_id = demo_crossomics$microbiome_data@variable_info$variable_id[1:40]
#' )
#'
#' da_result <- run_differential_abundance(
#'   object = demo_object,
#'   formula = "study_group",
#'   method = "ancombc"
#' )
#' da_result
run_differential_abundance <- function(object,
                                       formula,
                                       group = NULL,
                                       method = c("ancombc", "ancombc2", "maaslin2"),
                                       taxonomic_rank = NULL,
                                       random_effects = NULL,
                                       coefficient = NULL,
                                       p_adj_method = "holm",
                                       prv_cut = 0.1,
                                       lib_cut = 0,
                                       struc_zero = FALSE,
                                       neg_lb = FALSE,
                                       tol = 1e-05,
                                       max_iter = 100,
                                       conserve = FALSE,
                                       alpha = 0.05,
                                       global = FALSE,
                                       n_cl = 1,
                                       verbose = FALSE,
                                       normalization = "TSS",
                                       transform = "LOG",
                                       analysis_method = "LM",
                                       standardize = FALSE) {
  if (!methods::is(object, "microbiome_dataset")) {
    stop("object must be a microbiome_dataset.")
  }
  if (missing(formula) || !nzchar(formula)) {
    stop("formula must be supplied.")
  }
  method <- match.arg(method)
  
  model_object <- object
  if (!is.null(taxonomic_rank)) {
    model_object <- summarise_taxa(model_object, taxonomic_rank = taxonomic_rank)
  }
  
  result <- switch(
    method,
    ancombc = run_differential_abundance_ancombc(
      object = model_object,
      formula = formula,
      group = group,
      coefficient = coefficient,
      p_adj_method = p_adj_method,
      prv_cut = prv_cut,
      lib_cut = lib_cut,
      struc_zero = struc_zero,
      neg_lb = neg_lb,
      tol = tol,
      max_iter = max_iter,
      conserve = conserve,
      alpha = alpha,
      global = global,
      n_cl = n_cl,
      verbose = verbose
    ),
    ancombc2 = run_differential_abundance_ancombc2(
      object = model_object,
      formula = formula,
      random_effects = random_effects,
      group = group,
      coefficient = coefficient,
      p_adj_method = p_adj_method,
      prv_cut = prv_cut,
      lib_cut = lib_cut,
      struc_zero = struc_zero,
      neg_lb = neg_lb,
      alpha = alpha,
      global = global,
      n_cl = n_cl,
      verbose = verbose
    ),
    maaslin2 = run_differential_abundance_maaslin2(
      object = model_object,
      formula = formula,
      random_effects = random_effects,
      coefficient = coefficient,
      normalization = normalization,
      transform = transform,
      analysis_method = analysis_method,
      standardize = standardize,
      max_significance = alpha
    )
  )
  
  create_differential_abundance_result(
    result = result,
    method = method,
    taxonomic_rank = if (is.null(taxonomic_rank)) "" else taxonomic_rank,
    group = unique(result$coefficient)[1]
  )
}


#' @keywords internal
run_differential_abundance_ancombc <- function(object,
                                               formula,
                                               group = NULL,
                                               coefficient = NULL,
                                               p_adj_method = "holm",
                                               prv_cut = 0.1,
                                               lib_cut = 0,
                                               struc_zero = FALSE,
                                               neg_lb = FALSE,
                                               tol = 1e-05,
                                               max_iter = 100,
                                               conserve = FALSE,
                                               alpha = 0.05,
                                               global = FALSE,
                                               n_cl = 1,
                                               verbose = FALSE) {
  if (!requireNamespace("ANCOMBC", quietly = TRUE)) {
    stop("Package 'ANCOMBC' is required for method = 'ancombc'.")
  }
  
  phyloseq_object <- convert2phyloseq(object)
  fit <- ANCOMBC::ancombc(
    data = phyloseq_object,
    formula = formula,
    p_adj_method = p_adj_method,
    prv_cut = prv_cut,
    lib_cut = lib_cut,
    group = group,
    struc_zero = struc_zero,
    neg_lb = neg_lb,
    tol = tol,
    max_iter = max_iter,
    conserve = conserve,
    alpha = alpha,
    global = global,
    n_cl = n_cl,
    verbose = verbose
  )
  
  coefficient <- resolve_ancombc_coefficient(fit, coefficient = coefficient)
  lfc_tbl <- fit$res$lfc
  p_tbl <- fit$res$p_val
  q_tbl <- fit$res$q_val
  diff_tbl <- fit$res$diff_abn
  
  result <- data.frame(
    variable_id = as.character(lfc_tbl$taxon),
    estimate = as.numeric(lfc_tbl[[coefficient]]),
    p_value = as.numeric(p_tbl[[coefficient]]),
    q_value = as.numeric(q_tbl[[coefficient]]),
    diff_abundant = as.logical(diff_tbl[[coefficient]]),
    coefficient = coefficient,
    stringsAsFactors = FALSE
  ) %>%
    dplyr::left_join(object@variable_info, by = "variable_id") %>%
    dplyr::mutate(
      direction = dplyr::case_when(
        is.na(.data$estimate) ~ NA_character_,
        .data$estimate > 0 ~ "positive",
        .data$estimate < 0 ~ "negative",
        TRUE ~ "neutral"
      )
    )
  
  result
}


#' @keywords internal
resolve_ancombc_coefficient <- function(fit,
                                        coefficient = NULL) {
  available <- setdiff(colnames(fit$res$lfc), c("taxon", "(Intercept)"))
  if (length(available) == 0) {
    stop("No non-intercept coefficients were returned by ANCOMBC.")
  }
  if (is.null(coefficient)) {
    coefficient <- available[1]
    if (length(available) > 1) {
      message(
        "Using coefficient '", coefficient,
        "'. Set `coefficient =` to choose a different contrast."
      )
    }
  }
  if (!coefficient %in% available) {
    stop(
      "coefficient must be one of: ",
      paste(available, collapse = ", "),
      "."
    )
  }
  coefficient
}


#' @keywords internal
run_differential_abundance_ancombc2 <- function(object,
                                                formula,
                                                random_effects = NULL,
                                                group = NULL,
                                                coefficient = NULL,
                                                p_adj_method = "holm",
                                                prv_cut = 0.1,
                                                lib_cut = 0,
                                                struc_zero = FALSE,
                                                neg_lb = FALSE,
                                                alpha = 0.05,
                                                global = FALSE,
                                                n_cl = 1,
                                                verbose = FALSE) {
  if (!requireNamespace("ANCOMBC", quietly = TRUE)) {
    stop("Package 'ANCOMBC' is required for method = 'ancombc2'.")
  }
  
  phyloseq_object <- convert2phyloseq(object)
  fit <- ANCOMBC::ancombc2(
    data = phyloseq_object,
    fix_formula = formula,
    rand_formula = random_effects,
    p_adj_method = p_adj_method,
    prv_cut = prv_cut,
    lib_cut = lib_cut,
    group = group,
    struc_zero = struc_zero,
    neg_lb = neg_lb,
    alpha = alpha,
    n_cl = n_cl,
    verbose = verbose,
    global = global,
    pairwise = FALSE,
    dunnet = FALSE,
    trend = FALSE
  )
  
  coefficient <- resolve_ancombc2_coefficient(fit, coefficient = coefficient)
  result <- data.frame(
    variable_id = as.character(fit$res$taxon),
    estimate = as.numeric(fit$res[[paste0("lfc_", coefficient)]]),
    se = as.numeric(fit$res[[paste0("se_", coefficient)]]),
    statistic = as.numeric(fit$res[[paste0("W_", coefficient)]]),
    p_value = as.numeric(fit$res[[paste0("p_", coefficient)]]),
    q_value = as.numeric(fit$res[[paste0("q_", coefficient)]]),
    diff_abundant = as.logical(fit$res[[paste0("diff_", coefficient)]]),
    passed_ss = as.logical(fit$res[[paste0("passed_ss_", coefficient)]]),
    diff_robust = as.logical(fit$res[[paste0("diff_robust_", coefficient)]]),
    coefficient = coefficient,
    stringsAsFactors = FALSE
  ) %>%
    dplyr::left_join(object@variable_info, by = "variable_id") %>%
    dplyr::mutate(
      direction = dplyr::case_when(
        is.na(.data$estimate) ~ NA_character_,
        .data$estimate > 0 ~ "positive",
        .data$estimate < 0 ~ "negative",
        TRUE ~ "neutral"
      )
    )
  
  result
}


#' @keywords internal
resolve_ancombc2_coefficient <- function(fit,
                                         coefficient = NULL) {
  available <- grep("^lfc_", names(fit$res), value = TRUE)
  available <- sub("^lfc_", "", available)
  available <- setdiff(available, "(Intercept)")
  if (length(available) == 0) {
    stop("No non-intercept coefficients were returned by ANCOMBC2.")
  }
  if (is.null(coefficient)) {
    coefficient <- available[1]
    if (length(available) > 1) {
      message(
        "Using coefficient '", coefficient,
        "'. Set `coefficient =` to choose a different contrast."
      )
    }
  }
  if (!coefficient %in% available) {
    stop(
      "coefficient must be one of: ",
      paste(available, collapse = ", "),
      "."
    )
  }
  coefficient
}


#' @keywords internal
run_differential_abundance_maaslin2 <- function(object,
                                                formula,
                                                random_effects = NULL,
                                                coefficient = NULL,
                                                normalization = "TSS",
                                                transform = "LOG",
                                                analysis_method = "LM",
                                                standardize = FALSE,
                                                max_significance = 0.25) {
  if (!requireNamespace("Maaslin2", quietly = TRUE)) {
    stop("Package 'Maaslin2' is required for method = 'maaslin2'.")
  }
  
  fixed_effects <- parse_model_terms(formula)
  random_effects <- parse_model_terms(random_effects)
  if (length(fixed_effects) == 0) {
    stop("formula must define at least one fixed effect for Maaslin2.")
  }
  
  feature_table <- t(as.matrix(object@expression_data))
  metadata_table <- as.data.frame(object@sample_info, check.names = FALSE)
  rownames(metadata_table) <- metadata_table$sample_id
  metadata_table <- metadata_table[rownames(feature_table), , drop = FALSE]
  output_dir <- tempfile(pattern = "maaslin2_")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  fit <- Maaslin2::Maaslin2(
    input_data = feature_table,
    input_metadata = metadata_table,
    output = output_dir,
    fixed_effects = fixed_effects,
    random_effects = random_effects,
    normalization = normalization,
    transform = transform,
    analysis_method = analysis_method,
    standardize = standardize,
    max_significance = max_significance
  )
  
  result_table <- if (!is.null(fit$results)) fit$results else fit
  result_table <- as.data.frame(result_table, check.names = FALSE)
  has_stderr <- "stderr" %in% colnames(result_table)
  has_n <- "N" %in% colnames(result_table)
  coefficient <- resolve_maaslin2_coefficient(result_table, coefficient = coefficient)
  result <- result_table %>%
    dplyr::filter(.data$metadata == coefficient) %>%
    dplyr::transmute(
      variable_id = as.character(.data$feature),
      estimate = as.numeric(.data$coef),
      se = if (has_stderr) as.numeric(.data$stderr) else NA_real_,
      statistic = if (has_n) as.numeric(.data$N) else NA_real_,
      p_value = as.numeric(.data$pval),
      q_value = as.numeric(.data$qval),
      coefficient = .data$metadata
    ) %>%
    dplyr::left_join(object@variable_info, by = "variable_id") %>%
    dplyr::mutate(
      diff_abundant = !is.na(.data$q_value) & .data$q_value <= max_significance,
      direction = dplyr::case_when(
        is.na(.data$estimate) ~ NA_character_,
        .data$estimate > 0 ~ "positive",
        .data$estimate < 0 ~ "negative",
        TRUE ~ "neutral"
      )
    )
  
  result
}


#' @keywords internal
parse_model_terms <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(character())
  }
  x <- paste(x, collapse = "+")
  x <- gsub("~", "", x, fixed = TRUE)
  x <- unlist(strsplit(x, "\\+"))
  x <- trimws(x)
  x <- x[nzchar(x)]
  unique(x)
}


#' @keywords internal
resolve_maaslin2_coefficient <- function(result_table,
                                         coefficient = NULL) {
  if (!all(c("metadata", "feature", "coef", "pval", "qval") %in% colnames(result_table))) {
    stop("Maaslin2 output does not contain the expected result columns.")
  }
  available <- unique(as.character(result_table$metadata))
  available <- available[!is.na(available) & available != "(Intercept)"]
  if (length(available) == 0) {
    stop("No non-intercept coefficients were returned by Maaslin2.")
  }
  if (is.null(coefficient)) {
    coefficient <- available[1]
    if (length(available) > 1) {
      message(
        "Using coefficient '", coefficient,
        "'. Set `coefficient =` to choose a different contrast."
      )
    }
  }
  if (!coefficient %in% available) {
    stop(
      "coefficient must be one of: ",
      paste(available, collapse = ", "),
      "."
    )
  }
  coefficient
}
