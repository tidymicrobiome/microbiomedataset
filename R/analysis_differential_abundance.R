#' Run Differential Abundance Analysis
#'
#' Run a supported differential abundance method on a `microbiome_dataset` and
#' return a standardized [differential_abundance_result-class] object.
#'
#' @param object A `microbiome_dataset` object.
#' @param formula Model formula passed to the differential abundance method.
#' @param group Optional grouping variable used by the method.
#' @param method Differential abundance method. Currently supports `"ancombc"`.
#' @param taxonomic_rank Optional taxonomy rank used to aggregate the
#'   microbiome data before modeling.
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
                                       method = c("ancombc"),
                                       taxonomic_rank = NULL,
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
