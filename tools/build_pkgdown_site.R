#!/usr/bin/env Rscript

warning_pattern <- "\\[WARNING\\] Deprecated: --no-highlight\\. Use --syntax-highlighting=none instead\\."

stdout_file <- tempfile(fileext = ".stdout")
stderr_file <- tempfile(fileext = ".stderr")

status <- system2(
  command = file.path(R.home("bin"), "Rscript"),
  args = c("-e", shQuote("pkgdown::build_site()")),
  stdout = stdout_file,
  stderr = stderr_file
)

emit_filtered <- function(path) {
  if (!file.exists(path)) {
    return(invisible(NULL))
  }
  lines <- readLines(path, warn = FALSE)
  lines <- lines[!grepl(warning_pattern, lines)]
  if (length(lines) > 0) {
    cat(lines, sep = "\n")
    cat("\n")
  }
  invisible(NULL)
}

emit_filtered(stdout_file)
emit_filtered(stderr_file)

quit(save = "no", status = status)
