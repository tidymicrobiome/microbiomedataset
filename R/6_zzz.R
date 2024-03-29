.onAttach <- function(libname, pkgname) {
  #  needed <- core[!is_attached(core)]
  #  if (length(needed) == 0)
  #    return()
  #
  #  crayon::num_colors(TRUE)
  # microbiomedataset_attach()
  
  # if (!"package:conflicted" %in% search()) {
  #   x <-microbiomedataset_conflicts()
  #   msg(massdataset_conflict_message(x), startup = TRUE)
  # }
  msg(paste0(
    "microbiomedataset (v",
    microbiomedataset_version,
    ", ",
    update_date,
    ')'
  ))
}

is_attached <- function(x) {
  paste0("package:", x) %in% search()
}
