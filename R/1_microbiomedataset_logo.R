#' Display Logo and Version Information for microbiomedataset
#'
#' This function prints the logo of the microbiomedataset package along with
#' its current version and update date. It also provides a hint for obtaining
#' more information about the package.
#'
#' @details The function displays the logo using ASCII characters and prints
#' messages with version information and a suggestion to search for more
#' information about the `tidymicrobiome microbiomedataset`. The messages
#' and logo are printed in green using the `crayon` package.
#'
#' @export
#' @examples
#' microbiomedataset_logo()
#'
#' @note This function is part of the tidymicrobiome package and is intended
#' to enhance user experience by providing quick access to version information.
#'
#' @docType methods
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @importFrom stringr str_replace str_split str_replace_all str_trim
#' @importFrom ComplexHeatmap Heatmap columnAnnotation anno_barplot
#' @importFrom grid gpar
#' @importFrom ggplotify as.ggplot
#' @importFrom dplyr filter mutate select everything left_join bind_rows arrange
#' @importFrom dplyr desc distinct bind_cols bind_rows pull
#' @importFrom plyr dlply .
#' @importFrom rstudioapi isAvailable hasFun getThemeInfo
#' @importFrom utils packageDescription write.csv
#' @importFrom cli rule symbol
#' @importFrom crayon green blue col_align red black white style make_style num_colors
#' @importFrom plotly ggplotly
#' @importFrom pbapply pblapply
#' @importFrom openxlsx write.xlsx
#' @importFrom purrr map map2
#' @importFrom readr write_csv read_csv
#' @importFrom methods slot slot<-
#' @import ggplot2
#' @importFrom methods .hasSlot new is
#' @importFrom stats p.adjust rgamma sd median time setNames
#' @importFrom utils data str head tail packageVersion write.table
#' @importFrom magrittr %>%
#' @importFrom ggsci pal_lancet
#' @importFrom masstools read_mgf read_mzxml ms2_plot
#' @importFrom rlang warn quo_is_null abort seq2 syms
#' @importFrom tibble add_column as_tibble
#' @importFrom ggraph ggraph
#' @importFrom massdataset check_column_name extract_expression_data extract_sample_info
#' @importFrom massdataset extract_variable_info
#' @importFrom tidytree as.phylo as.treedata


microbiomedataset_logo <-
  function() {
    message(crayon::green("Thank you for using microbiomedataset!"))
    message(crayon::green("Version ",
                          microbiomedataset_version,
                          " (",
                          update_date,
                          ')'))
    message(crayon::green(
      "More information: search 'tidymicrobiome microbiomedataset'."
    ))
    cat(crayon::green(
      c(
        "            _                _     _                      _____        _                 _   ",
        "           (_)              | |   (_)                    |  __ \\      | |               | |  ",
        "  _ __ ___  _  ___ _ __ ___ | |__  _  ___  _ __ ___   ___| |  | | __ _| |_ __ _ ___  ___| |_ ",
        " | '_ ` _ \\| |/ __| '__/ _ \\| '_ \\| |/ _ \\| '_ ` _ \\ / _ \\ |  | |/ _` | __/ _` / __|/ _ \\ __|",
        " | | | | | | | (__| | | (_) | |_) | | (_) | | | | | |  __/ |__| | (_| | || (_| \\__ \\  __/ |_ ",
        " |_| |_| |_|_|\\___|_|  \\___/|_.__/|_|\\___/|_| |_| |_|\\___|_____/ \\__,_|\\__\\__,_|___/\\___|\\__|",
        "                                                                                             ",
        "                                                                                             "
      )
      
    ), sep = "\n")
  }

microbiomedataset_version <-
  as.character(utils::packageVersion(pkg = "microbiomedataset"))

update_date <- as.character(Sys.time())

#' Retrieve the Version of the microbiomedataset Package
#'
#' This function returns the current version of the microbiomedataset package.
#'
#' @details `get_microbiomedataset_version` retrieves the version number of the
#' microbiomedataset package using the `packageVersion` function from the utils
#' package. It returns the version as a character string.
#'
#' @return A character string representing the version of the
#' microbiomedataset package.
#'
#' @examples
#' get_microbiomedataset_version()
#'
#' @export
#' @note This function is especially useful for logging, debugging, or 
#' providing user feedback regarding the version of the microbiomedataset package.
#'
#' @docType methods
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}

get_microbiomedataset_version = function() {
  return(as.character(utils::packageVersion(pkg = "microbiomedataset")))
}

# library(cowsay)
# # https://onlineasciitools.com/convert-text-to-ascii-art
# # writeLines(capture.output(say("Hello"), type = "message"), con = "ascii_art.txt")
# art <- readLines("logo.txt")
# dput(art)
# microbiomedataset_logo <-
#   c("                          _____        _                 _   ",
#     "                         |  __ \\      | |               | |  ",
#     "  _ __ ___   __ _ ___ ___| |  | | __ _| |_ __ _ ___  ___| |_ ",
#     " | '_ ` _ \\ / _` / __/ __| |  | |/ _` | __/ _` / __|/ _ \\ __|",
#     " | | | | | | (_| \\__ \\__ \\ |__| | (_| | || (_| \\__ \\  __/ |_ ",
#     " |_| |_| |_|\\__,_|___/___/_____/ \\__,_|\\__\\__,_|___/\\___|\\__|",
#     "                                                             ",
#     "                                                             "
#   )
# cat(microbiomedataset_logo, sep = "\n")
