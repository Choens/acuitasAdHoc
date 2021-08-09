#' find_strat_col
#'
#' From a data frame/tibble, find the column labeled "Stratification".
#' This is the column we will use to stratify the reports by.
#' If there isn't a stratification column, find_strat_col returns NA.
#'
#' @param data The data frame to search through.
#' @param col_name The column name value of the stratification column. By
#' default, this is 'Stratification'. It can, however, be anything else you
#' want it to be.
#'
#' @return This function will return either a number or NA if it cannot locate a
#' stratification column.
#'
#' @export
find_strat_col <- function(data = NULL, col_name = "Stratification") {
    config <- config::get()
    if (is.null(data)) {
        return(NULL)
    }

    if (!col_name %in% names(data)) {
        warning("There is no 'Stratification' column in 'data'.")
        strat_col <- NA
    } else {
        strat_col <- which(stringr::str_detect(col_name, names(data)))
        if (length(strat_col) > 1) {
            stop("Cannot have more than one 'Stratification' column.")
        }
    }
    strat_col
}