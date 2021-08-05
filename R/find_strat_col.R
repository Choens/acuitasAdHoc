#' find_strat_col
#'
#' From a data frame/tibble, find the column labeled "Stratification".
#' This is the column we will use to stratify the reports by.
#' If config$has_stratification is set to:
#' -  'strict', this function will return an error on any query that
#'    doesn't have a stratification column.
#' - 'relaxed', it will only return a warning if it doesn't find a
#'   Stratification column and return NA.
#' - 'no', it will return NA.
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
        if (config$has_stratification == "strict") {
            stop("There is no 'Stratification' column in 'data'.")
        } else if (config$has_stratification == "relaxed") {
            warning("There is no 'Stratification' column in 'data'.")
            strat_col <- NA
        } else {
            strat_col <- NA
        }
    } else {
        strat_col <- which(stringr::str_detect(col_name, names(data)))
        if (length(strat_col) > 1) {
            stop("Cannot have more than one 'Stratification' column.")
        }
    }
    strat_col
}