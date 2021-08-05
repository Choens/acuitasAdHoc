#' import_data
#'
#' Opens a SQL query, runs it, and downloads the result-set.
#'
#' @param file The file where the query lives.
#'
#' @return A data frame with your data.
#'
#' @import magrittr
#' @export
import_data <- function(file = "query.sql") {
    config <- config::get()
    connect_rate <- purrr::rate_delay(pause = 30, max_times = 10)
    dbConnectInsistent <- purrr::insistently(DBI::dbConnect, rate = connect_rate)
    tryCatch(
        {
            con <- dbConnectInsistent(
                odbc::odbc(),
                dsn = config$dsn_name,
                uid = Sys.getenv("edw_user"),
                pwd = Sys.getenv("edw_pass")
            )
        },
        error = function(err) {
            fail_vocally(paste0("Unable to connect to database. ", as.character(err)))
        }
    )
    tryCatch(
        {
            qry <- readr::read_file(file)
            message("Downloading data.")
            res <- DBI::dbGetQuery(con, qry) %>% tibble::as_tibble()
        },
        error = function(err) {
            fail_vocally(paste0("Unable to run query. ", as.character(err)))
        }
    )
    DBI::dbDisconnect(con)
    res
} ## END import_data