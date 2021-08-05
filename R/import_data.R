#' import_data
#'
#' Opens a SQL query, runs it, and downloads the result-set.
#'
#' @param file The file where the query lives.
#' @param folder The folder where the query lives. If validation.sql is in this
#' folder it will be run BEFORE the sql file.
#'
#' @return A data frame with your data.
#'
#' @import magrittr
#' @export
import_data <- function(file = "query.sql", folder = "sql") {
    config <- config::get()
    qry_file <- file.path(folder, file)
    val_file <- file.path(folder, "validation.sql")
    stopifnot(exprs = {
        file.exists(qry_file)
    })
    connect_rate <- purrr::rate_delay(pause = 30, max_times = 10)
    dbConnectInsistent <- purrr::insistently(DBI::dbConnect, rate = connect_rate)
    dbGetQueryInsistent <- purrr::insistently(DBI::dbGetQuery, rate = connect_rate)
    ## ---- DB Connection ----
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

    ## ---- validation.qry ----
    if (file.exists(val_file)) {
        tryCatch(
            {
                qry <- readr::read_file(val_file)
                wait_time <- 3 ## 300
                res <- tibble::tibble(TestNM = c("Example Test"), TestValue = c(1))
                try <- 1
                while (nrow(res) > 0 & try < 4) {
                    message("Validating data.")
                    tmp <- dbGetQueryInsistent(con, qry) %>%
                        tibble::as_tibble() %>%
                        dplyr::filter("TestValue" == 0)
                    if (nrow(res) > 0) {
                        msg <- paste0(
                            paste(knitr::kable(res), collapse = "\n"),
                            paste("\n\nValidation query failed. Wait for", wait_time / 60, "minutes to try again.\n")
                        )
                        if (config::is_active("rsconnect")) {
                            httr::POST(
                                url = config$slack_webhook_url,
                                body = list(text = msg),
                                encode = "json"
                            )
                            message(msg)
                        } else {
                            message(msg)
                        }
                        Sys.sleep(wait_time)
                    }
                    try <- try + 1
                }
                rm(qry)
            },
            error = function(err) {
                fail_vocally(paste0("Unable to run/pass validation query. ", as.character(err)))
            }
        )
    } else {
        (warning("No validation query file. Good luck!"))
    }

    ## ---- query.sql ----
    tryCatch(
        {
            qry <- readr::read_file(qry_file)
            message("Downloading data.")
            res <- dbGetQueryInsistent(con, qry) %>% tibble::as_tibble()
        },
        error = function(err) {
            fail_vocally(paste0("Unable to run report query. ", as.character(err)))
        }
    )

    DBI::dbDisconnect(con)
    res
} ## END import_data