#' import_data
#'
#' Looks for "file" in "folder". If it exists, it attempts to run the query
#' AFTER running validation.sql if it exists. If validation.sql exists, all rows
#' must return 1 (TRUE) before the report query in "file" will be run. If
#' validation.sql fails, the function will stop for the number of seconds
#' defined in wait_time before trying to rerun validation.sql.
#'
#' @param file The file where the query lives.
#' @param folder The folder where the query lives. If validation.sql is in this
#' folder it will be run BEFORE the sql file.
#' @param validation_wait_time The number of seconds for import_data to wait if
#' validation.sql fails. This wait time is intended to give the EDW a chance to
#' finish, etc.
#'
#' @return A data frame with your data.
#'
#' @import magrittr
#' @export
import_data <- function(file = "query.sql", folder = "sql", validation_wait_time = 300) {
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
                res <- tibble::tibble(TestNM = c("Example Test"), TestValue = c(1))
                try <- 1
                while (try < 4) {
                    message("Validating data.")
                    res <- dbGetQueryInsistent(con, qry) %>%
                        tibble::as_tibble() %>%
                        dplyr::filter("TestValue" == 0)
                    if (nrow(res) > 0) {
                        msg <- paste0(
                            paste(knitr::kable(res), collapse = "\n"),
                            paste("\n\nValidation query failed. Wait for", validation_wait_time / 60, "minutes to try again.\n")
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
                        Sys.sleep(validation_wait_time)
                    } else {
                        break()
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