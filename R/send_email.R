#' send_email
#'
#' Sends the recipients in config.yml an email.
#' They get their report. We get paid. Everyone wins.
#'
#' @param emails_to_send A table of emails to send.
#'
#' @return Not much. This is all about the side effects.
#'
#' @import magrittr
#' @export
send_email <- function(emails_to_send = NULL) {
    config <- config::get()
    stopifnot(exprs = {
        !is.null(emails_to_send)
    })
    smtp_send_possibly <- purrr::possibly(
        blastula::smtp_send,
        otherwise = NA,
        quiet = FALSE
    )
    greeting <- dplyr::if_else(
        lubridate::am(lubridate::now(tzone = "America/New_York")),
        "Good Morning",
        "Good Afternoon"
    )
    for (i in 1:nrow(emails_to_send)) {
        if (i > config$number_reports_to_send) break()
        if (emails_to_send$stratification[i] == "") {
            message("Sending (one) email.")
        } else {
            message("Sending email ", emails_to_send$stratification[i])
        }
        email_to_send <-
            blastula::render_email(input = "email.Rmd") %>%
            blastula::add_attachment(file = emails_to_send$report_name[i])
        email_sent <- FALSE
        try <- 1
        while (email_sent == FALSE & try <= 3) {
            email_sent <- smtp_send_possibly(
                email_to_send,
                to = addresses(emails_to_send$to[i]),
                from = Sys.getenv("smtp_user"),
                subject = config$report_name,
                cc = addresses(emails_to_send$cc[i]),
                bcc = addresses(emails_to_send$bcc[i]),
                credentials = blastula::creds_envvar(
                    user = Sys.getenv("smtp_user"),
                    pass_envvar = "smtp_pass",
                    provider = "gmail"
                )
            )
            if (is.null(email_sent)) {
                email_sent <- TRUE
            } else if (email_sent == FALSE) {
                message("Email failed to send on try ", try, ". Trying again.")
            } else {
                message("Email failed to send on try ", try, ". Trying again.")
            }
            try <- try + 1
        }
        if (try > 3) {
            msg <- paste0(
                "Unable to send ",
                emails_to_send$stratification[i],
                "email."
            )
            message(msg)
            if (config::is_active("rsconnect")) {
                httr::POST(
                    url = config$slack_webhook_url,
                    body = list(text = msg),
                    encode = "json"
                )
            }
        }
    }
    message("Hopefully, we sent all of the messages.")
} ## END send_email