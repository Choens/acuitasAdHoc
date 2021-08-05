#' fail_vocally
#'
#' Pushes errors to the best possible destination. If we are running the
#' rsconnect config, it tries to publish errors to Slack. Otherwise, it pushes
#' out messages.
#'
#'
#' @param err The error you wish to handle.
#'
#' @return NOTHING! This thing either creates a message and/or pushes the error
#' to Slack.
#'
#' @export
fail_vocally <- function(err = NULL) {
    stopifnot(exprs = {
        !is.null(err)
    })
    config <- config::get()
    if (config::is_active("rsconnect")) {
        httr::POST(
            url = config$slack_webhook_url,
            body = list(text = err),
            encode = "json"
        )
        stop(err)
    } else {
        stop(err)
    }
}