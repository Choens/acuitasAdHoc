#' validate_recipients
#'
#' Validates that at all sites have at least one recipient. This only checks the
#' CURRENT active config. To test the rest of the YAML file, use validate_config.
#'
#' @param config A config object from config::get(). This config list must
#' contain 'recipients'.
#' @return Nothing. If the validation fails, it will stop your script.
#' @export
validate_recipients <- function(config) {
    stopifnot(exprs = {
        !missing(config)
        "recipients" %in% names(config)
        length(config$recipients) > 0
        is.list(config$recipients)
    })
    is_blank <- logical(length = length(config$recipients))
    for (i in seq_along(config$recipients)) {
        test_val <- config$recipients[[i]]$to
        is_blank[i] <- max(c(test_val == "", is.null(test_val)))
    }
    if (max(is_blank)) {
        stop("At least one stratification in your config lacks a recipient.")
    }
    TRUE
}