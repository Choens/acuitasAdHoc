#' validate_recipients
#'
#' Validates that at all sites have at least one recipient. This only checks the
#' CURRENT active config. To test the rest of the YAML file, use validate_config.
#'
#' @param recipients A list of recipients. This config list must
#' contain 'recipients'.
#' @return Nothing. If the validation fails, it will stop your script.
#' @export
validate_recipients <- function(recipients) {
    stopifnot(exprs = {
        !missing(recipients)
        length(recipients) > 0
        is.list(recipients)
    })
    is_blank <- logical(length = length(recipients))
    for (i in seq_along(recipients)) {
        test_val <- recipients[[i]]$to
        is_blank[i] <- max(c(test_val == "", is.null(test_val)))
    }
    if (max(is_blank)) {
        stop("At least one stratification in your config lacks a recipient.")
    }
    TRUE
}