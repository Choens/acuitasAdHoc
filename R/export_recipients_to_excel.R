#' export_recipients_to_excel
#'
#' Creates an Excel file, called recipients by default, which can be used to
#' assign recipients to specific stratification reports. You can also share this
#' file with PI/Outcomes/Product to determine who should receive what.
#'
#' This file will contain a single worksheet. This worksheet will have four
#' columns:
#' 1. stratification: The name of the stratification. This could be, for
#'    example, a site name.
#' 2. to: One or more email addresses to send the report to. Email addresses
#'    should be separated by a comma. ALL stratifications MUST have an email
#'    address, even if it is to a dummy account.
#' 3. cc: One or more email addresses to cc. May be blank.
#' 4. bcc: One or more email addresses to bcc. May be blank.
#'
#' As exported, the only column with information will be the stratification
#' column.
#'
#' @param stratification A vector of things you want to stratify by. It can be
#' length of one if you don't really have any stratifications.
#' @param file The name of the exported excel file.
#' @param overwrite Overwrite an existing recipients.xlsx or not.
#' Default is FALSE.
#'
#' @return TRUE if the file is created, else FALSE.
#' @export
export_recipients_to_excel <- function(stratification, file = "recipients.xlsx", overwrite = FALSE) {
    stopifnot(exprs = {
        !missing(stratification)
        is.vector(stratification)
        length(stratification) > 0
    })
    r <- tibble::tibble(stratification, to = "", cc = "", bcc = "")
    if (file.exists("recipients.xlsx") & overwrite) {
        warning("Overwriting existing recipients file.")
        openxlsx::write.xlsx(r, file, overwrite = TRUE)
    } else if (file.exists("recipients.xlsx") & !overwrite) {
        stop("A recipients file of that name already exists.")
    } else {
        openxlsx::write.xlsx(r, file)
    }
    if (file.exists(file)) TRUE
}