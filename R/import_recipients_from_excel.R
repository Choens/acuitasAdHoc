#' import_recipients_from_excel
#'
#' Imports recipients for a report from Excel. This makes your life suck less.
#' This file must contain a single worksheet. This worksheet shall have four
#' columns:
#' 1. stratification: The name of the stratification. This could be, for
#'    example, a site name.
#' 2. to: One or more email addresses to send the report to. Email addresses
#'    should be separated by a comma. ALL stratifications MUST have an email
#'    address, even if it is to a dummy account.
#' 3. cc: One or more email addresses to cc. May be blank.
#' 4. bcc: One or more email addresses to bcc. May be blank.
#'
#' @param file The recipients xlsx file. Default value is 'recipients.xlsx'.
#' @param configuration Which named configuration. Values must be "default",
#' "prod", or "rsconnect".
#' @param preview If TRUE, the results are NOT written to config.yml. Instead,
#' they are previewed by printing. This is only useful interactively. Don't use
#' this in an automated script, except for amusement purposes.
#'
#' @return TRUE if successful, else FALSE.
#' @export
import_recipients_from_excel <- function(file = "recipients.xlsx", configuration = "default", preview = FALSE) {
    stopifnot(exprs = {
        file.exists(file)
        file.exists("config.yml")
        configuration %in% c("default", "prod", "rsconnect", "all")
    })
    cfg <- yaml::read_yaml("config.yml")
    xl <- openxlsx::read.xlsx(file)
    cn <- names(xl)
    stopifnot(exprs = {
        c("stratification", "to", "cc", "bcc") %in% names(xl)
    })
    recip_list <- list()
    for (i in 1:nrow(xl)) {
        recip_list[[i]] <- list(
            stratification = xl$stratification[i],
            to = paste0('"', tidyr::replace_na(xl$to[i], ""), '"'),
            cc = paste0('"', tidyr::replace_na(xl$cc[i], ""), '"'),
            bcc = paste0('"', tidyr::replace_na(xl$bcc[i], ""), '"')
        )
    }
    if (configuration == "default" | configuration == "all") {
        cfg$default$recipients <- recip_list
    }
    if (configuration == "prod" | configuration == "all") {
        cfg$prod$recipients <- recip_list
    }
    if (configuration == "rsconnect" | configuration == "all") {
        cfg$rsconnect$recipients <- recip_list
    }
    if (preview) {
        message(yaml::as.yaml(cfg, indent.mapping.sequence = TRUE))
    } else {
        yaml::write_yaml(cfg, indent.mapping.sequence = TRUE)
    }
    TRUE
}