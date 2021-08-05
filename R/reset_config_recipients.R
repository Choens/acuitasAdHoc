#' reset_config_recipients
#'
#' Resets all to, cc, bcc entries in config.yml. The values of the
#' stratifications is provided by the stratification parameter.
#'
#' @param stratification A vector of things you would like to stratify by. For
#' example, a vector of site names. But it can be anything. If you only want one
#' report, stratification can be equal to ""
#' @param configuration Which named configuration from config.yml do you want to
#' use? Must be "default", "prod", or "rsconnect". The default is . . . default.
#' @param preview Do you want to just "see" what it would look like? This is for
#' interactive/testing purposes. Don't use this in a script. Seriously. It won't
#' do anything useful. The default is zero.
#'
#' @return Returns TRUE if successful. Else it probably just dies, but it might
#' return FALSE.
#' @export
reset_config_recipients <- function(stratification, configuration = "default", preview = FALSE) {
    stopifnot(exprs = {
        !missing(stratification)
        is.vector(stratification)
        length(stratification) > 0
        file.exists("config.yml")
        configuration %in% c("default", "prod", "rsconnect", "all")
    })
    cfg <- yaml::read_yaml("config.yml")
    recip_list <- list()
    for (i in seq_along(stratification)) {
        recip_list[[i]] <- list(
            stratification = stratification[i],
            to = "",
            cc = "",
            bcc = ""
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
        yaml::write_yaml(cfg, "config.yml", indent.mapping.sequence = TRUE)
    }
}