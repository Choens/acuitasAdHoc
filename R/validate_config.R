#' validate_config
#'
#' Validates that the config.yml is good.
#'
#' @param file The config.yml file you want to validate. You can specify any
#' file, but it defaults to ./config.yml.
validate_config <- function(file = "./config.yml") {
    cfg <- yaml::read_yaml(file)
    purrr::map(cfg, validate_recipients)
}