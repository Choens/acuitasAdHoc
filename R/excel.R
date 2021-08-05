## excel
##
## Functions for writing to Excel easily using the Acuitas style guidelines.



#' write_to_workbook
#'
#' Writes data to a workbook.
#'
#' @param data The data we are going to drop into Excel.
#' @param stratification Which stratification are we doing this time?
#' @export
write_to_workbook <- function(data = NULL, stratification = NA) {
    stopifnot(exprs = {
        !is.null(data)
    })
    config <- config::get()

    ## ---- VARS ----
    file_end <- paste0(" - ", lubridate::today(), ".xlsx")

    if (is.na(stratification)) {
        message("Exporting to Excel.")
        report_name <- paste0(config$report_name, file_end)
        ## report_name <- config$report_name
        success_message <- "Success! Report written.\n"
        failure_message <- "Error saving to Excel.\n"
    } else {
        message(paste0("Exporting `", stratification, "` to Excel."))
        report_name <- paste0(config$report_name, " - ", stratification, file_end)
        # report_name <- paste0(config$report_name, " - ", stratification)
        success_message <-
            paste0("Success! `", stratification, "` written.\n")
        failure_message <-
            paste0("Error saving `", stratification, "` to Excel.\n")
    }

    wb <- openxlsx::createWorkbook(
        creator = config$report_author,
        title = report_name,
        subject = config$report_description
    )


    ## ---- READ ME WORKSHEET ----
    read_me <- tibble::tibble(
        "Report Details" = c(
            "Report Name:",
            "Run Date:",
            "Author:",
            "Description:"
        ),
        "Description" = c(
            stringr::str_replace(report_name, ".xlsx", ""),
            format(lubridate::today(), format = "%B %d, %Y"),
            config$report_author,
            config$report_description
        )
    )
    sheet <- "Read Me"
    openxlsx::addWorksheet(wb, sheet)
    openxlsx::activeSheet(wb) <- sheet
    write_to_worksheet(
        wb,
        read_me,
        first_col_bold = TRUE,
        freeze_header = FALSE
    )

    ## ---- ANALYSIS WORKSHEET ----
    ## YOU WILL NEED TO FILL THIS IN!
    ## dplyr transformations go here!
    ## sheet <- "Analysis"
    ## openxlsx::addWorksheet(wb, sheet)
    ## openxlsx::activeSheet(wb) <- sheet
    ## write_to_worksheet(
    ##    wb,
    ##    ???,
    ##    first_col_bold = FALSE,
    ##    freeze_header = FALSE
    ## )

    ## ---- DATA WORKSHEET ----
    sheet <- "Data"
    openxlsx::addWorksheet(wb, sheet)
    openxlsx::activeSheet(wb) <- sheet
    write_to_worksheet(
        wb,
        data,
        first_col_bold = FALSE,
        freeze_header = TRUE
    )

    ## ---- SAVE ----
    openxlsx::activeSheet(wb) <- 1
    openxlsx::saveWorkbook(wb, file = report_name, overwrite = TRUE)

    excel_file_size <- file.size(report_name) / 1000000
    if (excel_file_size > 25) {
        ifelse(
            config$file_size == "strict",
            stop("File size is too large: ", excel_file_size, " MB."),
            warning("File size is too large: ", excel_file_size, " MB.")
        )
    }
    ## Tell the user what we have done.
    if (file.exists(report_name)) {
        message(success_message)
    } else {
        stop(failure_message)
    }
    return(report_name)
}



#' write_to_worksheet
#'
#' Writes data to a worksheet.
#'
#' @param wb The workbook
#' @param data The data we want to export.
#' @param start_row The first row of the data frame.
#' @param start_col The first column of the data frame.
#' @param col_widths Either "auto" or the widths you want for your columns.
#' Defaults to auto.
#' @param freeze_header TRUE/FALSE - do you want to filter on the first row?
#' Defaults to TRUE.
#' @param filter TRUE/FALSE - do you want to add filters? Defaults to TRUE.
#' @param first_col_bold TRUE/FALSE - do you want to make the first column BOLD?
#' Defaults to TRUE.
#' @param wrap_text TRUE/FALSE - do you want to wrap the text? Defaults to FALSE.
#' @export
write_to_worksheet <- function(wb, data, start_row = 1, start_col = 1, col_widths = "auto", freeze_header = TRUE, filter = TRUE, first_col_bold = FALSE, wrap_text = FALSE) {

    ## VARS --------------------------------------------------------------------
    data_cols <- ncol(data)
    last_col <- start_col + data_cols
    data_rows <- nrow(data)
    last_row <- start_row + data_rows + 1

    ## VALIDATION --------------------------------------------------------------
    if (!(col_widths == "auto" | is.numeric(col_widths))) {
        stop("col_widths must be automatic or a vector or widths")
    }
    if (is.numeric(col_widths)) {
        if (!length(col_widths) == data_cols) {
            stop("The length of col_width ,must equal the width of the data.")
        }
    }

    ## WRITE DATA --------------------------------------------------------------
    ## writeDataTable(
    ##    wb = wb,
    ##    sheet = openxlsx::activeSheet(wb),
    ##    x = data,
    ##    startCol = start_col,
    ##    startRow = start_row,
    ##    firstColumn = first_col_bold,
    ##    headerStyle = header,
    ##    withFilter = filter
    ## )
    openxlsx::writeData(
        wb = wb,
        sheet = openxlsx::activeSheet(wb),
        x = data,
        startCol = start_col,
        startRow = start_row,
        headerStyle = header,
        withFilter = filter
    )

    ## Conditional Data Features
    if (freeze_header & start_row == 1) {
        openxlsx::freezePane(
            wb = wb,
            sheet = openxlsx::activeSheet(wb),
            firstActiveRow = start_row + 1
        )
    } else if (freeze_header & start_row == 1) {
        warning("write_data parameter freeze_header ignored because start_row must be 1 to apply this feature.")
    }
    if (wrap_text) {
        openxlsx::addStyle(
            wb = wb,
            sheet = openxlsx::activeSheet(wb),
            style = font_wrap,
            rows = start_row:last_row,
            cols = start_col:last_col,
            gridExpand = TRUE,
            stack = TRUE
        )
    }
    openxlsx::setColWidths(wb,
        sheet = openxlsx::activeSheet(wb),
        cols = start_col:last_col,
        widths = col_widths
    )
}



## Styles ----------------------------------------------------------------------
## These are not functions.
## But they are objects needed for the functions to work.
header <-
    openxlsx::createStyle(
        textDecoration = "bold",
        fgFill = "#594779",
        fontColour = "white",
        halign = "center",
        valign = "center",
        border = "bottom",
        borderColour = "#309F4C",
        borderStyle = "thick"
    )

## Content Attributes
## A set of styles stratification control the alignment of data inside the cell.
## The style names should be self-explanatory.
font_bold <- openxlsx::createStyle(textDecoration = "bold")
font_italic <- openxlsx::createStyle(textDecoration = "italic")
font_wrap <- openxlsx::createStyle(
    halign = "left",
    valign = "center",
    wrapText = TRUE
)

align_center <- openxlsx::createStyle(halign = "center")
align_left <- openxlsx::createStyle(halign = "center")
align_right <- openxlsx::createStyle(halign = "right")

wrap_text <-
    openxlsx::createStyle(
        halign = "left",
        valign = "center",
        wrapText = TRUE
    )

## Highlights - lots of basics here.
## Reference: https://www.color-hex.com/color/ff7f7f
highlight_blue <-
    openxlsx::createStyle(
        fontColour = "#000000",
        fgFill = "#7fffff",
        border = c("bottom"),
        borderColour = "#309F4C"
    )
highlight_green <-
    openxlsx::createStyle(
        fontColour = "#000000",
        fgFill = "#7fff7f",
        border = c("bottom"),
        borderColour = "#309F4C"
    )
highlight_orange <-
    openxlsx::createStyle(
        fontColour = "#000000",
        fgFill = "#ffbf7f",
        border = c("bottom"),
        borderColour = "#309F4C"
    )
highlight_purple <-
    openxlsx::createStyle(
        fontColour = "#000000",
        fgFill = "#7f7fff",
        border = c("bottom"),
        borderColour = "#309F4C"
    )
highlight_red <-
    openxlsx::createStyle(
        fontColour = "#000000",
        fgFill = "#ff7f7f",
        border = c("bottom"),
        borderColour = "#309F4C"
    )


## Content Types -----------------------------------------------------------
## Defines the kind of data in the cells AND other cell attributes you will
## probably want because of that.
content_num <-
    openxlsx::createStyle(
        halign = "right",
        numFmt = "NUMBER"
    )
content_perc <-
    openxlsx::createStyle(
        halign = "right",
        numFmt = "0.0%"
    )
content_currency <-
    openxlsx::createStyle(
        halign = "right",
        numFmt = "CURRENCY"
    )
content_big_num <-
    openxlsx::createStyle(
        halign = "right",
        numFmt = "#,##0"
    )