#' Load real MS Forms data
#'
#' Load data in \code{\link{xmlGenerator}}
#'
#' @param spreadsheet_path a character string filepath pointing to the spreadsheet (xlsx, xls, csv) of MS Forms data.
#'
#' @return \code{"forms_spreadsheet"}: a data frame containing user-provided MS Forms data plus one column that \code{"load_example_data()"} left-joined to \code{"example_data"}: \code{"forms_spreadsheet$value"} that contains key values for reference-type lookups called in \code{"build_xml"}.
#' @return \code{"record_list"}: a list of \code{"forms_spreadsheet"} data parsed into fields for xml.
#'
#' @examples
#' xmlGenerator::load_data()
#'
#' @export

load_data <- function(spreadsheet_path){
    tryCatch(
        expr = {
            #----- external packages
            suppressMessages(suppressWarnings(library(xml2)))
            suppressMessages(suppressWarnings(library(dplyr)))
            suppressMessages(suppressWarnings(library(data.table)))
            suppressMessages(suppressWarnings(library(stringr)))
            #----- load static asset
            # example_forms_spreadsheet <- example_data # read example (new_spec)
            load("data/example_data.rda")

            #----- validate parts of `forms_spreadsheet`
            forms_spreadsheet <- validate_spreadsheet(spreadsheet_path)
            validate_ncol(forms_spreadsheet, example_data)
            validate_colnames(forms_spreadsheet, example_data)
            ref_type_lookup <- xmlGenerator::make_ref_type_lookup()
            forms_spreadsheet <- validate_reftypes(forms_spreadsheet, ref_type_lookup)
            record_list <- validateAuthors(forms_spreadsheet, ref_type_lookup, example_data)

            #----- if validations passed:
            assign("forms_spreadsheet", forms_spreadsheet, envir = globalenv())
            assign("record_list", record_list, envir = globalenv())
            message("`forms_spreadsheet` parsed to `record_list`...\nOutput available as `record_list` in global envrionment...\n")
            message("Run xmlGenerator::build_xml(record_list) to build xml!\n\n") # message indicating the function job completed
        }
    )
}

