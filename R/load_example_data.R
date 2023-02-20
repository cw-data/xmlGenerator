#' Load example data
#'
#' Load example data in \code{\link{xmlGenerator}}
#'
#' @param none
#'
#' @return \code{"example_data"}: a data frame containing the column structure that user-provided data must match. A user's MS Forms spreadsheet must have exactly the same column order and names as \code{"example_data"} to generate valid xml.
#' @return \code{"forms_spreadsheet"}: a data frame containing user-provided MS Forms data plus one column that \code{"load_example_data()"} left-joined to \code{"example_data"}: \code{"forms_spreadsheet$value"} that contains key values for reference-type lookups called in \code{"build_xml"}.
#' @return \code{"record_list"}: a list of \code{"forms_spreadsheet"} data parsed into fields for xml.
#'
#' @examples
#' xmlGenerator::load_example_data()
#'
#' @export

load_example_data <- function() {
    tryCatch(
        expr = {
            message("Example data loaded to global environment and converted to `forms_spreadsheet`")
            assign("EXAMPLE_DATA", "my_directory/my_data.xlsx", envir = globalenv())
            assign("example_data", example_data, envir = globalenv())

            validate_ncol(example_data, example_data)
            validate_colnames(forms_spreadsheet, example_data)
            ref_type_lookup <- xmlGenerator::make_ref_type_lookup()
            forms_spreadsheet <- validate_reftypes(forms_spreadsheet, ref_type_lookup)
            record_list <- validateAuthors(forms_spreadsheet, ref_type_lookup, example_data)

            assign("forms_spreadsheet", forms_spreadsheet, envir = globalenv())
            assign("record_list", record_list, envir = globalenv())
            message("`forms_spreadsheet` parsed to `record_list`...\nOutput available as `record_list` in global envrionment...\n")
            message("Run xmlGenerator::build_xml(record_list) to build xml!\n\n") # message indicating the function job completed
        }
    )
}
